{- | The agent episode loop: a local model drives the siza tools until it stops
calling them or hits the turn cap, re-entering while any owned cell is red within
a repair-round budget @D@ ('ebMaxRepairs') and a wall-clock deadline.

Under 'GrammarOn' the discover stage ('Siza.Agent.Discover') synthesises a grammar
from live @:browse@: proactively over the notebook's imports at start, after an
install, and as the step-4 seam re-browses a cell's modules on a not-in-scope
error. The scaffold ('Siza.Agent.Scaffold') pre-commits a dataframe load;
substitute-and-verify ('Siza.Agent.Repair') repairs any red owned cell from GHC
hole-fits, keeping the first fit that compiles.

The loop takes the request as plain 'Text' and injects verification through the
'Driver'; graders and the benchmark 'Task' stay in the eval harness that wraps it.
-}
module Siza.Agent.Loop (
    AgentRun (..),
    StopDecision (..),
    Driver (..),
    EpisodeBudget (..),
    GrammarMode (..),
    SampleVerify (..),
    SampleResult (..),
    defaultBudget,
    runEpisodeWith,
    runEpisodeWith',
    runEpisodeTraced,
    runEpisodeSeeded,
    sampleVerifyOne,
    ownedCellOutcome,
    stopDecision,
    discoverModules,
    systemPrompt,
    sampleK,
    writeSource,
    qualifiedBaseNames,
) where

import Control.Monad (void, when)
import Data.Aeson (Value (..), object, (.=))
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.PromptCore (sharedPromptCore)
import Sabela.AI.Types (ToolOutcome (..))

import Sabela.AI.Salvage (salvageCell)
import Sabela.LLM.Ollama.Client (ToolCall (..), Turn (..))
import Siza.Agent.Discover (
    GrammarMode (..),
    discoverModules,
    proactiveDiscover,
    runDiscover,
    seamDiscover,
 )
import Siza.Agent.Exemplars (
    Exemplar (..),
    exemplarMessage,
    exemplarStorePath,
    loadExemplars,
    retrieveExemplars,
    saveExemplar,
 )
import Siza.Agent.Loop.Support (
    callActs,
    groundingMsgs,
    maxChatRetries,
    maxStuckVerifies,
    qualifiedBaseNames,
    replaceCall,
    sampleK,
    stuckFinal,
    updateBudget,
    writeSource,
 )
import Siza.Agent.Messages (reenterMsg, toolMsg, verifyMsg)
import Siza.Agent.Owned (
    OwnedCell (..),
    StopDecision (..),
    bestFailing,
    noProgressStep,
    ownedCellOutcome,
    recordOwned,
    redSignature,
    stopDecision,
 )
import Siza.Agent.Repair (repairRedCells)
import Siza.Agent.Sample (SampleResult (..), SampleVerify (..), sampleVerifyOne)
import Siza.Agent.Scaffold (scaffoldCall, scaffoldNote)
import Siza.Agent.Tools (renderOutcome)
import Siza.Agent.Transcript (renderMessage)

data AgentRun = AgentRun
    { arTurns :: Int
    , arToolCalls :: Int
    , arFinal :: Text
    , arStopped :: Text
    , arTranscript :: [Value]
    -- ^ The episode's full message list, for 'Siza.Agent.Transcript.renderTranscript'.
    }
    deriving (Show)

{- | TODO: mchavinda - the system prompt might be moot cause
| small models forget anyway.
-}
systemPrompt :: Text
systemPrompt =
    "Pair on a live Sabela reactive Haskell notebook through tools; editing or \
    \running a cell re-runs everything downstream. insert_cell adds a cell (full \
    \source, with the requested type signature); replace_cell_source fixes one. \
    \Once the deliverable's cell runs clean, give a one-line summary and stop; do \
    \not ask questions.\n\n"
        <> sharedPromptCore

{- | The episode's side effects, injectable so the loop is testable without a
live model or server. Production wires Ollama @chat@ and the siza @dispatch@.
-}
data Driver = Driver
    { drvChat :: [Value] -> IO (Either Text Turn)
    , drvDispatch :: ToolCall -> IO (Either Text ToolOutcome)
    , drvNow :: IO Double
    -- ^ Wall-clock reading in seconds, injectable so the deadline is testable.
    , drvVerify :: IO Bool
    {- ^ Does the deliverable pass its covering test? The sound accept the Stop
    routes through, so "done" means the test greens, not merely that cells run.
    -}
    }

{- | What an episode is allowed to spend before it gives up. @D@ caps how many
times the health gate may re-enter the loop; the deadline caps total time.
-}
data EpisodeBudget = EpisodeBudget
    { ebMaxRepairs :: Int
    , ebDeadlineSecs :: Double
    }
    deriving (Show)

-- | The weak-model default: a few repair rounds inside a generous deadline.
defaultBudget :: EpisodeBudget
defaultBudget = EpisodeBudget{ebMaxRepairs = 4, ebDeadlineSecs = 600}

runEpisodeWith :: EpisodeBudget -> Driver -> Text -> Int -> IO AgentRun
runEpisodeWith = runEpisodeWith' GrammarOn

{- | Run an episode under an explicit grammar mode. 'GrammarOn' synthesises the
live-browse grammar (proactively at start, after installs, and on the re-discover
seam); 'GrammarOff' is the raw-dump baseline the negative control measures.
-}
runEpisodeWith' ::
    GrammarMode -> EpisodeBudget -> Driver -> Text -> Int -> IO AgentRun
runEpisodeWith' = runEpisodeTraced (const (pure ()))

{- | The episode loop with a trace sink: 'flush' streams each newly appended
message via 'renderMessage', so 'runEpisodeWith'' and the live-audit debug path
share one body. The sink sees the prompt, inputs, thinking, calls, and outcomes.
-}
runEpisodeTraced ::
    (Text -> IO ()) ->
    GrammarMode ->
    EpisodeBudget ->
    Driver ->
    Text ->
    Int ->
    IO AgentRun
runEpisodeTraced = runEpisodeSeeded []

{- | Like 'runEpisodeTraced' but seeded with a prior transcript: an empty seed
starts fresh (prompt, scaffold, discovery), a non-empty one appends the new user
turn and skips them — they already live in the seed — keeping session context.
-}
runEpisodeSeeded ::
    [Value] ->
    (Text -> IO ()) ->
    GrammarMode ->
    EpisodeBudget ->
    Driver ->
    Text ->
    Int ->
    IO AgentRun
runEpisodeSeeded seed emit mode budget driver prompt maxTurns = do
    printed <- newIORef (0 :: Int)
    consec <- newIORef (0 :: Int)
    chatRetries <- newIORef (0 :: Int)
    stuck <- newIORef (0 :: Int)
    reenterStuck <- newIORef (0 :: Int)
    seenRedSigs <- newIORef Set.empty
    start <- drvNow driver
    (owned0, msgs0) <-
        if null seed
            then do
                exemplars <- retrieveEx
                (owned, pre) <- runScaffold
                proactive <- proactiveDiscover mode (drvDispatch driver)
                pure (owned, initial ++ exemplars ++ pre ++ proactive)
            else pure (Map.empty, seed ++ [userMsg])
    let flush msgs = do
            n <- readIORef printed
            mapM_
                (\(i, m) -> emit (renderMessage i m <> "\n"))
                (zip [n + 1 ..] (drop n msgs))
            writeIORef printed (length msgs)
        finish turn nCalls final stopped msgs =
            flush msgs >> pure (AgentRun turn nCalls final stopped msgs)
        go start' turn nCalls repairs owned msgs = do
            flush msgs
            if turn >= maxTurns
                then finish turn nCalls (bestFailing owned) "max_turns" msgs
                else
                    if repairs >= ebMaxRepairs budget
                        then finish turn nCalls (bestFailing owned) "repair_budget" msgs
                        else do
                            now <- drvNow driver
                            if now - start' >= ebDeadlineSecs budget
                                then finish turn nCalls (bestFailing owned) "deadline" msgs
                                else step start' turn nCalls repairs owned msgs
        step start' turn nCalls repairs owned msgs = do
            res <- drvChat driver msgs
            case res of
                Left e -> do
                    -- N10: a chat error (e.g. malformed tool-args JSON) is a
                    -- bounded retry of the same turn, not a silent terminal
                    -- failure the next turn inherits as if progress was made.
                    r <- readIORef chatRetries
                    if r < maxChatRetries
                        then do
                            writeIORef chatRetries (r + 1)
                            step start' turn nCalls repairs owned msgs
                        else do
                            writeIORef chatRetries 0
                            finish turn nCalls ("chat error after retries: " <> e) "error" msgs
                Right t -> do
                    writeIORef chatRetries 0
                    if null (turnCalls t)
                        then case stopDecision (Map.map ocHealthy owned) of
                            Stop
                                | Map.null owned
                                , Just src <- salvageCell (turnContent t) -> do
                                    let call = ToolCall "insert_cell" (object ["source" .= src])
                                    outcome <- drvDispatch driver call
                                    let owned' = recordOwned (call, outcome) owned
                                        msgs' = msgs ++ [turnRaw t, toolMsg call (renderOutcome outcome)]
                                    writeIORef stuck 0
                                    go start' (turn + 1) (nCalls + 1) repairs owned' msgs'
                            Stop -> do
                                verified <- drvVerify driver
                                if verified
                                    then do
                                        saveEx owned
                                        finish (turn + 1) nCalls (turnContent t) "done" (msgs ++ [turnRaw t])
                                    else do
                                        -- No-progress guard: model declared done, the check
                                        -- failed, nothing changed. A few in a row means a bad or
                                        -- uncheckable check, so stop rather than spin; acting resets it.
                                        s <- readIORef stuck
                                        if s + 1 >= maxStuckVerifies
                                            then finish (turn + 1) nCalls stuckFinal "stuck" (msgs ++ [turnRaw t])
                                            else do
                                                writeIORef stuck (s + 1)
                                                go
                                                    start'
                                                    (turn + 1)
                                                    nCalls
                                                    (repairs + 1)
                                                    owned
                                                    (msgs ++ [turnRaw t, verifyMsg])
                            Reenter reds -> do
                                owned' <- repairReds owned reds
                                redisc <- reDiscover owned' reds
                                let still = [c | (c, oc) <- Map.toList owned', not (ocHealthy oc)]
                                    msg = if null still then verifyMsg else reenterMsg still
                                    sig = redSignature still owned'
                                    msgs' = msgs ++ [turnRaw t, msg] ++ redisc
                                writeIORef stuck 0
                                rs <- readIORef reenterStuck
                                seen <- readIORef seenRedSigs
                                -- Reenter no-progress guard: a still-red signature
                                -- seen before (stuck OR A/B/A/B oscillation) counts;
                                -- maxStuckVerifies repeats stops the spin honestly.
                                let (seen', repeated) = noProgressStep seen sig
                                if not (null still) && repeated
                                    then
                                        if rs + 1 >= maxStuckVerifies
                                            then
                                                finish
                                                    (turn + 1)
                                                    nCalls
                                                    (bestFailing owned')
                                                    "stuck_reenter"
                                                    msgs'
                                            else do
                                                writeIORef reenterStuck (rs + 1)
                                                writeIORef seenRedSigs seen'
                                                go start' (turn + 1) nCalls (repairs + 1) owned' msgs'
                                    else do
                                        writeIORef reenterStuck 0
                                        writeIORef
                                            seenRedSigs
                                            (if null still then seen else seen')
                                        go start' (turn + 1) nCalls (repairs + 1) owned' msgs'
                        else do
                            results <- mapM (dispatchCall msgs) (turnCalls t)
                            let dispatched = [c | (c, Right _) <- results]
                            discovered <- runDiscover mode (drvDispatch driver) dispatched
                            -- N4: nudge the model to act after too many
                            -- consecutive read-only/discovery calls.
                            nudge <- updateBudget consec dispatched
                            let owned' =
                                    foldr recordOwned owned [(c, o) | (c, Right o) <- results]
                                toolMsgs =
                                    [ toolMsg c (either id renderOutcome o)
                                    | (c, o) <- results
                                    ]
                                msgs' = msgs ++ [turnRaw t] ++ toolMsgs ++ discovered ++ nudge
                            writeIORef stuck 0
                            go start' (turn + 1) (nCalls + length dispatched) repairs owned' msgs'
    go start 0 0 0 owned0 msgs0
  where
    initial =
        [ object ["role" .= ("system" :: Text), "content" .= systemPrompt]
        , userMsg
        ]
    userMsg = object ["role" .= ("user" :: Text), "content" .= prompt]
    runScaffold = case scaffoldCall prompt of
        Nothing -> pure (Map.empty, [])
        Just call -> do
            _ <- drvDispatch driver call
            pure
                ( Map.empty
                , [object ["role" .= ("user" :: Text), "content" .= scaffoldNote]]
                )
    -- Learning loop: retrieve verified solutions for a similar request as in-context
    -- exemplars (imitation), gated by SIZA_EXEMPLAR_STORE. Empty when unset.
    retrieveEx = do
        mstore <- exemplarStorePath
        case mstore of
            Nothing -> pure []
            Just fp -> do
                exs <- loadExemplars fp
                pure (exemplarMessage (retrieveExemplars 2 prompt exs))
    -- Persist the verified deliverable so future similar requests can imitate it.
    saveEx owned = do
        mstore <- exemplarStorePath
        case mstore of
            Nothing -> pure ()
            Just fp -> do
                let src =
                        T.intercalate "\n\n" [ocSource oc | oc <- Map.elems owned, ocHealthy oc]
                if T.null (T.strip src)
                    then pure ()
                    else saveExemplar fp (Exemplar prompt src)
    -- Source comes from the tool-call args already repaired by parseTurn's
    -- recovery layer. A cell write routes through rejection sampling when
    -- SIZA_SAMPLE_K > 1; every other call dispatches once.
    dispatchCall msgs call = do
        k <- sampleK
        if k > 1 && callActs call && isJust (writeSource call)
            then rejectionDispatch msgs k call
            else plainDispatch call
    plainDispatch call = do
        outcome <- drvDispatch driver call
        pure (call, Right outcome)

    -- Rejection-sample a cell write: on a red proposal, re-ask up to K-1 times and
    -- keep the first that compiles, else restore the original. Verifies in the real
    -- notebook (deps, data files) via 'Siza.Agent.Sample.sampleVerifyOne'.
    rejectionDispatch msgs k call = do
        o0 <- drvDispatch driver call
        case ownedCellOutcome call o0 of
            Just (cid, False) -> do
                -- Ground the re-ask: answer the model's guessed API with the real
                -- signatures from the live index, so re-samples are grounded, not
                -- blind re-guesses of the same types. Computed once, reused per sample.
                ground <- groundingMsgs (drvDispatch driver) (fromMaybe "" (writeSource call))
                let msgs' = msgs ++ ground
                winRef <- newIORef Nothing
                let sv =
                        SampleVerify
                            { svSample = const (fromMaybe "" <$> reAskSource msgs')
                            , svRollout = rolloutReplace winRef cid
                            , svInsert = const (pure ())
                            }
                _ <- sampleVerifyOne (k - 1) sv
                mWin <- readIORef winRef
                case mWin of
                    Just win -> pure win
                    Nothing -> restoreOriginal cid call o0
            -- Already compiled, or not a cell-write outcome: nothing to sample.
            _ -> pure (call, Right o0)
    -- Replace the cell with a candidate and keep it iff the run compiled; capture
    -- the winning (call, outcome) so the caller records the real committed write.
    rolloutReplace winRef cid src
        | T.null (T.strip src) = pure False
        | otherwise = do
            let rc = replaceCall cid src
            o <- drvDispatch driver rc
            let ok = maybe False snd (ownedCellOutcome rc o)
            when ok (writeIORef winRef (Just (rc, Right o)))
            pure ok
    restoreOriginal cid call o0 = do
        _ <-
            maybe
                (pure ())
                (void . drvDispatch driver . replaceCall cid)
                (writeSource call)
        pure (call, Right o0)
    -- One more diverse proposal for the same step; temperature (env) makes each
    -- re-ask differ. The first cell-writing call's source, if any.
    reAskSource msgs = do
        r <- drvChat driver msgs
        pure $ case r of
            Right t -> listToMaybe [s | c <- turnCalls t, callActs c, Just s <- [writeSource c]]
            Left _ -> Nothing
    repairReds owned reds = do
        fixes <-
            repairRedCells
                (drvDispatch driver)
                [(c, ocDiagnostic oc) | c <- reds, Just oc <- [Map.lookup c owned]]
        pure (foldr recordOwned owned fixes)
    reDiscover owned' reds =
        seamDiscover mode (drvDispatch driver) (redCells owned' reds)
    redCells owned' reds =
        [ (ocSource oc, ocDiagnostic oc)
        | c <- reds
        , Just oc <- [Map.lookup c owned']
        , not (ocHealthy oc)
        ]
