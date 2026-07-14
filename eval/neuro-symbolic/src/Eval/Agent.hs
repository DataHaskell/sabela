{- | The agent episode loop: a local model drives the siza tools until it stops
calling them or hits the turn cap, re-entering while any owned cell is red within
a repair-round budget @D@ ('ebMaxRepairs') and a wall-clock deadline.

Under 'GrammarOn' the discover stage ('Eval.Discover') synthesises a grammar from
live @:browse@: proactively over the notebook's imports at start, after an
install, and as the step-4 seam re-browses a cell's modules on a not-in-scope
error. The scaffold ('Eval.Scaffold') pre-commits a dataframe load; substitute-
and-verify ('Eval.Repair') repairs any red owned cell from GHC hole-fits,
keeping the first fit that compiles.
-}
module Eval.Agent (
    AgentRun (..),
    StopDecision (..),
    Driver (..),
    EpisodeBudget (..),
    GrammarMode (..),
    SampleVerify (..),
    SampleResult (..),
    defaultBudget,
    runEpisode,
    runEpisodeWith,
    runEpisodeWith',
    runEpisodeTraced,
    runEpisodeSeeded,
    runEpisodeDebug,
    sampleVerifyOne,
    ownedCellOutcome,
    stopDecision,
    discoverModules,
) where

import Data.Aeson (Value (..), object, (.=))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Client (Manager)
import Sabela.AI.Capabilities.ToolName (actsOnNotebook, parseToolName)
import Sabela.AI.Grammar (grammarPromptBlock)
import Sabela.AI.Types (ToolOutcome (..))
import Siza.Transport (Conn)

import Eval.Discover (
    GrammarMode (..),
    discoverModules,
    proactiveDiscover,
    runDiscover,
    seamDiscover,
 )
import Eval.Messages (reenterMsg, toolMsg, verifyMsg)
import Eval.Ollama (ToolCall (..), Turn (..), chat, chatSeeded)
import Eval.Owned (
    OwnedCell (..),
    StopDecision (..),
    bestFailing,
    ownedCellOutcome,
    recordOwned,
    stopDecision,
 )
import Eval.Repair (repairRedCells)
import Eval.Salvage (salvageCell)
import Eval.Sample (SampleResult (..), SampleVerify (..), sampleVerifyOne)
import Eval.Scaffold (scaffoldCall, scaffoldNote)
import Eval.Task (Task (..), Verdict (..), grade)
import Eval.Tools (
    catalogue,
    dispatch,
    renderOutcome,
 )
import Eval.Transcript (renderMessage)

data AgentRun = AgentRun
    { arTurns :: Int
    , arToolCalls :: Int
    , arFinal :: Text
    , arStopped :: Text
    , arTranscript :: [Value]
    -- ^ The episode's full message list, for 'Eval.Transcript.renderTranscript'.
    }
    deriving (Show)

{- | TODO: mchavinda - the system prompt might be moot cause
| small models forget anyway.
-}
systemPrompt :: Text
systemPrompt =
    "Pair on a live Sabela reactive Haskell notebook through tools; editing or running a \
    \cell re-runs everything downstream. insert_cell adds a cell (full source, with the \
    \requested type signature); replace_cell_source fixes one — a write auto-runs, so read \
    \the result and fix any error. Reuse what earlier cells defined (list_bindings) instead \
    \of recomputing. scratchpad runs an isolated snippet that CANNOT see notebook bindings. \
    \Once the binding is defined and its cell runs clean, give a one-line summary and stop; \
    \do not ask questions.\n\n"
        <> grammarPromptBlock

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

runEpisode ::
    EpisodeBudget -> Manager -> Conn -> Text -> Text -> Task -> Int -> IO AgentRun
runEpisode budget mgr conn base model task maxTurns = do
    cat <- catalogue
    let driver =
            Driver
                { drvChat = \msgs -> chat mgr model msgs cat
                , drvDispatch = dispatch conn base
                , drvNow = realToFrac <$> getPOSIXTime
                , drvVerify = (== Surfaced) . fst <$> grade conn base task
                }
    runEpisodeWith budget driver task maxTurns

runEpisodeWith :: EpisodeBudget -> Driver -> Task -> Int -> IO AgentRun
runEpisodeWith = runEpisodeWith' GrammarOn

{- | Run an episode under an explicit grammar mode. 'GrammarOn' synthesises the
live-browse grammar (proactively at start, after installs, and on the re-discover
seam); 'GrammarOff' is the raw-dump baseline the negative control measures.
-}
runEpisodeWith' ::
    GrammarMode -> EpisodeBudget -> Driver -> Task -> Int -> IO AgentRun
runEpisodeWith' = runEpisodeTraced (const (pure ()))

{- | The episode loop with a trace sink. 'flush' emits each newly appended
message via 'renderMessage' as the run streams, so the no-op 'runEpisodeWith'' and
the live-audit 'runEpisodeDebug' share one body. The sink sees the system prompt
first, then every input, the model's thinking, tool calls, and outcomes.
-}
runEpisodeTraced ::
    (Text -> IO ()) ->
    GrammarMode ->
    EpisodeBudget ->
    Driver ->
    Task ->
    Int ->
    IO AgentRun
runEpisodeTraced = runEpisodeSeeded []

{- | Like 'runEpisodeTraced' but seeded with a prior transcript. An empty seed is
a fresh episode (system prompt + task, scaffold, and proactive discovery). A
non-empty seed continues a conversation: the new user turn is appended and the
system prompt, scaffold, and discovery are skipped (they already live in the
seed), so an interactive session keeps its context across prompts.
-}
runEpisodeSeeded ::
    [Value] ->
    (Text -> IO ()) ->
    GrammarMode ->
    EpisodeBudget ->
    Driver ->
    Task ->
    Int ->
    IO AgentRun
runEpisodeSeeded seed emit mode budget driver task maxTurns = do
    printed <- newIORef (0 :: Int)
    consec <- newIORef (0 :: Int)
    chatRetries <- newIORef (0 :: Int)
    stuck <- newIORef (0 :: Int)
    start <- drvNow driver
    (owned0, msgs0) <-
        if null seed
            then do
                (owned, pre) <- runScaffold
                proactive <- proactiveDiscover mode (drvDispatch driver)
                pure (owned, initial ++ pre ++ proactive)
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
                                    then finish (turn + 1) nCalls (turnContent t) "done" (msgs ++ [turnRaw t])
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
                                writeIORef stuck 0
                                go
                                    start'
                                    (turn + 1)
                                    nCalls
                                    (repairs + 1)
                                    owned'
                                    (msgs ++ [turnRaw t, msg] ++ redisc)
                        else do
                            results <- mapM dispatchCall (turnCalls t)
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
    userMsg = object ["role" .= ("user" :: Text), "content" .= taskPrompt task]
    runScaffold = case scaffoldCall task of
        Nothing -> pure (Map.empty, [])
        Just call -> do
            _ <- drvDispatch driver call
            pure
                ( Map.empty
                , [object ["role" .= ("user" :: Text), "content" .= scaffoldNote]]
                )
    -- Code tools read their source straight from the (recovered) tool-call args;
    -- the recovery layer in 'Eval.Ollama.parseTurn' has already repaired any
    -- unescaped backslash or content-channel tool-call JSON before this point.
    dispatchCall call = do
        outcome <- drvDispatch driver call
        pure (call, Right outcome)
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

{- | N4: consecutive read-only/discovery calls tolerated before the loop nudges
the model to act — catches the "burned the budget searching" pathology
without bothering a normal multi-read sync.
-}
readDiscoveryBudget :: Int
readDiscoveryBudget = 8

-- | N10: how many times a chat error is retried before the turn gives up.
maxChatRetries :: Int
maxChatRetries = 2

{- | Consecutive "declared done, but the check fails and nothing changed" turns
tolerated before the loop gives up — the degenerate verify spin where the model
recognises it is stuck yet the gate keeps re-prompting. Any acting turn (tool
call, repair, salvage-insert) resets the counter, so a model still trying is
bounded only by the repair budget and deadline, not by this.
-}
maxStuckVerifies :: Int
maxStuckVerifies = 3

{- | The give-up summary when 'maxStuckVerifies' trips: honest about the cause and
library/task-agnostic — the covering check is wrong, or it tests an effect a pure
expression cannot observe.
-}
stuckFinal :: Text
stuckFinal =
    "Gave up: the deliverable's check kept failing and the last few turns changed \
    \nothing. The check may be testing the wrong value, or an effect that a pure \
    \expression cannot observe (such as an IO action's result)."

-- | Whether a tool call acts on the notebook (so it resets the discovery budget).
callActs :: ToolCall -> Bool
callActs = maybe False actsOnNotebook . parseToolName . tcName

{- | Track the consecutive-discovery counter across a turn's tool calls: an
acting call resets it, otherwise it grows by the call count. When it crosses
'readDiscoveryBudget' the counter resets and a one-shot forcing message is
returned to inject into the conversation. Library- and task-agnostic.
-}
updateBudget :: IORef Int -> [ToolCall] -> IO [Value]
updateBudget ref calls
    | any callActs calls = writeIORef ref 0 >> pure []
    | otherwise = do
        c0 <- readIORef ref
        let c = c0 + length calls
        if c >= readDiscoveryBudget
            then writeIORef ref 0 >> pure [forceActMsg]
            else writeIORef ref c >> pure []

-- | The just-in-time, library-agnostic nudge injected when discovery runs long.
forceActMsg :: Value
forceActMsg =
    object
        [ "role" .= ("user" :: Text)
        , "content"
            .= ( "You have made several discovery/read calls in a row without writing to the \
                 \notebook. If you have enough to proceed, act now — add or edit a cell \
                 \(insert_cell / replace_cell_source). If a value, function, or package you \
                 \need does not exist, say so rather than searching further." ::
                    Text
               )
        ]

{- | Like 'runEpisode' but streams the whole conversation to @emit@ as it happens
and turns on the model's reasoning channel, for the single-run debug path. The
benchmark path stays silent and thinking-free.
-}
runEpisodeDebug ::
    (Text -> IO ()) ->
    EpisodeBudget ->
    Manager ->
    Conn ->
    Text ->
    Text ->
    Task ->
    Int ->
    IO AgentRun
runEpisodeDebug emit budget mgr conn base model task maxTurns = do
    cat <- catalogue
    let driver =
            Driver
                { drvChat = \msgs -> chatSeeded True Nothing mgr model msgs cat
                , drvDispatch = dispatch conn base
                , drvNow = realToFrac <$> getPOSIXTime
                , drvVerify = (== Surfaced) . fst <$> grade conn base task
                }
    runEpisodeTraced emit GrammarOn budget driver task maxTurns
