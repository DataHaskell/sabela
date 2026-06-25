{- | The agent episode loop: a local model drives the siza tools until it stops
calling them or hits the turn cap, re-entering while any owned cell is red within
a repair-round budget @D@ ('ebMaxRepairs') and a wall-clock deadline.

Under 'GrammarOn' the discover stage ('Eval.Discover') synthesises a grammar from
live @:browse@: proactively over the notebook's imports at start, after an
install, and as the step-4 seam re-browses a cell's modules on a not-in-scope
error. The scaffold ('Eval.Scaffold') pre-commits a dataframe load; substitute-
and-verify ('Eval.Repair') repairs a red ByValue cell from GHC hole-fits.
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
    runEpisodeDebug,
    sampleVerifyOne,
    ownedCellOutcome,
    stopDecision,
    discoverModules,
) where

import Data.Aeson (Value (..), object, (.=))
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Client (Manager)
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
import Eval.Task (Task (..), Verdict (..), grade, taskTest)
import Eval.Tools (catalogue, dispatch, renderOutcome)
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
    "You are pairing on a running Sabela reactive Haskell notebook through tools. \
    \Editing or running a cell automatically re-runs everything textually downstream. \
    \To add code, call insert_cell with the FULL Haskell source (always include the \
    \requested type signature). To fix a cell, call replace_cell_source. After a write \
    \the cell runs automatically — inspect the result and, if it has an error, read the \
    \error and fix it. ghci_query checks a type against the live session; scratchpad runs \
    \an isolated snippet that CANNOT see notebook bindings. When the requested binding is \
    \defined and its cell runs without error, give a one-line final summary and stop. Do \
    \not ask the user questions.\n\n"
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
runEpisode budget mgr conn base model task maxTurns =
    runEpisodeWith budget driver task maxTurns
  where
    driver =
        Driver
            { drvChat = \msgs -> chat mgr model msgs catalogue
            , drvDispatch = dispatch conn base
            , drvNow = realToFrac <$> getPOSIXTime
            , drvVerify = (== Surfaced) . fst <$> grade conn base task
            }

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
runEpisodeTraced emit mode budget driver task maxTurns = do
    printed <- newIORef (0 :: Int)
    start <- drvNow driver
    (owned0, pre) <- runScaffold
    proactive <- proactiveDiscover mode (drvDispatch driver)
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
                Left e -> finish turn nCalls ("chat error: " <> e) "error" msgs
                Right t
                    | null (turnCalls t) -> case stopDecision (Map.map ocHealthy owned) of
                        Stop
                            | Map.null owned
                            , Just src <- salvageCell (turnContent t) -> do
                                let call = ToolCall "insert_cell" (object ["source" .= src])
                                outcome <- drvDispatch driver call
                                let owned' = recordOwned (call, outcome) owned
                                    msgs' = msgs ++ [turnRaw t, toolMsg call (renderOutcome outcome)]
                                go start' (turn + 1) (nCalls + 1) repairs owned' msgs'
                        Stop -> do
                            verified <- drvVerify driver
                            if verified
                                then finish (turn + 1) nCalls (turnContent t) "done" (msgs ++ [turnRaw t])
                                else
                                    go
                                        start'
                                        (turn + 1)
                                        nCalls
                                        (repairs + 1)
                                        owned
                                        (msgs ++ [turnRaw t, verifyMsg])
                        Reenter reds -> do
                            owned' <- repairIfValue owned reds
                            redisc <- reDiscover owned' reds
                            let still = [c | (c, oc) <- Map.toList owned', not (ocHealthy oc)]
                                msg = if null still then verifyMsg else reenterMsg still
                            go
                                start'
                                (turn + 1)
                                nCalls
                                (repairs + 1)
                                owned'
                                (msgs ++ [turnRaw t, msg] ++ redisc)
                    | otherwise -> do
                        outcomes <- mapM (drvDispatch driver) (turnCalls t)
                        discovered <- runDiscover mode (drvDispatch driver) (turnCalls t)
                        let owned' = foldr recordOwned owned (zip (turnCalls t) outcomes)
                            toolMsgs = zipWith toolMsg (turnCalls t) (map renderOutcome outcomes)
                            msgs' = msgs ++ [turnRaw t] ++ toolMsgs ++ discovered
                        go start' (turn + 1) (nCalls + length (turnCalls t)) repairs owned' msgs'
    go start 0 0 0 owned0 (initial ++ pre ++ proactive)
  where
    initial =
        [ object ["role" .= ("system" :: Text), "content" .= systemPrompt]
        , object ["role" .= ("user" :: Text), "content" .= taskPrompt task]
        ]
    runScaffold = case scaffoldCall task of
        Nothing -> pure (Map.empty, [])
        Just call -> do
            _ <- drvDispatch driver call
            pure
                ( Map.empty
                , [object ["role" .= ("user" :: Text), "content" .= scaffoldNote]]
                )
    repairIfValue owned reds
        | isJust (taskTest task) = do
            fixes <-
                repairRedCells
                    (drvDispatch driver)
                    [(c, ocDiagnostic oc) | c <- reds, Just oc <- [Map.lookup c owned]]
            pure (foldr recordOwned owned fixes)
        | otherwise = pure owned
    reDiscover owned' reds =
        seamDiscover mode (drvDispatch driver) (redCells owned' reds)
    redCells owned' reds =
        [ (ocSource oc, ocDiagnostic oc)
        | c <- reds
        , Just oc <- [Map.lookup c owned']
        , not (ocHealthy oc)
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
runEpisodeDebug emit budget mgr conn base model task maxTurns =
    runEpisodeTraced emit GrammarOn budget driver task maxTurns
  where
    driver =
        Driver
            { drvChat = \msgs -> chatSeeded True Nothing mgr model msgs catalogue
            , drvDispatch = dispatch conn base
            , drvNow = realToFrac <$> getPOSIXTime
            , drvVerify = (== Surfaced) . fst <$> grade conn base task
            }
