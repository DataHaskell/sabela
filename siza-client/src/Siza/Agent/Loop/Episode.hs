{-# LANGUAGE OverloadedStrings #-}

{- | The state one episode carries between turns, and the operations that read
or advance it: printing the transcript, ending the run, and the per-turn wrap-up
the budget injects.
-}
module Siza.Agent.Loop.Episode (
    Episode (..),
    newEpisode,
    flush,
    finish,
    noteUnconfirmed,
    preTurn,
    doneSignalProbe,
    repairReds,
    reDiscover,
    stopTagFor,
    silentTurn,
    saveEx,
) where

import Data.Aeson (Value, object, (.=))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.CellResult (CellId)
import Sabela.AI.Types (ToolOutcome)
import Sabela.LLM.Ollama.Client (ToolCall (..), Turn (..))
import Siza.Agent.Check (CheckResult (..))
import Siza.Agent.Discover (seamDiscover)
import Siza.Agent.Discover.HistoryGuard (
    closeSearchLedgerRanked,
    setSearchPressure,
 )
import Siza.Agent.EmitLedger (EmitLedger, dedupInjected)
import Siza.Agent.Exemplars (saveVerified)
import Siza.Agent.Loop.Types (
    AgentRun (..),
    Driver (..),
    EpisodeBudget (..),
 )
import Siza.Agent.Loop.WrapUp (
    budgetView,
    countUnconfirmed,
    missRungFloor,
    wrapUpFinalUnconfirmed,
    wrapUpOnce,
 )
import Siza.Agent.Messages (doneSignalMsg, noCheckSignalMsg, toolMsg)
import Siza.Agent.Owned (
    OwnedCell (..),
    bestFailing,
    hasArtifact,
    recordOwned,
 )
import Siza.Agent.Repair (repairRedCells)
import Siza.Agent.Stack (StackSession (..))
import Siza.Agent.Tools (renderOutcome)
import Siza.Agent.Transcript (renderMessage)

{- | Everything the turn loop reads or mutates. The counters are separate refs
rather than one state value because the loop is re-entrant: a turn may end from
several branches and each bumps a different one.
-}
data Episode = Episode
    { epSess :: StackSession
    , epEmits :: IORef EmitLedger
    , epEmit :: Text -> IO ()
    , epBudget :: EpisodeBudget
    , epDriver :: Driver
    , epPrompt :: Text
    , epMaxTurns :: Int
    , epPrinted :: IORef Int
    , epDelivered :: IORef Bool
    , epSignalled :: IORef Bool
    , epChatRetries :: IORef Int
    , epStuck :: IORef Int
    , epReenterStuck :: IORef Int
    , epKernelDown :: IORef Int
    , epSeenRedSigs :: IORef (Set [(CellId, Text)])
    , epStreaks :: IORef (Map CellId (Text, Int))
    , epWrapped :: IORef Bool
    , epLastDitch :: IORef Bool
    , epUnconfirmed :: IORef Int
    }

newEpisode ::
    StackSession ->
    IORef EmitLedger ->
    (Text -> IO ()) ->
    EpisodeBudget ->
    Driver ->
    Text ->
    Int ->
    IO Episode
newEpisode sess emits emit budget driver prompt maxTurns =
    Episode sess emits emit budget driver prompt maxTurns
        <$> newIORef 0
        <*> newIORef False
        <*> newIORef False
        <*> newIORef 0
        <*> newIORef 0
        <*> newIORef 0
        <*> newIORef 0
        <*> newIORef Set.empty
        <*> newIORef Map.empty
        <*> newIORef False
        <*> newIORef False
        <*> newIORef 0

stopTagFor :: CheckResult -> Text
stopTagFor CheckPassed = "done"
stopTagFor _ = "done_unverified"

-- | An assistant turn that called nothing and said nothing is not a summary.
silentTurn :: Turn -> Bool
silentTurn t = null (turnCalls t) && T.null (T.strip (turnContent t))

-- | Print every transcript message the caller has not already seen.
flush :: Episode -> [Value] -> IO ()
flush ep msgs = do
    n <- readIORef (epPrinted ep)
    mapM_
        (\(i, m) -> epEmit ep (renderMessage i m <> "\n"))
        (zip [n + 1 ..] (drop n msgs))
    writeIORef (epPrinted ep) (length msgs)

{- | End the run. A give-up that repair could still answer gets one last
automatic pass over the red cells, audited into the transcript.
-}
finish ::
    Episode ->
    Map CellId OwnedCell ->
    Int ->
    Int ->
    Text ->
    Text ->
    [Value] ->
    IO AgentRun
finish ep owned turn nCalls final stopped msgs
    | stopped `elem` repairableGiveUpReasons = do
        already <- readIORef (epLastDitch ep)
        (owned', fixes) <-
            if already
                then pure (owned, [])
                else do
                    writeIORef (epLastDitch ep) True
                    fixes <-
                        repairRedCells (drvDispatch (epDriver ep)) $
                            [ (c, ocDiagnostic oc)
                            | (c, oc) <- Map.toList owned
                            , not (ocHealthy oc)
                            ]
                    pure (foldr recordOwned owned fixes, fixes)
        let repairMsgs = concatMap auditedRepairMessages fixes
            msgs' = msgs ++ repairMsgs
        line <- finalLine ep stopped owned' (bestFailing owned')
        flush ep msgs'
            >> pure (AgentRun turn (nCalls + length fixes) line stopped msgs')
    | otherwise = do
        line <- finalLine ep stopped owned final
        flush ep msgs >> pure (AgentRun turn nCalls line stopped msgs)

repairableGiveUpReasons :: [Text]
repairableGiveUpReasons = ["stuck", "stuck_reenter"]

finalLine :: Episode -> Text -> Map CellId OwnedCell -> Text -> IO Text
finalLine ep stopped owned candidate = do
    n <- readIORef (epUnconfirmed ep)
    pure (wrapUpFinalUnconfirmed n stopped owned candidate)

auditedRepairMessages :: (ToolCall, Either Text ToolOutcome) -> [Value]
auditedRepairMessages (tc, out) =
    [ object
        [ "role" .= ("assistant" :: Text)
        , "content" .= ("Automatic final repair attempt." :: Text)
        , "tool_calls" .= [object ["function" .= callFunction]]
        ]
    , toolMsg tc (renderOutcome out)
    ]
  where
    callFunction = object ["name" .= tcName tc, "arguments" .= tcArgs tc]

noteUnconfirmed :: Episode -> [(ToolCall, Either Text ToolOutcome)] -> IO ()
noteUnconfirmed ep steps =
    modifyIORef' (epUnconfirmed ep) (+ countUnconfirmed steps)

{- | Raise search pressure for the turns remaining, then inject the one-shot
wrap-up if the budget says this is the moment for it.
-}
preTurn :: Episode -> Double -> Int -> Int -> Map CellId OwnedCell -> IO [Value]
preTurn ep elapsed turn repairs owned = do
    setSearchPressure ledger (missRungFloor maxTurns (maxTurns - turn))
    wrap <-
        wrapUpOnce (epWrapped ep) (rankedFacts ep owned) $
            budgetView
                maxTurns
                turn
                (ebMaxRepairs (epBudget ep))
                repairs
                elapsed
                (ebDeadlineSecs (epBudget ep))
    dedupInjected (epEmits ep) turn wrap
  where
    ledger = ssLedger (epSess ep)
    maxTurns = epMaxTurns ep

rankedFacts :: Episode -> Map CellId OwnedCell -> IO [Text]
rankedFacts ep owned =
    closeSearchLedgerRanked
        (epPrompt ep)
        (map ocSource (Map.elems owned))
        (ssLedger (epSess ep))

{- | The signal is adjacent to the write it is about: it fires on the turn
whose own steps landed an artifact, so it can never read as a verdict on the
rejection that happened to follow.
-}
doneSignalProbe :: Episode -> Bool -> Map CellId OwnedCell -> IO [Value]
doneSignalProbe ep landedNow owned
    | not landedNow || not (hasArtifact owned) = pure []
    | otherwise = do
        already <- readIORef (epSignalled ep)
        if already
            then pure []
            else do
                writeIORef (epSignalled ep) True
                (r, mDetail) <- drvVerify (epDriver ep) owned
                pure $ case (r, mDetail) of
                    (CheckPassed, Just check) ->
                        [doneSignalMsg (Map.keys owned) check]
                    (CheckFailed, _) -> []
                    (CheckPassed, Nothing) -> []
                    _ -> [noCheckSignalMsg mDetail]

repairReds ::
    Episode -> Map CellId OwnedCell -> [CellId] -> IO (Map CellId OwnedCell)
repairReds ep owned reds = do
    fixes <-
        repairRedCells
            (drvDispatch (epDriver ep))
            [(c, ocDiagnostic oc) | c <- reds, Just oc <- [Map.lookup c owned]]
    pure (foldr recordOwned owned fixes)

reDiscover :: Episode -> Map CellId OwnedCell -> [CellId] -> IO [Value]
reDiscover ep owned reds = do
    done <- readIORef (epDelivered ep)
    if done
        then pure []
        else
            seamDiscover
                (ssGrammar (epSess ep))
                (drvDispatch (epDriver ep))
                (redCells owned reds)

redCells :: Map CellId OwnedCell -> [CellId] -> [(Text, Text)]
redCells owned reds =
    [ (ocSource oc, ocDiagnostic oc)
    | c <- reds
    , Just oc <- [Map.lookup c owned]
    , not (ocHealthy oc)
    ]

saveEx :: Episode -> Map CellId OwnedCell -> IO ()
saveEx ep owned =
    saveVerified
        (epPrompt ep)
        [ocSource oc | oc <- Map.elems owned, ocHealthy oc]
