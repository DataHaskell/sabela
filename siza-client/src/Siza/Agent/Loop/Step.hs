{-# LANGUAGE OverloadedStrings #-}

{- | One turn of the episode. 'runTurns' checks the budgets and hands to
'step', which either dispatches the turn's tool calls or, when the model called
nothing, decides whether the episode stops, re-enters on red cells, or salvages
a cell out of the prose.
-}
module Siza.Agent.Loop.Step (
    runTurns,
) where

import Control.Monad (when)
import Data.Aeson (Value, object, (.=))
import Data.IORef (readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Sabela.AI.CellResult (CellId)
import Sabela.AI.Salvage (salvageCell)
import Sabela.LLM.Ollama.Client (ToolCall (..), Turn (..))
import Siza.Agent.Check (CheckResult (..))
import Siza.Agent.Discover (runDiscoverOutcomes)
import Siza.Agent.EmitLedger (emitTurn)
import Siza.Agent.Loop.Episode (
    Episode (..),
    doneSignalProbe,
    finish,
    flush,
    noteUnconfirmed,
    preTurn,
    reDiscover,
    repairReds,
    saveEx,
    silentTurn,
    stopTagFor,
 )
import Siza.Agent.Loop.Sampling (dispatchCall)
import Siza.Agent.Loop.Support (
    kernelFailureStep,
    maxChatRetries,
    maxStuckVerifies,
    streakHints,
    stuckFinal,
 )
import Siza.Agent.Loop.Types (AgentRun, Driver (..), EpisodeBudget (..))
import Siza.Agent.Loop.Verdict (unconfirmedDiagMsg, verdictMsg)
import Siza.Agent.Messages (reenterAlarmMsg, streakMsg, toolMsg)
import Siza.Agent.Owned (
    OwnedCell (..),
    StopDecision (..),
    bestFailing,
    hasArtifact,
    landedArtifact,
    noProgressStep,
    recordOwned,
    redSignature,
    stopDecision,
 )
import Siza.Agent.Stack (StackSession (..))
import Siza.Agent.Stack.Call (CallResult (..), notesMessage)
import Siza.Agent.Tools (renderOutcome)

type Owned = Map CellId OwnedCell

{- | What every step of the loop threads forward: the episode, the wall-clock it
started at, the turn, tool-call and repair counts, the cells it owns, and the
transcript so far.
-}
type Stepping a =
    Episode -> Double -> Int -> Int -> Int -> Owned -> [Value] -> a

{- | Drive turns until a budget runs out or a branch finishes the run. @start@
is the wall-clock the deadline is measured from.
-}
runTurns :: Stepping (IO AgentRun)
runTurns ep start turn nCalls repairs owned msgs = do
    flush ep msgs
    if turn >= epMaxTurns ep
        then stop "max_turns"
        else
            if repairs >= ebMaxRepairs (epBudget ep)
                then stop "repair_budget"
                else do
                    now <- drvNow (epDriver ep)
                    if now - start >= ebDeadlineSecs (epBudget ep)
                        then stop "deadline"
                        else do
                            wrap <- preTurn ep (now - start) turn repairs owned
                            step ep start turn nCalls repairs owned (msgs ++ wrap)
  where
    stop reason = finish ep owned turn nCalls (bestFailing owned) reason msgs

step :: Stepping (IO AgentRun)
step ep start turn nCalls repairs owned msgs = do
    res <- drvChat (epDriver ep) msgs
    case res of
        Left e -> do
            r <- readIORef (epChatRetries ep)
            if r < maxChatRetries
                then do
                    writeIORef (epChatRetries ep) (r + 1)
                    step ep start turn nCalls repairs owned msgs
                else do
                    writeIORef (epChatRetries ep) 0
                    finish
                        ep
                        owned
                        turn
                        nCalls
                        ("chat error after retries: " <> e)
                        "error"
                        msgs
        Right t -> do
            writeIORef (epChatRetries ep) 0
            if null (turnCalls t)
                then noCallTurn ep start turn nCalls repairs owned msgs t
                else calledTurn ep start turn nCalls repairs owned msgs t

{- | The model called nothing. Either it is done, it salvaged a cell out of its
own prose, or red cells send it back in.
-}
noCallTurn :: Stepping (Turn -> IO AgentRun)
noCallTurn ep start turn nCalls repairs owned msgs t =
    case stopDecision (Map.map ocHealthy owned) of
        Stop
            | Map.null owned
            , Just src <- salvageCell (turnContent t) -> do
                let call = ToolCall "insert_cell" (object ["source" .= src])
                outcome <- drvDispatch (epDriver ep) call
                noteUnconfirmed ep [(call, outcome)]
                let owned' = recordOwned (call, outcome) owned
                    salvaged = ToolCall "salvage" (tcArgs call)
                out <-
                    emitTurn
                        (epEmits ep)
                        turn
                        (turnRaw t)
                        [toolMsg salvaged (renderOutcome outcome)]
                writeIORef (epStuck ep) 0
                runTurns ep start (turn + 1) (nCalls + 1) repairs owned' (msgs ++ out)
        Stop
            | silentTurn t ->
                finish ep owned (turn + 1) nCalls "" "no_reply" (msgs ++ [turnRaw t])
        Stop -> verifyStop ep start turn nCalls repairs owned msgs t
        Reenter reds -> reenter ep start turn nCalls repairs owned msgs t reds

{- | A stop the model asked for, checked. A pass with an artifact ends the run;
anything else re-enters with the verdict, until the stuck budget runs out.
-}
verifyStop :: Stepping (Turn -> IO AgentRun)
verifyStop ep start turn nCalls repairs owned msgs t = do
    (result, mEv) <- drvVerify (epDriver ep) owned
    case result of
        CheckPassed
            | hasArtifact owned -> do
                saveEx ep owned
                stopWith CheckPassed
        CheckNotApplicable
            | hasArtifact owned -> stopWith CheckNotApplicable
        _ -> do
            s <- readIORef (epStuck ep)
            if s + 1 >= maxStuckVerifies
                then
                    finish
                        ep
                        owned
                        (turn + 1)
                        nCalls
                        stuckFinal
                        "stuck"
                        (msgs ++ [turnRaw t])
                else do
                    writeIORef (epStuck ep) (s + 1)
                    out <-
                        emitTurn
                            (epEmits ep)
                            turn
                            (turnRaw t)
                            [verdictMsg (epPrompt ep) result mEv owned]
                    runTurns
                        ep
                        start
                        (turn + 1)
                        nCalls
                        (repairs + 1)
                        owned
                        (msgs ++ out)
  where
    stopWith checked =
        finish
            ep
            owned
            (turn + 1)
            nCalls
            (turnContent t)
            (stopTagFor checked)
            (msgs ++ [turnRaw t])

{- | Red cells send the model back in. A red signature it has already seen
counts as no progress; enough of those in a row end the run.
-}
reenter :: Stepping (Turn -> [CellId] -> IO AgentRun)
reenter ep start turn nCalls repairs owned msgs t reds = do
    owned' <- repairReds ep owned reds
    redisc <- reDiscover ep owned' reds
    let stillPairs =
            [ (c, ocDiagnostic oc, ocInvariantAlarm oc)
            | (c, oc) <- Map.toList owned'
            , not (ocHealthy oc)
            ]
        still = [c | (c, _, _) <- stillPairs]
        msg =
            if null still
                then unconfirmedDiagMsg (epPrompt ep) Nothing owned'
                else reenterAlarmMsg stillPairs
        sig = redSignature still owned'
    out <- emitTurn (epEmits ep) turn (turnRaw t) (msg : redisc)
    let msgs' = msgs ++ out
    writeIORef (epStuck ep) 0
    rs <- readIORef (epReenterStuck ep)
    seen <- readIORef (epSeenRedSigs ep)
    let (seen', repeated) = noProgressStep seen sig
    if not (null still) && repeated
        then
            if rs + 1 >= maxStuckVerifies
                then
                    finish
                        ep
                        owned'
                        (turn + 1)
                        nCalls
                        (bestFailing owned')
                        "stuck_reenter"
                        msgs'
                else do
                    writeIORef (epReenterStuck ep) (rs + 1)
                    writeIORef (epSeenRedSigs ep) seen'
                    runTurns ep start (turn + 1) nCalls (repairs + 1) owned' msgs'
        else do
            writeIORef (epReenterStuck ep) 0
            writeIORef (epSeenRedSigs ep) (if null still then seen else seen')
            runTurns ep start (turn + 1) nCalls (repairs + 1) owned' msgs'

-- | The model called tools: dispatch them all, then report what they returned.
calledTurn :: Stepping (Turn -> IO AgentRun)
calledTurn ep start turn nCalls repairs owned msgs t = do
    results <- mapM (dispatchCall (epSess ep) (epDriver ep) msgs) (turnCalls t)
    let steps = [(crCall r, crOutcome r) | r <- results]
        landedNow = any landedArtifact steps
    done0 <- readIORef (epDelivered ep)
    discovered <-
        if done0
            then pure []
            else
                runDiscoverOutcomes
                    (ssGrammar (epSess ep))
                    (drvDispatch (epDriver ep))
                    [(c, o) | (c, Right o) <- steps]
    when landedNow $ writeIORef (epDelivered ep) True
    noteUnconfirmed ep steps
    let owned' = foldr recordOwned owned steps
    signalMsgs <- doneSignalProbe ep landedNow owned'
    let toolMsgs = [toolMsg c (renderOutcome o) | (c, o) <- steps]
        noteMsgs = maybe [] pure (notesMessage results)
    hints <- streakHints (epStreaks ep) owned'
    out <-
        emitTurn (epEmits ep) turn (turnRaw t) $
            toolMsgs
                ++ noteMsgs
                ++ discovered
                ++ map streakMsg hints
                ++ signalMsgs
    writeIORef (epStuck ep) 0
    down <- kernelFailureStep (epKernelDown ep) (map snd steps)
    if down
        then
            finish
                ep
                owned'
                (turn + 1)
                (nCalls + length steps)
                (bestFailing owned')
                "kernel_error"
                (msgs ++ out)
        else
            runTurns
                ep
                start
                (turn + 1)
                (nCalls + length steps)
                repairs
                owned'
                (msgs ++ out)
