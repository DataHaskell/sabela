{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | The ordered run inside a live disposable session: compiled modules, the
display prelude, the replayed cells, the safety prelude, and finally the
candidate itself. Each stage either yields to the next or ends the run.
-}
module Sabela.Session.Materialize.Pipeline (
    runMaterialized,
    runCandidate,
    evalCandidate,
) where

import Control.Exception (SomeException, displayException, try)
import qualified Data.Text as T

import Data.Text (Text)
import Sabela.Bridge (bridgePreamble)
import Sabela.Diagnose.KnockOn (dropImportKnockOns)
import Sabela.Model (Cell (..))
import Sabela.Output (displayPrelude)
import Sabela.Reactivity (ExecutionPlan (..))
import Sabela.Session.Materialize.Candidate (
    CandidateSpec (..),
    candidateSafetyPrelude,
    candidateTimeoutUs,
    partitionReplayCells,
    unrestrictedIOError,
 )
import Sabela.Session.Materialize.Replay (
    loadCompiled,
    replayCells,
    snapshotRenderContext,
 )
import Sabela.Session.Materialize.Result (failed, snapshotFailure)
import Sabela.Session.Materialize.Run (runChecked, runOptional)
import Sabela.Session.MaterializeSnapshot (
    MaterializeSnapshot,
    withCurrentSnapshot,
 )
import Sabela.Session.MaterializeStage (
    DisposableResult (..),
    DisposableVerdict (..),
    MaterializeStage (..),
    failureFor,
 )
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..))

runMaterialized ::
    App ->
    MaterializeSnapshot ->
    FilePath ->
    ExecutionPlan ->
    CandidateSpec ->
    DisposableResult ->
    IO () ->
    ST.SessionBackend ->
    IO DisposableResult
runMaterialized app snapshot projectDir plan spec base0 captureBaseline backend = do
    let context = snapshotRenderContext snapshot
        (skipped, toReplay) = partitionReplayCells (epCellsToRun plan)
        base = base0{disposableSkippedCells = skipped}
    compiled <- loadCompiled projectDir backend (epCompilePlan plan)
    case compiled of
        Left msg -> pure (failed base StageCompiled Nothing msg)
        Right () -> do
            preludeResult <- runChecked backend displayPrelude
            case preludeResult of
                Left msg -> pure (failed base StagePrelude Nothing msg)
                Right _ -> do
                    captured <- try captureBaseline
                    case captured of
                        Left (e :: SomeException) ->
                            pure
                                ( failed
                                    base
                                    StagePrelude
                                    Nothing
                                    (T.pack (displayException e))
                                )
                        Right () -> do
                            replayed <- replayCells backend context toReplay
                            case replayed of
                                Left (done, cid, stage, msg) ->
                                    pure
                                        (failed base stage cid msg)
                                            { disposableReplayedCells = compiledIds <> done
                                            }
                                Right (done, scratchBridge) ->
                                    afterReplay base (compiledIds <> done) scratchBridge
  where
    compiledIds = map cellId (epCompileCells plan)

    -- Everything from the post-replay preludes to the candidate run. Split out
    -- so the replay staircase above stays readable.
    afterReplay base replayedIds scratchBridge = do
        let carry result = result{disposableReplayedCells = replayedIds}
            stopAt stage msg = pure (carry (failed base stage Nothing msg))
        finalPrelude <- runChecked backend displayPrelude
        case finalPrelude of
            Left msg -> stopAt StagePrelude msg
            Right _ -> do
                finalBridge <- runOptional backend (bridgePreamble scratchBridge)
                case finalBridge of
                    Left msg -> stopAt StageCellReplay msg
                    Right _ -> do
                        safety <-
                            runChecked backend (candidateSafetyPrelude spec)
                        case safety of
                            Left msg -> stopAt StageSafety msg
                            Right _ -> do
                                let candidateBase = carry base
                                candidate <-
                                    withCurrentSnapshot
                                        app
                                        snapshot
                                        (runCandidate backend spec candidateBase)
                                case candidate of
                                    Left message ->
                                        pure
                                            ( snapshotFailure
                                                candidateBase
                                                replayedIds
                                                message
                                            )
                                    Right result -> pure result

runCandidate ::
    ST.SessionBackend ->
    CandidateSpec ->
    DisposableResult ->
    IO DisposableResult
runCandidate backend spec base = do
    setupResult <-
        if T.null (T.strip (candidateSetup spec))
            then pure (Right ("", ""))
            else runChecked backend (candidateSetup spec)
    case setupResult of
        Left msg ->
            pure
                ( failed
                    base
                    StageCandidateSetup
                    Nothing
                    (dropImportKnockOns (candidateSetup spec) msg)
                )
        Right (setupOut, setupErr) -> case candidateExpression spec of
            Nothing ->
                pure
                    base
                        { disposableVerdict = DisposableOk
                        , disposableStdout = setupOut
                        , disposableStderr = setupErr
                        }
            Just expression -> evalCandidate backend expression base

{- | Evaluate the candidate with the pure evaluator, which admits a value only
after inferring a type that is not IO. One it declines as IO is reported with
that type and left unrun: a fresh project is not a sandbox.
-}
evalCandidate ::
    ST.SessionBackend -> Text -> DisposableResult -> IO DisposableResult
evalCandidate backend expression base = do
    generation <- ST.sbSessionGen backend
    result <-
        ST.sbEvalPureLive
            backend
            ST.PureEvalRequest
                { ST.pureEvalExpectedGeneration = generation
                , ST.pureEvalTimeoutUs = candidateTimeoutUs
                , ST.pureEvalExpression = expression
                }
    let out = ST.pureEvalOutput result
        err = ST.pureEvalError result
        inferredText = T.strip (ST.pureEvalInferredType result)
        inferred =
            if T.null inferredText
                then Nothing
                else Just inferredText
        finish verdict stage =
            base
                { disposableVerdict = verdict
                , disposableType = inferred
                , disposableStdout = out
                , disposableStderr = err
                , disposableFailure = failureFor verdict stage err
                }
    case ST.pureEvalVerdict result of
        ST.PureEvalSucceeded -> pure (finish DisposableOk StageCandidateRun)
        ST.PureEvalUnshowable -> pure (finish DisposableOk StageCandidateRun)
        ST.PureEvalRejected
            | unrestrictedIOError err -> pure (finish DisposableOk StageCandidateTypecheck)
            | otherwise -> pure (finish DisposableCompileError StageCandidateTypecheck)
        ST.PureEvalRuntimeError -> pure (finish DisposableRuntimeError StageCandidateRun)
        ST.PureEvalTimedOut -> pure (finish DisposableTimedOut StageCandidateRun)
        ST.PureEvalStale -> pure (finish DisposableUnavailable StageCandidateTypecheck)
        ST.PureEvalInvariantFailed -> pure (finish DisposableUnavailable StageCandidateRun)
        ST.PureEvalUnavailable -> pure (finish DisposableUnavailable StageCandidateRun)
