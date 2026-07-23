{-# LANGUAGE OverloadedStrings #-}

{- | The single model-facing speculative execution operation.

Routing is internal. A one-line expression may use the atomic, compiler-
admitted live fast path when a live Haskell session is already idle. Imports,
declarations, multiline expressions, or candidate dependencies use a fresh
disposable notebook reconstruction. Unrestricted effects fail closed.
-}
module Sabela.AI.Capabilities.Try (
    execTry,
    trialPlanErrorText,
) where

import Control.Concurrent.MVar (readMVar)
import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.Types (Pair)
import Data.IORef (readIORef)
import qualified Data.Map.Strict as M
import Data.Maybe (isNothing)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Unique (Unique)

import Sabela.AI.Capabilities.TryPlan
import Sabela.AI.Capabilities.Util (fieldText, parseCellLang)
import Sabela.AI.Types (ToolOutcome, errOutcome, okOutcome)
import Sabela.AI.Verdict (VerdictClass (..), verdictTag)
import Sabela.Deps (ProjectSig, collectMetadata)
import Sabela.Handlers.Lifecycle (sessionMetaMatches)
import Sabela.Handlers.Plan (executeFullRestart)
import Sabela.Model (Cell (..), Notebook)
import Sabela.Reactivity (haskellCodeCells)
import Sabela.Session.Materialize
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..))
import Sabela.State.BridgeStore (getBridgeValues)
import Sabela.State.DependencyTracker (
    getHaskellDeps,
    getHaskellExts,
    getHaskellProjectSig,
    getPythonDeps,
 )
import Sabela.State.EventBus (EventBus (..))
import Sabela.State.NotebookStore (readNotebook)
import Sabela.State.SessionManager (getHaskellSession)
import Sabela.State.WidgetStore (WidgetStore (..))

execTry :: App -> Value -> IO ToolOutcome
execTry app input
    | T.null rawCode =
        pure (errOutcome (inputErrorPayload "code required"))
    | Nothing <- language =
        pure
            ( errOutcome
                ( inputErrorPayload
                    ("Unknown language: " <> rawLanguage <> ". Expected Haskell or Python.")
                )
            )
    | Just ST.Python <- language =
        pure . errOutcome $
            object
                [ "route" .= ("unavailable" :: Text)
                , "verdict" .= verdictTag VerdictCouldNotRun
                , "outcome" .= ("unavailable" :: Text)
                , "reason"
                    .= ( "Python execution is unrestricted and no qualified containment backend is available; no candidate code ran." ::
                            Text
                       )
                ]
    | otherwise = case planTrial rawCode of
        Left planErr -> pure (errOutcome (planErrorPayload planErr))
        Right plan -> runHaskellTrial app plan
  where
    codeField = fieldText "code" input
    expressionField = fieldText "expression" input
    rawCode = if T.null codeField then expressionField else codeField
    rawLanguage = fieldText "language" input
    language =
        if T.null rawLanguage
            then Just ST.Haskell
            else parseCellLang rawLanguage

runHaskellTrial :: App -> TrialPlan -> IO ToolOutcome
runHaskellTrial app plan
    | candidateNeedsDisposable plan = runDisposable app plan
    | Just expression <- trialExpression plan = do
        ready <- liveFastPathReady app
        mBackend <- getHaskellSession (appSessions app)
        case (ready, mBackend) of
            (False, _) -> runDisposable app plan
            (True, Nothing) -> runDisposable app plan
            (_, Just backend) -> do
                busy <- ST.sbBusy backend
                if busy
                    then runDisposable app plan
                    else runPureLive app backend expression
    | otherwise = runDisposable app plan

{- | A process being alive is not enough: after an edit, failed cell, package
directive change, or hard reset its interactive bindings may not represent
the current notebook. The live shortcut is allowed only for a clean
notebook whose installed project metadata still matches.
-}
liveFastPathReady :: App -> IO Bool
liveFastPathReady app = do
    notebook <- readNotebook (appNotebook app)
    metadataMatches <- sessionMetaMatches app (collectMetadata notebook)
    let cells = haskellCodeCells notebook
        settled cell = not (cellDirty cell) && isNothing (cellError cell)
    pure (metadataMatches && all settled cells)

runPureLive :: App -> ST.SessionBackend -> Text -> IO ToolOutcome
runPureLive app backend expression = do
    before <- snapshotApp app
    expectedGeneration <- ST.sbSessionGen backend
    result <-
        ST.sbEvalPureLive
            backend
            ST.PureEvalRequest
                { ST.pureEvalExpectedGeneration = expectedGeneration
                , ST.pureEvalTimeoutUs = liveTimeoutUs
                , ST.pureEvalExpression = expression
                }
    after <- snapshotApp app
    current <- getHaskellSession (appSessions app)
    let sameBackend =
            maybe False ((== ST.sbSessionId backend) . ST.sbSessionId) current
        invariant =
            before == after
                && sameBackend
                && ST.pureEvalBindingsUnchanged result
                && ST.pureEvalItUnchanged result
    recoverDestroyedKernel app result before
    case ST.pureEvalVerdict result of
        ST.PureEvalSucceeded
            | invariant -> pure (okOutcome (purePayload result))
            | otherwise ->
                pure (errOutcome (invariantPayload "live state changed during try"))
        ST.PureEvalRejected
            | invariant && isUnrestrictedIO result ->
                pure (errOutcome (unrestrictedIOPayload result))
            | invariant -> pure (errOutcome (purePayload result))
            | otherwise ->
                pure (errOutcome (invariantPayload "live state changed while checking try"))
        ST.PureEvalTimedOut -> pure (errOutcome (purePayload result))
        ST.PureEvalRuntimeError -> pure (errOutcome (purePayload result))
        ST.PureEvalStale -> pure (errOutcome (purePayload result))
        ST.PureEvalInvariantFailed -> pure (errOutcome (purePayload result))
        ST.PureEvalUnavailable -> pure (errOutcome (purePayload result))

{- | A failed marker resynchronization destroys the live kernel. Rebuild and
replay synchronously before returning so a bounded trial cannot leave the
notebook pointing at a dead interpreter.
-}
recoverDestroyedKernel :: App -> ST.PureEvalResult -> AppSnapshot -> IO ()
recoverDestroyedKernel app result before =
    case ST.pureEvalRecovery result of
        ST.PureEvalKernelDestroyed -> executeFullRestart app (snapshotEventGeneration before)
        _ -> pure ()

isUnrestrictedIO :: ST.PureEvalResult -> Bool
isUnrestrictedIO result =
    let lower = T.toLower (ST.pureEvalError result)
     in "scratch candidate is io" `T.isInfixOf` lower
            || "sabela_unrestricted_io" `T.isInfixOf` lower

unrestrictedIOPayload :: ST.PureEvalResult -> Value
unrestrictedIOPayload result =
    object
        [ "route" .= ("unavailable" :: Text)
        , "verdict" .= verdictTag VerdictCouldNotRun
        , "outcome" .= ("unavailable" :: Text)
        , "reason"
            .= ( "The candidate has an unrestricted IO type and no qualified containment backend is available; no candidate code ran." ::
                    Text
               )
        , "type" .= ST.pureEvalInferredType result
        , "purityAssurance" .= ("type_only" :: Text)
        , "generation" .= ST.pureEvalGeneration result
        , "bindingsUnchanged" .= ST.pureEvalBindingsUnchanged result
        , "itUnchanged" .= ST.pureEvalItUnchanged result
        ]

runDisposable :: App -> TrialPlan -> IO ToolOutcome
runDisposable app plan = do
    before <- snapshotApp app
    result <- runDisposableTry app (candidateSpec plan)
    after <- snapshotApp app
    if before /= after
        then
            pure
                (errOutcome (invariantPayload "notebook state changed during disposable try"))
        else pure $
            case disposableVerdict result of
                DisposableOk -> okOutcome (disposablePayload result)
                _ -> errOutcome (disposablePayload result)

candidateSpec :: TrialPlan -> CandidateSpec
candidateSpec plan =
    case trialExpression plan of
        Just expression
            | T.any (`elem` ['\n', '\r']) expression ->
                CandidateSpec
                    { candidateMetadataSource = trialSource plan
                    , candidateSetup =
                        trialSetup plan
                            <> "\n"
                            <> hiddenExpressionBinding expression
                    , candidateExpression = Just "_sabelaTryCandidate"
                    }
        expression ->
            CandidateSpec
                { candidateMetadataSource = trialSource plan
                , candidateSetup = trialSetup plan
                , candidateExpression = expression
                }

hiddenExpressionBinding :: Text -> Text
hiddenExpressionBinding expression =
    T.unlines
        [ ":{"
        , "_sabelaTryCandidate ="
        , T.unlines ["    " <> line | line <- T.lines expression]
        , ":}"
        ]

liveTimeoutUs :: Int
liveTimeoutUs = 30 * 1000000

purePayload :: ST.PureEvalResult -> Value
purePayload result =
    object
        [ "route" .= ("pure_live" :: Text)
        , "verdict" .= verdictTag (pureVerdictClass (ST.pureEvalVerdict result))
        , "outcome" .= pureOutcomeText (ST.pureEvalVerdict result)
        , "type" .= ST.pureEvalInferredType result
        , "stdout" .= ST.pureEvalOutput result
        , "stderr" .= ST.pureEvalError result
        , "purityAssurance" .= ("type_only" :: Text)
        , "pollutionContract" .= ("semantic_read_only" :: Text)
        , "generation" .= ST.pureEvalGeneration result
        , "bindingsUnchanged" .= ST.pureEvalBindingsUnchanged result
        , "itUnchanged" .= ST.pureEvalItUnchanged result
        , "recovery" .= pureRecoveryText (ST.pureEvalRecovery result)
        ]

disposablePayload :: DisposableResult -> Value
disposablePayload result =
    object
        ( [ "route" .= disposableRoute result
          , "verdict" .= verdictTag (disposableVerdictClass (disposableVerdict result))
          , "outcome" .= disposableOutcomeText (disposableVerdict result)
          , "type" .= disposableType result
          , "stdout" .= disposableStdout result
          , "stderr" .= disposableStderr result
          , "purityAssurance" .= ("type_only" :: Text)
          , "pollutionContract" .= ("disposable_session" :: Text)
          , "replayedCells" .= disposableReplayedCells result
          , "dependencies" .= disposableDependencies result
          ]
            <> failurePairs (disposableFailure result)
            <> silentSuccessPairs result
        )

silentSuccessPairs :: DisposableResult -> [Pair]
silentSuccessPairs result
    | disposableVerdict result == DisposableOk
        && T.null (T.strip (disposableStdout result))
        && T.null (T.strip (disposableStderr result)) =
        [ "diagnostic"
            .= ( "Candidate declarations compiled successfully; no expression was executed." ::
                    Text
               )
        ]
    | otherwise = []

failurePairs :: Maybe MaterializeFailure -> [Pair]
failurePairs Nothing = []
failurePairs (Just failure) =
    [ "failure"
        .= object
            [ "stage" .= materializeStageText (failureStage failure)
            , "cellId" .= failureCellId failure
            , "message" .= failureMessage failure
            ]
    ]

planErrorPayload :: TrialPlanError -> Value
planErrorPayload planErr =
    object
        [ "route" .= ("unavailable" :: Text)
        , "verdict" .= verdictTag VerdictDiagnostic
        , "outcome" .= ("rejected" :: Text)
        , "reason" .= trialPlanErrorText planErr
        ]

invariantPayload :: Text -> Value
invariantPayload reason =
    object
        [ "route" .= ("unavailable" :: Text)
        , "verdict" .= verdictTag VerdictCouldNotRun
        , "outcome" .= ("invariant_failed" :: Text)
        , "reason" .= reason
        ]

inputErrorPayload :: Text -> Value
inputErrorPayload reason =
    object
        [ "route" .= ("unavailable" :: Text)
        , "verdict" .= verdictTag VerdictDiagnostic
        , "outcome" .= ("invalid_input" :: Text)
        , "reason" .= reason
        ]

trialPlanErrorText :: TrialPlanError -> Text
trialPlanErrorText planErr = case planErr of
    TrialEmpty -> "code required"
    TrialMultipleExpressions ->
        "try accepts imports/declarations and at most one final expression; no code ran"
    TrialExpressionNotFinal ->
        "the optional try expression must be the final code unit; no code was reordered or run"
    TrialEffectfulStatement stmt ->
        "unrestricted effectful statement is unavailable; no code ran: " <> stmt
    TrialMetaCommand cmd ->
        "GHCi meta-commands are not admitted by try; no command ran: " <> cmd
    TrialUnsafeSyntax reason -> reason <> "; no code ran"

pureOutcomeText :: ST.PureEvalVerdict -> Text
pureOutcomeText verdict = case verdict of
    ST.PureEvalSucceeded -> "ok"
    ST.PureEvalRejected -> "compile_error"
    ST.PureEvalRuntimeError -> "runtime_error"
    ST.PureEvalTimedOut -> "timed_out"
    ST.PureEvalStale -> "stale"
    ST.PureEvalInvariantFailed -> "invariant_failed"
    ST.PureEvalUnavailable -> "unavailable"

pureRecoveryText :: ST.PureEvalRecovery -> Text
pureRecoveryText recovery = case recovery of
    ST.PureEvalNoRecovery -> "none"
    ST.PureEvalInterrupted -> "interrupted"
    ST.PureEvalKernelDestroyed -> "restarted_kernel"

pureVerdictClass :: ST.PureEvalVerdict -> VerdictClass
pureVerdictClass verdict = case verdict of
    ST.PureEvalSucceeded -> VerdictOk
    ST.PureEvalRejected -> VerdictDiagnostic
    ST.PureEvalRuntimeError -> VerdictDiagnostic
    ST.PureEvalTimedOut -> VerdictDiagnostic
    ST.PureEvalStale -> VerdictCouldNotRun
    ST.PureEvalInvariantFailed -> VerdictCouldNotRun
    ST.PureEvalUnavailable -> VerdictCouldNotRun

disposableOutcomeText :: DisposableVerdict -> Text
disposableOutcomeText verdict = case verdict of
    DisposableOk -> "ok"
    DisposableCompileError -> "compile_error"
    DisposableRuntimeError -> "runtime_error"
    DisposableTimedOut -> "timed_out"
    DisposableUnavailable -> "unavailable"

disposableVerdictClass :: DisposableVerdict -> VerdictClass
disposableVerdictClass verdict = case verdict of
    DisposableOk -> VerdictOk
    DisposableCompileError -> VerdictDiagnostic
    DisposableRuntimeError -> VerdictDiagnostic
    DisposableTimedOut -> VerdictDiagnostic
    DisposableUnavailable -> VerdictCouldNotRun

materializeStageText :: MaterializeStage -> Text
materializeStageText stage = case stage of
    StagePlan -> "plan"
    StageProject -> "project"
    StageSession -> "session"
    StageCompiled -> "compiled"
    StagePrelude -> "prelude"
    StageCellReplay -> "cell_replay"
    StageSnapshot -> "snapshot"
    StageSafety -> "safety"
    StageCandidateSetup -> "candidate_setup"
    StageCandidateTypecheck -> "candidate_typecheck"
    StageCandidateRun -> "candidate_run"

data AppSnapshot = AppSnapshot
    { snapshotNotebook :: Notebook
    , snapshotEventGeneration :: Int
    , snapshotHaskellDeps :: S.Set Text
    , snapshotHaskellExts :: S.Set Text
    , snapshotProjectSig :: ProjectSig
    , snapshotPythonDeps :: S.Set Text
    , snapshotCompiledModules :: M.Map Text Text
    , snapshotBridgeValues :: M.Map Text Text
    , snapshotWidgetValues :: M.Map Int (M.Map Text Text)
    , snapshotHaskellSession :: Maybe (Unique, Int)
    }
    deriving (Eq)

snapshotApp :: App -> IO AppSnapshot
snapshotApp app = do
    notebook <- readNotebook (appNotebook app)
    eventGeneration <- readIORef (ebGeneration (appEvents app))
    haskellDeps <- getHaskellDeps (appDeps app)
    haskellExts <- getHaskellExts (appDeps app)
    projectSig <- getHaskellProjectSig (appDeps app)
    pythonDeps <- getPythonDeps (appDeps app)
    compiled <- readIORef (appCompiledModules app)
    bridge <- getBridgeValues (appBridge app)
    widgets <- readMVar (wsValues (appWidgets app))
    session <- getHaskellSession (appSessions app)
    sessionFingerprint <- case session of
        Nothing -> pure Nothing
        Just backend -> do
            generation <- ST.sbSessionGen backend
            pure (Just (ST.sbSessionId backend, generation))
    pure
        AppSnapshot
            { snapshotNotebook = notebook
            , snapshotEventGeneration = eventGeneration
            , snapshotHaskellDeps = haskellDeps
            , snapshotHaskellExts = haskellExts
            , snapshotProjectSig = projectSig
            , snapshotPythonDeps = pythonDeps
            , snapshotCompiledModules = compiled
            , snapshotBridgeValues = bridge
            , snapshotWidgetValues = widgets
            , snapshotHaskellSession = sessionFingerprint
            }
