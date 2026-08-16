{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Capabilities.Try (
    execTry,
    execTryWith,
    trialPlanErrorText,
) where

import Data.Aeson (Value (..), object, (.=))
import Data.Text (Text)
import qualified Data.Text as T

import Data.List (nub)
import Sabela.AI.Capabilities.ModuleCard (resolveInstalledModules)
import Sabela.AI.Capabilities.Query (execCheckType)
import Sabela.AI.Capabilities.Try.Autofix (
    autofixNote,
    hiddenPackageOf,
    missingDepNote,
    notFoundModuleOf,
    ownerCandidateCap,
    renameCandidateCap,
    renameNote,
    scopedCandidateCap,
 )
import Sabela.AI.Capabilities.Try.Candidate (
    ContainedRun,
    candidateSpec,
    localiseTrial,
 )
import Sabela.AI.Capabilities.Try.Commit (
    discloseCandidate,
    routedEnvelope,
    withField,
    withPairs,
 )
import Sabela.AI.Capabilities.Try.HoleProbe (runHoleProbe)
import Sabela.AI.Capabilities.Try.Payload (
    disposablePayload,
    executionPairs,
    inputErrorPayload,
    invariantPayload,
    planErrorPayload,
    trialPlanErrorText,
    tryRepairPairs,
 )
import Sabela.AI.Capabilities.Try.Route (LiveDecision (..), liveDecision)
import Sabela.AI.Capabilities.Try.Scope (scopedModuleCandidates)
import Sabela.AI.Capabilities.Try.Snapshot (AppSnapshot (..), snapshotApp)
import Sabela.AI.Capabilities.TryPlan
import Sabela.AI.Capabilities.Util (fieldText, parseCellLang)
import Sabela.AI.DepRepair (addBuildDepend)
import Sabela.AI.FactsRow (PkgFacts (..))
import Sabela.AI.HackageFacts (moduleOwners)
import Sabela.AI.ImportRepair (renameModule)
import Sabela.AI.NormalizeGate (gatedRewrite)
import Sabela.AI.PackageIndex (PackageEntry (..))
import Sabela.AI.TypeOriginProbe (
    annotateDisposableWith,
    annotatePureEvalWith,
    exportedByPairs,
    facadeClaims,
 )
import Sabela.AI.TypedHole (containsTypedHole)
import Sabela.AI.Types (ToolOutcome (..), errOutcome, okOutcome)
import Sabela.AI.Verdict (VerdictClass (..), verdictTag)
import Sabela.Deps (collectMetadata)
import Sabela.Diagnose (pinnedDep)
import Sabela.Handlers.Lifecycle (sessionMetaMatches)
import Sabela.Handlers.Plan (executeFullRestart)
import Sabela.Reactivity (cellSettled, haskellCodeCells)
import Sabela.Session.Materialize
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..))
import Sabela.State.NotebookStore (readNotebook)
import Sabela.State.SessionManager (getHaskellSession)
import ScriptHs.Parser (CabalMeta (..))

{- | The trial the tool surface offers. Its contained backend is the disposable
materialisation; 'execTryWith' takes another so the route a trial takes can be
observed without one.
-}
execTry :: App -> Value -> IO ToolOutcome
execTry app = execTryWith (runDisposableTry app) app

execTryWith :: ContainedRun -> App -> Value -> IO ToolOutcome
execTryWith contained app input
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
    | otherwise =
        attachNormalizeNote normalizeNotes <$> case planTrial code of
            Left (TrialMetaQuery q) -> routeMetaQuery app q
            Left planErr -> pure (errOutcome (planErrorPayload planErr))
            Right _ | containsTypedHole code -> runHoleProbe app code
            Right plan -> runTrialWithDepAutofix contained app code plan
  where
    codeField = fieldText "code" input
    expressionField = fieldText "expression" input
    rawCode = if T.null codeField then expressionField else codeField
    rawLanguage = fieldText "language" input
    language =
        if T.null rawLanguage
            then Just ST.Haskell
            else parseCellLang rawLanguage
    (code, normalizeNotes) = gatedRewrite rawCode

attachNormalizeNote :: [Text] -> ToolOutcome -> ToolOutcome
attachNormalizeNote [] out = out
attachNormalizeNote notes out = withField "normalized" (String (T.unwords notes)) out

{- | A meta-command that only reads the type environment is a question, so the
tool that owns types answers it against the candidate's own imports. The answer
keeps try's envelope, so a routed call is still readable as a trial.
-}
routeMetaQuery :: App -> MetaQuery -> IO ToolOutcome
routeMetaQuery app q =
    routedEnvelope (mqCommand q) "check_type"
        <$> execCheckType
            app
            (object ["expr" .= mqSubject q, "imports" .= mqImports q])

runTrialWithDepAutofix ::
    ContainedRun -> App -> Text -> TrialPlan -> IO ToolOutcome
runTrialWithDepAutofix contained app code plan = do
    outcome <- runHaskellTrial contained app plan
    case hiddenPackageOf outcome of
        Just pkg
            | repairedCode <- addBuildDepend pkg code
            , Right repaired <- planTrial repairedCode ->
                withField "autofix" (String (autofixNote pkg repairedCode))
                    <$> runHaskellTrial contained app repaired
        _ -> case notFoundModuleOf outcome of
            Nothing -> pure outcome
            Just wrong -> do
                scoped <- scopedModuleCandidates scopedCandidateCap code wrong
                installed <-
                    if null scoped
                        then resolveInstalledModules renameCandidateCap wrong
                        else pure []
                owners <- take ownerCandidateCap <$> moduleOwners wrong
                let pinEntry e =
                        pinnedDep
                            (peName e)
                            (nonEmptyVersion (peVersion e))
                    named =
                        map (fmap pinEntry) (scoped <> installed)
                            <> [ (wrong, pinnedDep pkgName (nonEmptyVersion (pfVersion facts)))
                               | (pkgName, facts) <- owners
                               ]
                tryRenames outcome wrong (nub named)
  where
    tryRenames failed _ [] = pure failed
    tryRenames failed wrong ((right, pkg) : rest)
        | repairedCode == code = tryRenames failed wrong rest
        | Right repaired <- planTrial repairedCode = do
            retried <- runHaskellTrial contained app repaired
            case retried of
                ToolOk _ -> pure (withField "autofix" (String note) retried)
                ToolErr _ -> tryRenames failed wrong rest
        | otherwise = tryRenames failed wrong rest
      where
        repairedCode = addBuildDepend pkg (renameModule wrong right code)
        note
            | right == wrong = missingDepNote pkg wrong repairedCode
            | otherwise = renameNote wrong right pkg repairedCode

runHaskellTrial :: ContainedRun -> App -> TrialPlan -> IO ToolOutcome
runHaskellTrial contained app plan =
    discloseCandidate app plan =<< routeHaskellTrial contained app plan

routeHaskellTrial :: ContainedRun -> App -> TrialPlan -> IO ToolOutcome
routeHaskellTrial contained app plan
    | candidateNeedsDisposable plan = runDisposable contained app plan
    | Just expression <- trialExpression plan = do
        ready <- liveFastPathReady app
        mBackend <- getHaskellSession (appSessions app)
        case (ready, mBackend) of
            (False, _) -> runDisposable contained app plan
            (True, Nothing) -> runDisposable contained app plan
            (_, Just backend) -> do
                busy <- ST.sbBusy backend
                if busy
                    then runDisposable contained app plan
                    else do
                        decision <- runPureLive app backend expression
                        case decision of
                            LiveAnswer out -> pure out
                            LiveEscalateContained -> runDisposable contained app plan
    | otherwise = runDisposable contained app plan

liveFastPathReady :: App -> IO Bool
liveFastPathReady app = do
    notebook <- readNotebook (appNotebook app)
    metadataMatches <- sessionMetaMatches app (collectMetadata notebook)
    let cells = haskellCodeCells notebook
    pure (metadataMatches && all cellSettled cells)

runPureLive :: App -> ST.SessionBackend -> Text -> IO LiveDecision
runPureLive app backend expression = do
    before <- snapshotApp app
    expectedGeneration <- ST.sbSessionGen backend
    raw <-
        ST.sbEvalPureLive
            backend
            ST.PureEvalRequest
                { ST.pureEvalExpectedGeneration = expectedGeneration
                , ST.pureEvalTimeoutUs = liveTimeoutUs
                , ST.pureEvalExpression = expression
                }
    liveDeps <- metaDeps . collectMetadata <$> readNotebook (appNotebook app)
    liveClaims <- facadeClaims app liveDeps (ST.pureEvalError raw)
    let result = annotatePureEvalWith liveClaims raw
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
    pure (liveDecision invariant result)

recoverDestroyedKernel :: App -> ST.PureEvalResult -> AppSnapshot -> IO ()
recoverDestroyedKernel app result before =
    case ST.pureEvalRecovery result of
        ST.PureEvalKernelDestroyed -> executeFullRestart app (snapshotEventGeneration before)
        _ -> pure ()

runDisposable :: ContainedRun -> App -> TrialPlan -> IO ToolOutcome
runDisposable contained app plan = do
    before <- snapshotApp app
    result <- contained spec
    after <- snapshotApp app
    if before /= after
        then
            pure
                (errOutcome (invariantPayload "notebook state changed during disposable try"))
        else case disposableVerdict result of
            DisposableOk -> pure (disclosed result (okOutcome (disposablePayload result)))
            _ -> do
                claims <-
                    facadeClaims
                        app
                        (disposableDependencies result)
                        (trialDiagnosticText result)
                pairs <- localiseTrial contained plan result
                repair <- tryRepairPairs contained (trialSource plan) result
                let annotated = annotateDisposableWith claims result
                pure
                    ( withPairs
                        (pairs <> repair <> exportedByPairs claims)
                        (disclosed annotated (rejected annotated))
                    )
  where
    spec = candidateSpec plan
    disclosed result = withPairs (executionPairs spec result)
    rejected = errOutcome . disposablePayload
    trialDiagnosticText result =
        disposableStderr result
            <> "\n"
            <> maybe "" failureMessage (disposableFailure result)

liveTimeoutUs :: Int
liveTimeoutUs = 30 * 1000000

nonEmptyVersion :: Text -> Maybe Text
nonEmptyVersion v = if T.null v then Nothing else Just v
