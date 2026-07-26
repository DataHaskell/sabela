{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Capabilities.Try (
    execTry,
    trialPlanErrorText,
) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.ModuleCard (resolveInstalledModules)
import Sabela.AI.Capabilities.Try.Autofix (
    autofixNote,
    hiddenPackageOf,
    notFoundModuleOf,
    renameCandidateCap,
    renameNote,
 )
import Sabela.AI.Capabilities.Try.HoleProbe (runHoleProbe)
import Sabela.AI.Capabilities.Try.Payload (
    disposablePayload,
    inputErrorPayload,
    invariantPayload,
    planErrorPayload,
    purePayload,
    trialPlanErrorText,
    unrestrictedIOPayload,
 )
import Sabela.AI.Capabilities.Try.Snapshot (AppSnapshot (..), snapshotApp)
import Sabela.AI.Capabilities.TryPlan
import Sabela.AI.Capabilities.Util (fieldText, parseCellLang)
import Sabela.AI.DepRepair (addBuildDepend)
import Sabela.AI.ImportRepair (renameModule)
import Sabela.AI.NormalizeGate (gatedRewrite)
import Sabela.AI.PackageIndex (PackageEntry (..))
import Sabela.AI.TypedHole (containsTypedHole)
import Sabela.AI.Types (ToolOutcome (..), errOutcome, okOutcome)
import Sabela.AI.Verdict (VerdictClass (..), verdictTag)
import Sabela.Deps (collectMetadata)
import Sabela.Handlers.Lifecycle (sessionMetaMatches)
import Sabela.Handlers.Plan (executeFullRestart)
import Sabela.Model (Cell (..))
import Sabela.Reactivity (haskellCodeCells)
import Sabela.Session.Materialize
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..))
import Sabela.State.NotebookStore (readNotebook)
import Sabela.State.SessionManager (getHaskellSession)

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
    | otherwise =
        attachNormalizeNote normalizeNotes <$> case planTrial code of
            Left planErr -> pure (errOutcome (planErrorPayload planErr))
            Right _ | containsTypedHole code -> runHoleProbe app code
            Right plan -> runTrialWithDepAutofix app code plan
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

withField :: Text -> Value -> ToolOutcome -> ToolOutcome
withField k v (ToolOk (Object o)) = ToolOk (Object (KM.insert (Key.fromText k) v o))
withField k v (ToolErr (Object o)) = ToolErr (Object (KM.insert (Key.fromText k) v o))
withField _ _ out = out

runTrialWithDepAutofix :: App -> Text -> TrialPlan -> IO ToolOutcome
runTrialWithDepAutofix app code plan = do
    outcome <- runHaskellTrial app plan
    case hiddenPackageOf outcome of
        Just pkg
            | repairedCode <- addBuildDepend pkg code
            , Right repaired <- planTrial repairedCode ->
                withField "autofix" (String (autofixNote pkg repairedCode))
                    <$> runHaskellTrial app repaired
        _ -> case notFoundModuleOf outcome of
            Nothing -> pure outcome
            Just wrong -> do
                cands <- resolveInstalledModules renameCandidateCap wrong
                tryRenames outcome wrong cands
  where
    tryRenames failed _ [] = pure failed
    tryRenames failed wrong ((right, pkg) : rest)
        | right == wrong = tryRenames failed wrong rest
        | repairedCode <-
            addBuildDepend (peName pkg) (renameModule wrong right code)
        , Right repaired <- planTrial repairedCode = do
            retried <- runHaskellTrial app repaired
            case retried of
                ToolOk _ ->
                    pure
                        ( withField
                            "autofix"
                            (String (renameNote wrong right (peName pkg) repairedCode))
                            retried
                        )
                ToolErr _ -> tryRenames failed wrong rest
        | otherwise = tryRenames failed wrong rest

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
                    , candidateReplacesCellId = Nothing
                    , candidateDeliberate = False
                    }
        expression ->
            CandidateSpec
                { candidateMetadataSource = trialSource plan
                , candidateSetup = trialSetup plan
                , candidateExpression = expression
                , candidateReplacesCellId = Nothing
                , candidateDeliberate = False
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
