{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Capabilities.Edit.CompileGate (
    compileGateCheck,
    compileGateSpec,
    gateHoleNudge,
    gateRepairPairs,
    holeAnswerPairs,
    presentedDiagnostic,
    prevDefinedNames,
    GateSource (..),
    submittedOnly,
    gateDefaultingRejection,
    rejectionJson,
    notCommittedKind,
    exposingPackage,
    defaultedToUnit,
) where

import Data.Aeson (Value, object, (.=))
import Data.Aeson.Types (Pair)
import Data.List (nub)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.Edit.CompileGate.Defaulting (
    defaultedToUnit,
    gateDefaultingRejection,
 )
import Sabela.AI.Capabilities.Edit.CompileGate.Render (renderNonExecuting)
import Sabela.AI.Capabilities.Edit.CompileGate.Source (
    GateSource (..),
    compiledSourcePairs,
    submittedOnly,
 )
import Sabela.AI.Capabilities.Edit.HoleNudge (attachPairs)
import Sabela.AI.Capabilities.Edit.HoleRewrite.Repair (
    diagnosticRepairPairs,
    holeAnswerPairs,
    repairDiagnostic,
    repairPairs,
 )
import Sabela.AI.Capabilities.Edit.OrphanGate (undeclaredImportPairs)
import Sabela.AI.Health (scopeSubject)
import Sabela.AI.Verdict (VerdictClass (..), verdictTag)
import Sabela.AI.WriteAck (refusalAck)
import Sabela.Api (errorJsonWith)
import Sabela.Diagnose (couldNotFindModule, diagnoseWith, guidancePairs)
import Sabela.Diagnose.Packages (findModulePackage)
import Sabela.Model (Cell (..), CellType (..), lookupCell)
import Sabela.Parse (cellNames)
import Sabela.Session.Materialize (
    CandidateSpec (..),
    runDisposableTry,
 )
import Sabela.Session.MaterializeStage (
    DisposableResult (..),
    DisposableVerdict (..),
    MaterializeFailure (..),
    SkippedCell (..),
    attributionOf,
    attributionText,
    blamedCell,
    blockedRemedy,
    materializeStageText,
    reachedCandidate,
    resultVerdictClass,
 )
import Sabela.SessionTypes (CellLang (..))
import Sabela.State (App (..))
import Sabela.State.NotebookStore (readNotebook)

compileGateCheck ::
    App -> Maybe Int -> CellLang -> CellType -> Text -> IO (Either Value ())
compileGateCheck app mReplaces lang ty src
    | ty /= CodeCell || lang /= Haskell = pure (Right ())
    | otherwise = do
        result <- runDisposableTry app (compileGateSpec mReplaces src)
        prevDefined <- prevDefinedNames app mReplaces
        case disposableVerdict result of
            DisposableOk ->
                pure
                    ( maybe (Right ()) Left $
                        gateDefaultingRejection mReplaces [] (submittedOnly src) result
                    )
            verdict -> do
                let gsrc = submittedOnly src
                repair <-
                    gateHoleNudge app mReplaces verdict (rawDiagnostic result) src
                exposedBy <- exposingPackage (rawDiagnostic result)
                diverge <- undeclaredImportPairs app result
                pure . Left . attachPairs (repair <> diverge) $
                    rejectionJson exposedBy mReplaces gsrc prevDefined result

{- | Everything a rejection earns, for the cost of one extra compile: the
rewrite when a hole can be placed, the standing nudge only when it cannot, and
what the diagnostic already carried.
-}
gateHoleNudge ::
    App -> Maybe Int -> DisposableVerdict -> Text -> Text -> IO [Pair]
gateHoleNudge app mReplaces =
    gateRepairPairs probe
  where
    probe s = rawDiagnostic <$> runDisposableTry app (compileGateSpec mReplaces s)

{- | The gate's half of the shared repair seam, over any probe, so its cost can
be counted and its answer compared with the trial's for the same rejection.
-}
gateRepairPairs ::
    (Text -> IO Text) -> DisposableVerdict -> Text -> Text -> IO [Pair]
gateRepairPairs compile verdict diagnostic src
    | verdict /= DisposableCompileError = pure []
    | otherwise = repairPairs compile diagnostic src

rawDiagnostic :: DisposableResult -> Text
rawDiagnostic result =
    maybe (disposableStderr result) failureMessage (disposableFailure result)

{- | The diagnostic the rejection presents. It is the seam's own normaliser, so
the words the payload shows are the words its repair was computed from.
-}
presentedDiagnostic :: GateSource -> DisposableResult -> Text
presentedDiagnostic gsrc result
    | T.null (T.strip presented) = infraFallback
    | otherwise = presented
  where
    presented = repairDiagnostic (gateCompiled gsrc) (rawDiagnostic result)

infraFallback :: Text
infraFallback =
    "The compile gate could not verify this write; nothing was committed."

{- | Names the replaced cell's current source defines; a rejection can then
explain a not-in-scope name the replacement itself removed.
-}
prevDefinedNames :: App -> Maybe Int -> IO [Text]
prevDefinedNames _ Nothing = pure []
prevDefinedNames app (Just cid) = do
    nb <- readNotebook (appNotebook app)
    pure (maybe [] (Set.toList . fst . cellNames . cellSource) (lookupCell cid nb))

compileGateSpec :: Maybe Int -> Text -> CandidateSpec
compileGateSpec mReplaces src =
    CandidateSpec
        { candidateMetadataSource = src
        , candidateSetup = renderNonExecuting src
        , candidateExpression = Nothing
        , candidateReplacesCellId = mReplaces
        , candidateDeliberate = True
        }

{- | The package exposing the module a diagnostic could not find, if any is
installed. Resolved here because it is a fact about the world; `rejectionJson`
stays pure and simply reports it.
-}
exposingPackage :: Text -> IO (Maybe Text)
exposingPackage diagnostic = case couldNotFindModule diagnostic of
    Nothing -> pure Nothing
    Just m -> findModulePackage m

{- | Why nothing was committed, and everything the trial learned about why.
Guidance is refined against the text the payload presents as @source@, so no
claim it makes about "this cell" is about bytes the caller cannot see.
-}
rejectionJson ::
    Maybe Text ->
    Maybe Int ->
    GateSource ->
    [Text] ->
    DisposableResult ->
    Value
rejectionJson exposedBy mReplaces gsrc prevDefined result =
    refusalAck (notCommittedKind result) mReplaces $
        errorJsonWith
            message
            ( [ "verdict" .= verdictTag verdictClass
              , "stage" .= stageText
              , "attributedTo" .= attributionText attribution
              , "diagnostic" .= diagnostic
              , "source" .= submitted
              ]
                <> compiledSourcePairs gsrc
                <> contextPairs
                <> removedNotePairs
                <> guidancePairs (diagnoseWith exposedBy submitted diagnostic)
                <> diagnosticRepairPairs diagnostic
            )
  where
    src = gateCompiled gsrc
    submitted = gateSubmitted gsrc
    failure = disposableFailure result
    attribution = attributionOf result
    stageText = maybe "unknown" (materializeStageText . failureStage) failure
    contextPairs =
        [ "replayedCells" .= disposableReplayedCells result
        | not (null (disposableReplayedCells result))
        ]
            <> [ "skippedCells" .= map skippedJson (disposableSkippedCells result)
               | not (null (disposableSkippedCells result))
               ]
            <> ["blamedCell" .= cid | Just cid <- [blamedCell result]]
    skippedJson sc =
        object ["cellId" .= skippedCellId sc, "reason" .= skippedReason sc]
    diagnostic = presentedDiagnostic gsrc result
    removedNotePairs = case removedDefinitions prevDefined src diagnostic of
        [] -> []
        names ->
            [ "note"
                .= ( T.intercalate ", " names
                        <> " was defined by the previous version of this cell; \
                           \this replacement removes it. Keep its definition, \
                           \or define it in another cell first."
                   )
            ]
    verdictClass = resultVerdictClass result
    message = case verdictClass of
        VerdictInfra ->
            "Could not verify this write (compile gate infrastructure failed): "
                <> diagnostic
                <> " Nothing was committed."
                <> remedySentence
        VerdictDiagnostic ->
            "This candidate does not compile, so nothing was committed: " <> diagnostic
        _ ->
            "Your candidate was never compiled. The trial run stopped at the "
                <> stageText
                <> " stage, in "
                <> attributionText attribution
                <> ", so nothing was committed: "
                <> diagnostic
                <> remedySentence
    remedySentence =
        maybe
            " Retry, or state the blocker."
            (\f -> " " <> blockedRemedy f attribution)
            failure

{- | Why nothing was committed. Only a candidate the compiler actually read
was refused by the compile gate; a trial that stopped earlier was blocked
whatever stopped it, so the label reads the stage rather than the verdict.
-}
notCommittedKind :: DisposableResult -> Text
notCommittedKind result
    | reachedCandidate result = "compile-gate"
    | otherwise = "trial-blocked"

removedDefinitions :: [Text] -> Text -> Text -> [Text]
removedDefinitions prevDefined src diagnostic =
    nub
        [ n
        | chunk <- T.splitOn "\n\n" diagnostic
        , Just n <- [scopeSubject chunk]
        , n `elem` prevDefined
        , not (n `Set.member` fst (cellNames src))
        ]
