{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Capabilities.Edit.CompileGate (
    compileGateCheck,
    compileGateSpec,
    gateHoleNudge,
    prevDefinedNames,
    rejectionJson,
) where

import Data.Aeson (Value, (.=))
import Data.Aeson.Types (Pair)
import Data.List (nub)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.Edit.CompileGate.Render (renderNonExecuting)
import Sabela.AI.Capabilities.Edit.HoleNudge (attachPairs, holeNudgePairs)
import Sabela.AI.Health (scopeSubject)
import Sabela.AI.Hints (expectedTypeOf)
import Sabela.AI.HoleFits (holeFitsJson)
import Sabela.AI.HoleProbe (holeProbeJson)
import Sabela.AI.Verdict (VerdictClass (..), verdictTag)
import Sabela.AI.WriteAck (refusalAck)
import Sabela.Api (errorJsonWith)
import Sabela.Diagnose (diagnose, guidancePairs)
import Sabela.Model (Cell (..), CellType (..), lookupCell)
import Sabela.Parse (cellNames)
import Sabela.Session.Materialize (
    CandidateSpec (..),
    DisposableResult (..),
    DisposableVerdict (..),
    MaterializeFailure (..),
    materializeStageText,
    runDisposableTry,
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
            DisposableOk -> pure (Right ())
            verdict -> do
                nudge <- gateHoleNudge app mReplaces verdict (rawDiagnostic result) src
                pure . Left . attachPairs nudge $
                    rejectionJson mReplaces src prevDefined verdict result

gateHoleNudge ::
    App -> Maybe Int -> DisposableVerdict -> Text -> Text -> IO [Pair]
gateHoleNudge app mReplaces verdict diagnostic src
    | verdict /= DisposableCompileError = pure []
    | otherwise = holeNudgePairs probe diagnostic src
  where
    probe s = rawDiagnostic <$> runDisposableTry app (compileGateSpec mReplaces s)

rawDiagnostic :: DisposableResult -> Text
rawDiagnostic result =
    maybe (disposableStderr result) failureMessage (disposableFailure result)

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

rejectionJson ::
    Maybe Int -> Text -> [Text] -> DisposableVerdict -> DisposableResult -> Value
rejectionJson mReplaces src prevDefined verdict result =
    refusalAck "compile-gate" mReplaces $
        errorJsonWith
            message
            ( [ "verdict" .= verdictTag verdictClass
              , "stage" .= maybe "unknown" (materializeStageText . failureStage) failure
              , "diagnostic" .= diagnostic
              , "source" .= src
              ]
                <> removedNotePairs
                <> guidancePairs (diagnose diagnostic)
                <> holeFitPairs
                <> holeProbePairs
                <> expectedTypePairs
            )
  where
    failure = disposableFailure result
    diagnostic =
        let raw = rawDiagnostic result
         in if T.null (T.strip raw)
                then infraFallback
                else dropSelfKnockOns src raw
    infraFallback =
        "The compile gate could not verify this write; nothing was committed."
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
    verdictClass
        | verdict `elem` [DisposableTimedOut, DisposableUnavailable] = VerdictInfra
        | otherwise = VerdictDiagnostic
    message = case verdictClass of
        VerdictInfra ->
            "Could not verify this write (compile gate infrastructure failed): "
                <> diagnostic
                <> " Nothing was committed; retry, or state the blocker."
        _ -> "This candidate does not compile, so nothing was committed: " <> diagnostic
    holeFitPairs =
        case holeFitsJson holeFitCap diagnostic of
            [] -> []
            fits -> ["holeFits" .= fits]
    holeProbePairs = maybe [] (\v -> ["holeProbe" .= v]) (holeProbeJson diagnostic)
    expectedTypePairs =
        maybe [] (\g -> ["expectedType" .= g]) (expectedTypeOf diagnostic)

holeFitCap :: Int
holeFitCap = 8

dropSelfKnockOns :: Text -> Text -> Text
dropSelfKnockOns src raw
    | null kept = raw
    | otherwise = T.intercalate "\n\n" kept
  where
    defined = fst (cellNames src)
    kept = filter (not . phantom) (T.splitOn "\n\n" raw)
    phantom chunk =
        maybe False (`Set.member` defined) (scopeSubject chunk)

removedDefinitions :: [Text] -> Text -> Text -> [Text]
removedDefinitions prevDefined src diagnostic =
    nub
        [ n
        | chunk <- T.splitOn "\n\n" diagnostic
        , Just n <- [scopeSubject chunk]
        , n `elem` prevDefined
        , not (n `Set.member` fst (cellNames src))
        ]
