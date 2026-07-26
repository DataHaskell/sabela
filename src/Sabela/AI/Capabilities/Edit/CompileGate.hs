{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Capabilities.Edit.CompileGate (
    compileGateCheck,
    compileGateSpec,
    rejectionJson,
) where

import Data.Aeson (Value, (.=))
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.Edit.CompileGate.Render (renderNonExecuting)
import Sabela.AI.Hints (expectedTypeOf)
import Sabela.AI.HoleFits (holeFitsJson)
import Sabela.AI.HoleProbe (holeProbeJson)
import Sabela.AI.Verdict (VerdictClass (..), verdictTag)
import Sabela.AI.WriteAck (refusalAck)
import Sabela.Api (errorJsonWith)
import Sabela.Model (CellType (..))
import Sabela.Session.Materialize (
    CandidateSpec (..),
    DisposableResult (..),
    DisposableVerdict (..),
    MaterializeFailure (..),
    materializeStageText,
    runDisposableTry,
 )
import Sabela.SessionTypes (CellLang (..))
import Sabela.State (App)

compileGateCheck ::
    App -> Maybe Int -> CellLang -> CellType -> Text -> IO (Either Value ())
compileGateCheck app mReplaces lang ty src
    | ty /= CodeCell || lang /= Haskell = pure (Right ())
    | otherwise = do
        result <- runDisposableTry app (compileGateSpec mReplaces src)
        pure $ case disposableVerdict result of
            DisposableOk -> Right ()
            verdict -> Left (rejectionJson mReplaces src verdict result)

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
    Maybe Int -> Text -> DisposableVerdict -> DisposableResult -> Value
rejectionJson mReplaces src verdict result =
    refusalAck "compile-gate" mReplaces $
        errorJsonWith
            message
            ( [ "verdict" .= verdictTag verdictClass
              , "stage" .= maybe "unknown" (materializeStageText . failureStage) failure
              , "diagnostic" .= diagnostic
              , "source" .= src
              ]
                <> holeFitPairs
                <> holeProbePairs
                <> expectedTypePairs
            )
  where
    failure = disposableFailure result
    diagnostic =
        let raw = maybe (disposableStderr result) failureMessage failure
         in if T.null (T.strip raw) then infraFallback else raw
    infraFallback =
        "The compile gate could not verify this write; nothing was committed."
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
