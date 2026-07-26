{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Capabilities.Try.HoleProbe (
    runHoleProbe,
    holeProbePayload,
) where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.Edit.CompileGate (compileGateSpec)
import Sabela.AI.HoleFits (holeFitsJson)
import Sabela.AI.HoleProbe (holeProbeFacts, holeProbeJson)
import Sabela.AI.Types (ToolOutcome, errOutcome, okOutcome)
import Sabela.AI.Verdict (VerdictClass (..), verdictTag)
import Sabela.Session.Materialize (
    DisposableResult (..),
    DisposableVerdict (..),
    MaterializeFailure (..),
    materializeStageText,
    runDisposableTry,
 )
import Sabela.State (App)

runHoleProbe :: App -> Text -> IO ToolOutcome
runHoleProbe app src = do
    result <- runDisposableTry app (compileGateSpec Nothing src)
    pure $ case disposableVerdict result of
        verdict
            | verdict `elem` [DisposableTimedOut, DisposableUnavailable] ->
                errOutcome (infraPayload result)
        _ -> okOutcome (holeProbePayload src result)

probeDiagnostic :: DisposableResult -> Text
probeDiagnostic result =
    maybe (disposableStderr result) failureMessage (disposableFailure result)

holeProbePayload :: Text -> DisposableResult -> Value
holeProbePayload src result =
    object
        [ "route" .= ("typecheck_only" :: Text)
        , "verdict" .= verdictTag VerdictOk
        , "outcome" .= ("hole_fits" :: Text)
        , "evaluated" .= False
        , "purityAssurance" .= ("typecheck_only" :: Text)
        , "pollutionContract" .= ("disposable_session" :: Text)
        , "source" .= src
        , "diagnostic" .= diagnostic
        , "holeFits" .= holeFitsJson holeFitCap diagnostic
        , "answer" .= holeProbeFacts diagnostic
        , "holeProbe" .= holeProbeJson diagnostic
        , "stdout" .= disposableStdout result
        , "replayedCells" .= disposableReplayedCells result
        ]
  where
    diagnostic = probeDiagnostic result

infraPayload :: DisposableResult -> Value
infraPayload result =
    object
        [ "route" .= ("typecheck_only" :: Text)
        , "verdict" .= verdictTag VerdictInfra
        , "outcome" .= ("unavailable" :: Text)
        , "evaluated" .= False
        , "stage" .= maybe "unknown" (materializeStageText . failureStage) failure
        , "reason"
            .= ( "The hole probe could not reach the compiler; no producers were \
                 \established. Retry, or state the blocker." ::
                    Text
               )
        , "diagnostic" .= T.strip (probeDiagnostic result)
        ]
  where
    failure = disposableFailure result

holeFitCap :: Int
holeFitCap = 8
