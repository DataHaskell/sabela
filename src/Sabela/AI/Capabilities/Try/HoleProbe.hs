{-# LANGUAGE OverloadedStrings #-}

{- | G3 task 4: @try@'s typecheck-only admission of a hole-bearing candidate.
A typed hole cannot compile, so it can never run — the candidate is put
through the SAME declarations-only disposable trial G1's gate uses
('compileGateSpec': no expression, nothing forced, nothing committed) and
the compiler's fits come back as an ANSWER, not a failure. Voluntary probing
by the model is fine; nothing in the harness ever asks it to write a hole.
-}
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

{- | Answer a hole-bearing candidate. Infrastructure failure fails closed with
the infra verdict; anything else is an answer, because a hole-bearing
candidate is EXPECTED not to compile — that refusal is the reply.
-}
runHoleProbe :: App -> Text -> IO ToolOutcome
runHoleProbe app src = do
    result <- runDisposableTry app (compileGateSpec Nothing src)
    pure $ case disposableVerdict result of
        verdict
            | verdict `elem` [DisposableTimedOut, DisposableUnavailable] ->
                errOutcome (infraPayload result)
        _ -> okOutcome (holeProbePayload src result)

-- | The diagnostic the trial reported, from its failure or its stderr.
probeDiagnostic :: DisposableResult -> Text
probeDiagnostic result =
    maybe (disposableStderr result) failureMessage (disposableFailure result)

{- | The answer envelope: the route that ran, the compiler's own diagnostic,
its parsed fits, and the plain conclusions. @evaluated@ is 'False' by
construction — the trial carries no expression to evaluate.
-}
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

-- | Fail closed: the probe could not be run, so it asserts nothing.
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

{- | How many fits a probe ships. A goal like @_ :: Picture -> Picture ->
Picture@ can fit dozens of names; past the first handful they are noise the
model pays context for.
-}
holeFitCap :: Int
holeFitCap = 8
