{- | The trial's half of the shared repair seam. A rejected trial is the same
question the write gate asks — this candidate does not compile, what would —
so it is answered by the same code over the trial's own contained compile.
-}
module Sabela.AI.Capabilities.Try.Payload.Repair (
    containedProbe,
    trialDiagnostic,
    tryRepairPairs,
) where

import Data.Aeson.Types (Pair)
import Data.Text (Text)

import Sabela.AI.Capabilities.Edit.CompileGate.Render (renderNonExecuting)
import Sabela.AI.Capabilities.Edit.HoleRewrite.Repair (repairPairs)
import Sabela.AI.Capabilities.Try.Candidate (ContainedRun)
import Sabela.Session.Materialize (CandidateSpec (..))
import Sabela.Session.MaterializeStage (
    DisposableResult (..),
    DisposableVerdict (..),
    MaterializeFailure (..),
 )

{- | What a rejected trial earns. Only a candidate the compiler read and
refused is answered, so a trial stopped before that costs no extra compile.
-}
tryRepairPairs :: ContainedRun -> Text -> DisposableResult -> IO [Pair]
tryRepairPairs run src result
    | disposableVerdict result /= DisposableCompileError = pure []
    | otherwise = repairPairs (containedProbe run) (trialDiagnostic result) src

{- | The repair probe over a trial's own contained compile. The rewritten
candidate goes to the session as the write gate sends one — rendered
non-executing, no expression submitted — and not through the trial planner,
which refuses the harness's own marker on sight.
-}
containedProbe :: ContainedRun -> (Text -> IO Text)
containedProbe run = fmap trialDiagnostic . run . probeSpec

probeSpec :: Text -> CandidateSpec
probeSpec s =
    CandidateSpec
        { candidateMetadataSource = s
        , candidateSetup = renderNonExecuting s
        , candidateExpression = Nothing
        , candidateReplacesCellId = Nothing
        , candidateDeliberate = False
        }

-- | What the compiler said about a contained run, wherever it said it.
trialDiagnostic :: DisposableResult -> Text
trialDiagnostic result =
    maybe (disposableStderr result) failureMessage (disposableFailure result)
