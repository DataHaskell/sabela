module Main (main) where

import qualified Test.AgentPromptSpec as AgentPromptSpec
import qualified Test.ApplicabilitySpec as ApplicabilitySpec
import qualified Test.BenchSpec as BenchSpec
import qualified Test.BudgetRetrySpec as BudgetRetrySpec
import qualified Test.CapabilityBaselineSpec as CapabilityBaselineSpec
import qualified Test.CapabilityCorpusSpec as CapabilityCorpusSpec
import qualified Test.CodePathSpec as CodePathSpec
import qualified Test.ColdStartSpec as ColdStartSpec
import qualified Test.DiscoverSpec as DiscoverSpec
import qualified Test.ElisionLintSpec as ElisionLintSpec
import qualified Test.EpisodeSpec as EpisodeSpec
import qualified Test.ExemplarsSpec as ExemplarsSpec
import qualified Test.FitCheckSpec as FitCheckSpec
import qualified Test.GateLeverSpec as GateLeverSpec
import qualified Test.GateResultSpec as GateResultSpec
import qualified Test.GradeRenderSpec as GradeRenderSpec
import qualified Test.GrammarSpec as GrammarSpec
import qualified Test.GuardGridSpec as GuardGridSpec
import qualified Test.HealthGateSpec as HealthGateSpec
import qualified Test.HoleFitSpec as HoleFitSpec
import Test.Hspec (hspec)
import qualified Test.LintDumpSpec as LintDumpSpec
import qualified Test.OllamaBodySpec as OllamaBodySpec
import qualified Test.OutcomeDistillLintSpec as OutcomeDistillLintSpec
import qualified Test.PreflightSpec as PreflightSpec
import qualified Test.ProposerStepSpec as ProposerStepSpec
import qualified Test.ProviderMapSpec as ProviderMapSpec
import qualified Test.ReasoningCorpusSpec as ReasoningCorpusSpec
import qualified Test.RecoveredCallLintSpec as RecoveredCallLintSpec
import qualified Test.RegressionFloorSpec as RegressionFloorSpec
import qualified Test.RelinkProbeSpec as RelinkProbeSpec
import qualified Test.RepairBudgetSpec as RepairBudgetSpec
import qualified Test.RepairSpec as RepairSpec
import qualified Test.SalvageSpec as SalvageSpec
import qualified Test.SampleVerifySpec as SampleVerifySpec
import qualified Test.ScaffoldSpec as ScaffoldSpec
import qualified Test.SchemaMatchSpec as SchemaMatchSpec
import qualified Test.SpecVerifierSpec as SpecVerifierSpec
import qualified Test.TaskSpec as TaskSpec
import qualified Test.ToolsSpec as ToolsSpec
import qualified Test.TranscriptLintSpec as TranscriptLintSpec
import qualified Test.TranscriptSpec as TranscriptSpec
import qualified Test.VerdictLintSpec as VerdictLintSpec
import qualified Test.VerifierLeakLintSpec as VerifierLeakLintSpec
import qualified Test.VerifyHonestySpec as VerifyHonestySpec
import qualified Test.WrapUpLintSpec as WrapUpLintSpec

main :: IO ()
main = hspec $ do
    AgentPromptSpec.spec
    ApplicabilitySpec.spec
    BenchSpec.spec
    BudgetRetrySpec.spec
    CapabilityBaselineSpec.spec
    CapabilityCorpusSpec.spec
    CodePathSpec.spec
    SchemaMatchSpec.spec
    RecoveredCallLintSpec.spec
    ColdStartSpec.spec
    DiscoverSpec.spec
    EpisodeSpec.spec
    GuardGridSpec.spec
    GateLeverSpec.spec
    GateResultSpec.spec
    GradeRenderSpec.spec
    GrammarSpec.spec
    HoleFitSpec.spec
    HealthGateSpec.spec
    OllamaBodySpec.spec
    ProviderMapSpec.spec
    PreflightSpec.spec
    ReasoningCorpusSpec.spec
    RegressionFloorSpec.spec
    RelinkProbeSpec.spec
    RepairBudgetSpec.spec
    RepairSpec.spec
    SalvageSpec.spec
    SampleVerifySpec.spec
    ProposerStepSpec.spec
    ExemplarsSpec.spec
    FitCheckSpec.spec
    ScaffoldSpec.spec
    SpecVerifierSpec.spec
    TaskSpec.spec
    ToolsSpec.spec
    TranscriptLintSpec.spec
    ElisionLintSpec.spec
    WrapUpLintSpec.spec
    VerdictLintSpec.spec
    VerifierLeakLintSpec.spec
    VerifyHonestySpec.spec
    LintDumpSpec.spec
    OutcomeDistillLintSpec.spec
    TranscriptSpec.spec
