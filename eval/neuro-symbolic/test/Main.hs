module Main (main) where

import qualified Test.BenchSpec as BenchSpec
import qualified Test.BudgetRetrySpec as BudgetRetrySpec
import qualified Test.CapabilityCorpusSpec as CapabilityCorpusSpec
import qualified Test.ChatSpec as ChatSpec
import qualified Test.CodePathSpec as CodePathSpec
import qualified Test.DiscoverSpec as DiscoverSpec
import qualified Test.GateResultSpec as GateResultSpec
import qualified Test.GradeRenderSpec as GradeRenderSpec
import qualified Test.GrammarSpec as GrammarSpec
import qualified Test.HealthGateSpec as HealthGateSpec
import qualified Test.HoleFitSpec as HoleFitSpec
import Test.Hspec (hspec)
import qualified Test.OllamaBodySpec as OllamaBodySpec
import qualified Test.PreflightSpec as PreflightSpec
import qualified Test.ProviderMapSpec as ProviderMapSpec
import qualified Test.ReasoningCorpusSpec as ReasoningCorpusSpec
import qualified Test.RepairBudgetSpec as RepairBudgetSpec
import qualified Test.RepairSpec as RepairSpec
import qualified Test.SalvageSpec as SalvageSpec
import qualified Test.SampleVerifySpec as SampleVerifySpec
import qualified Test.ScaffoldSpec as ScaffoldSpec
import qualified Test.SpecVerifierSpec as SpecVerifierSpec
import qualified Test.TaskSpec as TaskSpec
import qualified Test.ToolsSpec as ToolsSpec
import qualified Test.TranscriptSpec as TranscriptSpec

main :: IO ()
main = hspec $ do
    BenchSpec.spec
    BudgetRetrySpec.spec
    CapabilityCorpusSpec.spec
    ChatSpec.spec
    CodePathSpec.spec
    DiscoverSpec.spec
    GateResultSpec.spec
    GradeRenderSpec.spec
    GrammarSpec.spec
    HoleFitSpec.spec
    HealthGateSpec.spec
    OllamaBodySpec.spec
    ProviderMapSpec.spec
    PreflightSpec.spec
    ReasoningCorpusSpec.spec
    RepairBudgetSpec.spec
    RepairSpec.spec
    SalvageSpec.spec
    SampleVerifySpec.spec
    ScaffoldSpec.spec
    SpecVerifierSpec.spec
    TaskSpec.spec
    ToolsSpec.spec
    TranscriptSpec.spec
