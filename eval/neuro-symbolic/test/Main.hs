module Main (main) where

import qualified Test.BenchSpec as BenchSpec
import qualified Test.ChatSpec as ChatSpec
import qualified Test.DiscoverSpec as DiscoverSpec
import qualified Test.GradeRenderSpec as GradeRenderSpec
import qualified Test.GrammarSpec as GrammarSpec
import qualified Test.HealthGateSpec as HealthGateSpec
import qualified Test.HoleFitSpec as HoleFitSpec
import Test.Hspec (hspec)
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
    ChatSpec.spec
    DiscoverSpec.spec
    GradeRenderSpec.spec
    GrammarSpec.spec
    HoleFitSpec.spec
    HealthGateSpec.spec
    RepairBudgetSpec.spec
    RepairSpec.spec
    SalvageSpec.spec
    SampleVerifySpec.spec
    ScaffoldSpec.spec
    SpecVerifierSpec.spec
    TaskSpec.spec
    ToolsSpec.spec
    TranscriptSpec.spec
