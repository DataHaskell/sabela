module Main (main) where

import qualified Test.AiDocSpec as AiDocSpec
import qualified Test.AiHandlesSpec as AiHandlesSpec
import qualified Test.AiHistorySpec as AiHistorySpec
import qualified Test.AiRestSpec as AiRestSpec
import qualified Test.ApiWireSpec as ApiWireSpec
import qualified Test.CacheControlSpec as CacheControlSpec
import qualified Test.ClassifyErrorSpec as ClassifyErrorSpec
import qualified Test.CompactResultSpec as CompactResultSpec
import qualified Test.CompileEscalationSpec as CompileEscalationSpec
import qualified Test.CompiledPlanSpec as CompiledPlanSpec
import qualified Test.CycleMsgSpec as CycleMsgSpec
import qualified Test.DefaultExtsSpec as DefaultExtsSpec
import qualified Test.DepsMatchSpec as DepsMatchSpec
import qualified Test.ExportSpec as ExportSpec
import qualified Test.GenerationSpec as GenerationSpec
import Test.Hspec (hspec)
import qualified Test.NotebookExportSpec as NotebookExportSpec
import qualified Test.OutputSpec as OutputSpec
import qualified Test.ParseSpec as ParseSpec
import qualified Test.PreinstalledSpec as PreinstalledSpec
import qualified Test.ProseRoundTripSpec as ProseRoundTripSpec
import qualified Test.ScratchpadRenderSpec as ScratchpadRenderSpec
import qualified Test.SessionLoopSpec as SessionLoopSpec
import qualified Test.SessionSpec as SessionSpec
import qualified Test.StaleRunSpec as StaleRunSpec
import qualified Test.ToolParseSpec as ToolParseSpec
import qualified Test.TopoSpec as TopoSpec
import qualified Test.UploadSpec as UploadSpec
import qualified Test.UrlSpec as UrlSpec
import qualified Test.UsageEventSpec as UsageEventSpec
import qualified Test.UsageMergeSpec as UsageMergeSpec

main :: IO ()
main = hspec $ do
    SessionSpec.spec
    SessionLoopSpec.spec
    StaleRunSpec.spec
    TopoSpec.spec
    CompiledPlanSpec.spec
    CompileEscalationSpec.spec
    ClassifyErrorSpec.spec
    ExportSpec.spec
    DefaultExtsSpec.spec
    DepsMatchSpec.spec
    NotebookExportSpec.spec
    ParseSpec.spec
    OutputSpec.spec
    PreinstalledSpec.spec
    ProseRoundTripSpec.spec
    GenerationSpec.spec
    AiDocSpec.spec
    AiHandlesSpec.spec
    AiHistorySpec.spec
    AiRestSpec.spec
    CacheControlSpec.spec
    CompactResultSpec.spec
    CycleMsgSpec.spec
    ScratchpadRenderSpec.spec
    ToolParseSpec.spec
    UploadSpec.spec
    UrlSpec.spec
    UsageEventSpec.spec
    UsageMergeSpec.spec
    ApiWireSpec.spec
