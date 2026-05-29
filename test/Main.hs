module Main (main) where

import qualified Test.AiDocSpec as AiDocSpec
import qualified Test.AiHandlesSpec as AiHandlesSpec
import qualified Test.AiHistorySpec as AiHistorySpec
import qualified Test.AiRestSpec as AiRestSpec
import qualified Test.ApiWireSpec as ApiWireSpec
import qualified Test.CacheControlSpec as CacheControlSpec
import qualified Test.CompactResultSpec as CompactResultSpec
import qualified Test.CycleMsgSpec as CycleMsgSpec
import qualified Test.ExportSpec as ExportSpec
import qualified Test.GenerationSpec as GenerationSpec
import Test.Hspec (hspec)
import qualified Test.NotebookExportSpec as NotebookExportSpec
import qualified Test.OutputSpec as OutputSpec
import qualified Test.ParseSpec as ParseSpec
import qualified Test.PreinstalledSpec as PreinstalledSpec
import qualified Test.ProseRoundTripSpec as ProseRoundTripSpec
import qualified Test.ScratchpadRenderSpec as ScratchpadRenderSpec
import qualified Test.SessionSpec as SessionSpec
import qualified Test.ToolParseSpec as ToolParseSpec
import qualified Test.TopoSpec as TopoSpec
import qualified Test.UploadSpec as UploadSpec
import qualified Test.UsageEventSpec as UsageEventSpec
import qualified Test.UsageMergeSpec as UsageMergeSpec

main :: IO ()
main = hspec $ do
    SessionSpec.spec
    TopoSpec.spec
    ExportSpec.spec
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
    UsageEventSpec.spec
    UsageMergeSpec.spec
    ApiWireSpec.spec
