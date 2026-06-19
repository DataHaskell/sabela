module Main (main) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import qualified Test.AdmissionSpec as AdmissionSpec
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
import qualified Test.ConfigurableTimeoutSpec as ConfigurableTimeoutSpec
import qualified Test.CycleMsgSpec as CycleMsgSpec
import qualified Test.DefaultExtsSpec as DefaultExtsSpec
import qualified Test.DepsMatchSpec as DepsMatchSpec
import qualified Test.ExportGoldenSpec as ExportGoldenSpec
import qualified Test.ExportSpec as ExportSpec
import qualified Test.GenerationSpec as GenerationSpec
import Test.Hspec (hspec)
import qualified Test.InterruptTimestampFilterSpec as InterruptTimestampFilterSpec
import qualified Test.MarkerSpec as MarkerSpec
import qualified Test.NotebookAnimSpec as NotebookAnimSpec
import qualified Test.NotebookExportSpec as NotebookExportSpec
import qualified Test.NotebookFrpSpec as NotebookFrpSpec
import qualified Test.NotebookPictureSpec as NotebookPictureSpec
import qualified Test.OrphanSpec as OrphanSpec
import qualified Test.OutputSpec as OutputSpec
import qualified Test.ParseSpec as ParseSpec
import qualified Test.PlatformSpec as PlatformSpec
import qualified Test.PreinstalledSpec as PreinstalledSpec
import qualified Test.ProseRoundTripSpec as ProseRoundTripSpec
import qualified Test.QueryConcurrencySpec as QueryConcurrencySpec
import qualified Test.RenderSpec as RenderSpec
import qualified Test.ScratchpadRenderSpec as ScratchpadRenderSpec
import qualified Test.SessionGenSpec as SessionGenSpec
import qualified Test.SessionLiveSpec as SessionLiveSpec
import qualified Test.SessionLoopSpec as SessionLoopSpec
import qualified Test.SessionSpec as SessionSpec
import qualified Test.StaleRunSpec as StaleRunSpec
import qualified Test.TimeoutEscalationSpec as TimeoutEscalationSpec
import qualified Test.ToolParseSpec as ToolParseSpec
import qualified Test.TopoSpec as TopoSpec
import qualified Test.UploadSpec as UploadSpec
import qualified Test.UrlSpec as UrlSpec
import qualified Test.UsageEventSpec as UsageEventSpec
import qualified Test.UsageMergeSpec as UsageMergeSpec
import qualified Test.WidgetsSpec as WidgetsSpec

main :: IO ()
main = do
    setLocaleEncoding utf8
    hspec $ do
        NotebookFrpSpec.spec
        NotebookPictureSpec.spec
        NotebookAnimSpec.spec
        WidgetsSpec.spec
        SessionSpec.spec
        SessionLoopSpec.spec
        SessionLiveSpec.spec
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
        PlatformSpec.spec
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
        AdmissionSpec.spec
        MarkerSpec.spec
        RenderSpec.spec
        ExportGoldenSpec.spec
        ConfigurableTimeoutSpec.spec
        InterruptTimestampFilterSpec.spec
        QueryConcurrencySpec.spec
        OrphanSpec.spec
        TimeoutEscalationSpec.spec
        SessionGenSpec.spec
