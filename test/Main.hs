module Main (main) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import qualified Test.AdmissionSpec as AdmissionSpec
import qualified Test.AiDocSpec as AiDocSpec
import qualified Test.AiHandlesSpec as AiHandlesSpec
import qualified Test.AiHistorySpec as AiHistorySpec
import qualified Test.AiRestSpec as AiRestSpec
import qualified Test.ApiRefSpec as ApiRefSpec
import qualified Test.ApiWireSpec as ApiWireSpec
import qualified Test.AtomicAdmissionSpec as AtomicAdmissionSpec
import qualified Test.AwaitIdleSpec as AwaitIdleSpec
import qualified Test.CacheControlSpec as CacheControlSpec
import qualified Test.CapabilityApiSpec as CapabilityApiSpec
import qualified Test.CapabilitySearchSpec as CapabilitySearchSpec
import qualified Test.CapabilitySpec as CapabilitySpec
import qualified Test.CellDefinesSpec as CellDefinesSpec
import qualified Test.CellResultWireSpec as CellResultWireSpec
import qualified Test.CellShapeSpec as CellShapeSpec
import qualified Test.CheckTypeStructSpec as CheckTypeStructSpec
import qualified Test.ClassifyErrorSpec as ClassifyErrorSpec
import qualified Test.CompactResultSpec as CompactResultSpec
import qualified Test.CompileEscalationSpec as CompileEscalationSpec
import qualified Test.CompiledPlanSpec as CompiledPlanSpec
import qualified Test.ConfigWireSpec as ConfigWireSpec
import qualified Test.ConfigurableTimeoutSpec as ConfigurableTimeoutSpec
import qualified Test.CycleMsgSpec as CycleMsgSpec
import qualified Test.DefaultExtsSpec as DefaultExtsSpec
import qualified Test.DepRepairSpec as DepRepairSpec
import qualified Test.DepsMatchSpec as DepsMatchSpec
import qualified Test.DepsRepairSpec as DepsRepairSpec
import qualified Test.DiagnoseSpec as DiagnoseSpec
import qualified Test.DiscoverGrammarSpec as DiscoverGrammarSpec
import qualified Test.DiscoverToolSpec as DiscoverToolSpec
import qualified Test.DiscoveryToolsPreSessionSpec as DiscoveryToolsPreSessionSpec
import qualified Test.ErrorsJsonSpec as ErrorsJsonSpec
import qualified Test.EvCellResultWireSpec as EvCellResultWireSpec
import qualified Test.ExampleSearchSpec as ExampleSearchSpec
import qualified Test.ExecuteCellSpec as ExecuteCellSpec
import qualified Test.ExportGoldenSpec as ExportGoldenSpec
import qualified Test.ExportSpec as ExportSpec
import qualified Test.ExtRepairSpec as ExtRepairSpec
import qualified Test.GenerationSpec as GenerationSpec
import qualified Test.GrammarRouteSpec as GrammarRouteSpec
import qualified Test.HealthSpec as HealthSpec
import qualified Test.HoleFitsSpec as HoleFitsSpec
import qualified Test.HoogleResolveSpec as HoogleResolveSpec
import Test.Hspec (hspec)
import qualified Test.ImportRepairSpec as ImportRepairSpec
import qualified Test.InterruptTimestampFilterSpec as InterruptTimestampFilterSpec
import qualified Test.JsonDiagSpec as JsonDiagSpec
import qualified Test.KernelStateWireSpec as KernelStateWireSpec
import qualified Test.LibDiscoverSpec as LibDiscoverSpec
import qualified Test.MarkerSpec as MarkerSpec
import qualified Test.ModuleResolveSpec as ModuleResolveSpec
import qualified Test.NotebookAnimSpec as NotebookAnimSpec
import qualified Test.NotebookEditSpec as NotebookEditSpec
import qualified Test.NotebookExportSpec as NotebookExportSpec
import qualified Test.NotebookFrpSpec as NotebookFrpSpec
import qualified Test.NotebookPictureSpec as NotebookPictureSpec
import qualified Test.NotebookViolationSpec as NotebookViolationSpec
import qualified Test.OrchestratorLoopSpec as OrchestratorLoopSpec
import qualified Test.OrphanSpec as OrphanSpec
import qualified Test.OutputChokepointWireSpec as OutputChokepointWireSpec
import qualified Test.OutputSpec as OutputSpec
import qualified Test.OwnedSpec as OwnedSpec
import qualified Test.ParseSpec as ParseSpec
import qualified Test.PeekDataSpec as PeekDataSpec
import qualified Test.PlatformSpec as PlatformSpec
import qualified Test.PreinstalledSpec as PreinstalledSpec
import qualified Test.PromptUnifySpec as PromptUnifySpec
import qualified Test.ProseRoundTripSpec as ProseRoundTripSpec
import qualified Test.ProvenanceWireSpec as ProvenanceWireSpec
import qualified Test.ProviderAdapterSpec as ProviderAdapterSpec
import qualified Test.ProviderSelectSpec as ProviderSelectSpec
import qualified Test.QueryConcurrencySpec as QueryConcurrencySpec
import qualified Test.QueryGuidanceSpec as QueryGuidanceSpec
import qualified Test.RenderSpec as RenderSpec
import qualified Test.RepairEngineSpec as RepairEngineSpec
import qualified Test.RepairTraceWireSpec as RepairTraceWireSpec
import qualified Test.ReplProjectSpec as ReplProjectSpec
import qualified Test.ResolveSpec as ResolveSpec
import qualified Test.ScratchpadRenderSpec as ScratchpadRenderSpec
import qualified Test.SessionGenSpec as SessionGenSpec
import qualified Test.SessionLiveSpec as SessionLiveSpec
import qualified Test.SessionLoopSpec as SessionLoopSpec
import qualified Test.SessionSpec as SessionSpec
import qualified Test.SizaContractWireSpec as SizaContractWireSpec
import qualified Test.SpineSpec as SpineSpec
import qualified Test.StaleRunSpec as StaleRunSpec
import qualified Test.SwitchNotebookSpec as SwitchNotebookSpec
import qualified Test.TimeoutEscalationSpec as TimeoutEscalationSpec
import qualified Test.ToolInputRewriteSpec as ToolInputRewriteSpec
import qualified Test.ToolOutcomeWireSpec as ToolOutcomeWireSpec
import qualified Test.ToolParseSpec as ToolParseSpec
import qualified Test.TopoSpec as TopoSpec
import qualified Test.UploadSpec as UploadSpec
import qualified Test.UrlSpec as UrlSpec
import qualified Test.UsageEventSpec as UsageEventSpec
import qualified Test.UsageMergeSpec as UsageMergeSpec
import qualified Test.VerifyDownstreamSpec as VerifyDownstreamSpec
import qualified Test.WidgetsSpec as WidgetsSpec

main :: IO ()
main = do
    setLocaleEncoding utf8
    hspec $ do
        NotebookFrpSpec.spec
        NotebookEditSpec.spec
        NotebookPictureSpec.spec
        NotebookAnimSpec.spec
        WidgetsSpec.spec
        SessionSpec.spec
        SessionLoopSpec.spec
        SessionLiveSpec.spec
        StaleRunSpec.spec
        SwitchNotebookSpec.spec
        TopoSpec.spec
        CompiledPlanSpec.spec
        CompileEscalationSpec.spec
        ClassifyErrorSpec.spec
        ErrorsJsonSpec.errorsJsonSpec
        JsonDiagSpec.jsonDiagSpec
        ExecuteCellSpec.spec
        ExampleSearchSpec.spec
        DiagnoseSpec.diagnoseSpec
        SpineSpec.spineSpec
        ModuleResolveSpec.spec
        DiscoverToolSpec.spec
        DepRepairSpec.spec
        ExtRepairSpec.spec
        ImportRepairSpec.spec
        HealthSpec.spec
        OwnedSpec.spec
        RepairEngineSpec.spec
        RepairTraceWireSpec.spec
        VerifyDownstreamSpec.spec
        ToolInputRewriteSpec.spec
        PromptUnifySpec.spec
        DiscoverGrammarSpec.spec
        HoogleResolveSpec.spec
        CapabilitySearchSpec.spec
        CapabilityApiSpec.spec
        CapabilitySpec.spec
        NotebookViolationSpec.spec
        GrammarRouteSpec.grammarRouteSpec
        HoleFitsSpec.spec
        LibDiscoverSpec.spec
        ExportSpec.spec
        DefaultExtsSpec.spec
        DepsMatchSpec.spec
        DepsRepairSpec.depsRepairSpec
        NotebookExportSpec.spec
        ParseSpec.spec
        PeekDataSpec.spec
        CellShapeSpec.spec
        CellDefinesSpec.spec
        CheckTypeStructSpec.spec
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
        ProviderAdapterSpec.spec
        ConfigWireSpec.spec
        OrchestratorLoopSpec.spec
        ProviderSelectSpec.spec
        ApiRefSpec.spec
        CompactResultSpec.spec
        OutputChokepointWireSpec.spec
        CycleMsgSpec.spec
        ScratchpadRenderSpec.spec
        ToolParseSpec.spec
        UploadSpec.spec
        UrlSpec.spec
        UsageEventSpec.spec
        UsageMergeSpec.spec
        ApiWireSpec.spec
        SizaContractWireSpec.spec
        CellResultWireSpec.spec
        AdmissionSpec.spec
        AtomicAdmissionSpec.spec
        DiscoveryToolsPreSessionSpec.spec
        AwaitIdleSpec.spec
        KernelStateWireSpec.spec
        EvCellResultWireSpec.spec
        ToolOutcomeWireSpec.spec
        ProvenanceWireSpec.spec
        MarkerSpec.spec
        RenderSpec.spec
        ResolveSpec.spec
        ReplProjectSpec.spec
        ExportGoldenSpec.spec
        ConfigurableTimeoutSpec.spec
        InterruptTimestampFilterSpec.spec
        QueryConcurrencySpec.spec
        QueryGuidanceSpec.spec
        OrphanSpec.spec
        TimeoutEscalationSpec.spec
        SessionGenSpec.spec
