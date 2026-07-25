module Test.AllSpecsB (allSpecsB) where

import Test.Hspec (Spec)
import qualified Test.AdmissionSpec as AdmissionSpec
import qualified Test.AiDocSpec as AiDocSpec
import qualified Test.AiHandlesSpec as AiHandlesSpec
import qualified Test.AiHistorySpec as AiHistorySpec
import qualified Test.AiRestSpec as AiRestSpec
import qualified Test.ApiRefSpec as ApiRefSpec
import qualified Test.ApiWireSpec as ApiWireSpec
import qualified Test.AtomicAdmissionSpec as AtomicAdmissionSpec
import qualified Test.AwaitIdleSpec as AwaitIdleSpec
import qualified Test.BindingsLiveSpec as BindingsLiveSpec
import qualified Test.BuiltinSearchLiveSpec as BuiltinSearchLiveSpec
import qualified Test.AwaitIdleAgreementSpec as AwaitIdleAgreementSpec
import qualified Test.PromptBuiltinsSpec as PromptBuiltinsSpec
import qualified Test.BusyWindowSpec as BusyWindowSpec
import qualified Test.CacheControlSpec as CacheControlSpec
import qualified Test.CellDefinesSpec as CellDefinesSpec
import qualified Test.CellResultWireSpec as CellResultWireSpec
import qualified Test.CellShapeSpec as CellShapeSpec
import qualified Test.CheckTypeStructSpec as CheckTypeStructSpec
import qualified Test.CompactResultSpec as CompactResultSpec
import qualified Test.ConfigWireSpec as ConfigWireSpec
import qualified Test.ConfigurableTimeoutSpec as ConfigurableTimeoutSpec
import qualified Test.CycleMsgSpec as CycleMsgSpec
import qualified Test.DeclaredSpec as DeclaredSpec
import qualified Test.DepsMatchSpec as DepsMatchSpec
import qualified Test.DepsRepairSpec as DepsRepairSpec
import qualified Test.DiscoveryToolsPreSessionSpec as DiscoveryToolsPreSessionSpec
import qualified Test.EvCellResultWireSpec as EvCellResultWireSpec
import qualified Test.ExportGoldenSpec as ExportGoldenSpec
import qualified Test.GenerationSpec as GenerationSpec
import qualified Test.InterruptTimestampFilterSpec as InterruptTimestampFilterSpec
import qualified Test.KernelStateIntegritySpec as KernelStateIntegritySpec
import qualified Test.KernelStateWireSpec as KernelStateWireSpec
import qualified Test.KernelVocabSpec as KernelVocabSpec
import qualified Test.MarkerSpec as MarkerSpec
import qualified Test.NotebookExportSpec as NotebookExportSpec
import qualified Test.OllamaParseSafetySpec as OllamaParseSafetySpec
import qualified Test.OrchestratorLoopSpec as OrchestratorLoopSpec
import qualified Test.OrphanSpec as OrphanSpec
import qualified Test.OutputChokepointWireSpec as OutputChokepointWireSpec
import qualified Test.OutputSpec as OutputSpec
import qualified Test.ParseSpec as ParseSpec
import qualified Test.PathRepairSpec as PathRepairSpec
import qualified Test.PeekDataSpec as PeekDataSpec
import qualified Test.PendingRefusalSpec as PendingRefusalSpec
import qualified Test.PlatformSpec as PlatformSpec
import qualified Test.PreinstalledSpec as PreinstalledSpec
import qualified Test.ProseRoundTripSpec as ProseRoundTripSpec
import qualified Test.ProvenanceWireSpec as ProvenanceWireSpec
import qualified Test.ProviderAdapterSpec as ProviderAdapterSpec
import qualified Test.ProviderSelectSpec as ProviderSelectSpec
import qualified Test.QueryConcurrencySpec as QueryConcurrencySpec
import qualified Test.QueryDistillSpec as QueryDistillSpec
import qualified Test.QueryGuidanceSpec as QueryGuidanceSpec
import qualified Test.RenderSpec as RenderSpec
import qualified Test.ReplProjectSpec as ReplProjectSpec
import qualified Test.ResolveSpec as ResolveSpec
import qualified Test.ResourceLiveSpec as ResourceLiveSpec
import qualified Test.ResourceSpec as ResourceSpec
import qualified Test.RtsGhcOptionsSpec as RtsGhcOptionsSpec
import qualified Test.ScratchpadRenderSpec as ScratchpadRenderSpec
import qualified Test.ScratchpadSilenceSpec as ScratchpadSilenceSpec
import qualified Test.SessionGenSpec as SessionGenSpec
import qualified Test.SizaContractWireSpec as SizaContractWireSpec
import qualified Test.SourceNormalizeSpec as SourceNormalizeSpec
import qualified Test.TimeoutEscalationSpec as TimeoutEscalationSpec
import qualified Test.ToolOutcomeWireSpec as ToolOutcomeWireSpec
import qualified Test.ToolParseSpec as ToolParseSpec
import qualified Test.UploadSpec as UploadSpec
import qualified Test.UrlSpec as UrlSpec
import qualified Test.UsageEventSpec as UsageEventSpec
import qualified Test.UsageMergeSpec as UsageMergeSpec
import qualified Test.VerdictSpec as VerdictSpec
import qualified Test.WriteAckLiveSpec as WriteAckLiveSpec
import qualified Test.WriteAckRetrySpec as WriteAckRetrySpec
import qualified Test.WriteAckShapeSpec as WriteAckShapeSpec
import qualified Test.WriteAckSpec as WriteAckSpec
import qualified Test.WriteBoundarySpec as WriteBoundarySpec

allSpecsB :: Spec
allSpecsB = do
        DepsMatchSpec.spec
        DepsRepairSpec.depsRepairSpec
        NotebookExportSpec.spec
        ParseSpec.spec
        DeclaredSpec.spec
        PathRepairSpec.spec
        PeekDataSpec.spec
        CellShapeSpec.spec
        WriteBoundarySpec.spec
        PendingRefusalSpec.spec
        CellDefinesSpec.spec
        CheckTypeStructSpec.spec
        QueryDistillSpec.spec
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
        OllamaParseSafetySpec.spec
        ProviderSelectSpec.spec
        ApiRefSpec.spec
        CompactResultSpec.spec
        OutputChokepointWireSpec.spec
        CycleMsgSpec.spec
        ScratchpadRenderSpec.spec
        ScratchpadSilenceSpec.spec
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
        KernelVocabSpec.spec
        VerdictSpec.spec
        BindingsLiveSpec.spec
        BuiltinSearchLiveSpec.spec
        PromptBuiltinsSpec.spec
        AwaitIdleAgreementSpec.spec
        BusyWindowSpec.spec
        ResourceSpec.spec
        RtsGhcOptionsSpec.spec
        ResourceLiveSpec.spec
        WriteAckSpec.spec
        WriteAckRetrySpec.spec
        WriteAckShapeSpec.spec
        WriteAckLiveSpec.spec
        KernelStateWireSpec.spec
        KernelStateIntegritySpec.spec
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
        SourceNormalizeSpec.spec
