module Test.AllSpecsB (allSpecsB) where

import qualified Test.AdmissionSpec as AdmissionSpec
import qualified Test.AiDocSpec as AiDocSpec
import qualified Test.AiHandlesSpec as AiHandlesSpec
import qualified Test.AiHistorySpec as AiHistorySpec
import qualified Test.AiRestSpec as AiRestSpec
import qualified Test.ApiRefSpec as ApiRefSpec
import qualified Test.ApiWireSpec as ApiWireSpec
import qualified Test.ArtefactSampleSpec as ArtefactSampleSpec
import qualified Test.ArtefactViewSpec as ArtefactViewSpec
import qualified Test.AtomicAdmissionSpec as AtomicAdmissionSpec
import qualified Test.AwaitIdleAgreementSpec as AwaitIdleAgreementSpec
import qualified Test.AwaitIdleSpec as AwaitIdleSpec
import qualified Test.BindingsLiveSpec as BindingsLiveSpec
import qualified Test.BuildBlameSpec as BuildBlameSpec
import qualified Test.BuiltinSearchLiveSpec as BuiltinSearchLiveSpec
import qualified Test.BusyWindowSpec as BusyWindowSpec
import qualified Test.CacheControlSpec as CacheControlSpec
import qualified Test.CellDefinesSpec as CellDefinesSpec
import qualified Test.CellResultWireSpec as CellResultWireSpec
import qualified Test.CellShapeSpec as CellShapeSpec
import qualified Test.CellWireSpec as CellWireSpec
import qualified Test.CheckTypeRouteSpec as CheckTypeRouteSpec
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
import qualified Test.FilesSpec as FilesSpec
import qualified Test.FitRuleSpec as FitRuleSpec
import qualified Test.GateCandidatesSpec as GateCandidatesSpec
import qualified Test.GateFrontierLiveSpec as GateFrontierLiveSpec
import qualified Test.GateFrontierSpec as GateFrontierSpec
import qualified Test.GateLocaliseSpec as GateLocaliseSpec
import qualified Test.GateRepairSpec as GateRepairSpec
import qualified Test.GenerationSpec as GenerationSpec
import qualified Test.GitHubSpec as GitHubSpec
import qualified Test.HarnessFrameSpec as HarnessFrameSpec
import qualified Test.HarnessHonestyLiveSpec as HarnessHonestyLiveSpec
import qualified Test.HintsSpec as HintsSpec
import qualified Test.HoleFitsJsonSpec as HoleFitsJsonSpec
import qualified Test.HoleNudgeSpec as HoleNudgeSpec
import qualified Test.HoleRewriteSpec as HoleRewriteSpec
import qualified Test.HoleRewriteTruthSpec as HoleRewriteTruthSpec
import Test.Hspec (Spec)
import qualified Test.InterruptTimestampFilterSpec as InterruptTimestampFilterSpec
import qualified Test.KernelErrorWireSpec as KernelErrorWireSpec
import qualified Test.KernelState.HolderSpec as KernelStateHolderSpec
import qualified Test.KernelStateIntegritySpec as KernelStateIntegritySpec
import qualified Test.KernelStateWireSpec as KernelStateWireSpec
import qualified Test.KernelVocabSpec as KernelVocabSpec
import Test.Live (liveSpecs)
import qualified Test.MarkerSpec as MarkerSpec
import qualified Test.NotebookExportSpec as NotebookExportSpec
import qualified Test.NotebookStateSpec as NotebookStateSpec
import qualified Test.OllamaParseSafetySpec as OllamaParseSafetySpec
import qualified Test.OrchestratorLoopSpec as OrchestratorLoopSpec
import qualified Test.OrphanSpec as OrphanSpec
import qualified Test.OutputChokepointWireSpec as OutputChokepointWireSpec
import qualified Test.OutputSpec as OutputSpec
import qualified Test.PackageIndexSpec as PackageIndexSpec
import qualified Test.ParquetSpec as ParquetSpec
import qualified Test.ParseSpec as ParseSpec
import qualified Test.PathFactsSpec as PathFactsSpec
import qualified Test.PathGateSpec as PathGateSpec
import qualified Test.PathRepairSpec as PathRepairSpec
import qualified Test.PeekArtefactSpec as PeekArtefactSpec
import qualified Test.PeekDataSpec as PeekDataSpec
import qualified Test.PeekVerdictSpec as PeekVerdictSpec
import qualified Test.PendingRefusalSpec as PendingRefusalSpec
import qualified Test.PlatformSpec as PlatformSpec
import qualified Test.PreinstalledSpec as PreinstalledSpec
import qualified Test.PromptBuiltinsSpec as PromptBuiltinsSpec
import qualified Test.ProseRoundTripSpec as ProseRoundTripSpec
import qualified Test.ProvenanceWireSpec as ProvenanceWireSpec
import qualified Test.ProviderAdapterSpec as ProviderAdapterSpec
import qualified Test.ProviderSelectSpec as ProviderSelectSpec
import qualified Test.PureEvalProtocolSpec as PureEvalProtocolSpec
import qualified Test.QualifiedSeamSpec as QualifiedSeamSpec
import qualified Test.QueryConcurrencySpec as QueryConcurrencySpec
import qualified Test.QueryDistillSpec as QueryDistillSpec
import qualified Test.QueryGuidanceSpec as QueryGuidanceSpec
import qualified Test.ReadCellFitsSpec as ReadCellFitsSpec
import qualified Test.ReadMissSpec as ReadMissSpec
import qualified Test.ReadSurfaceSpec as ReadSurfaceSpec
import qualified Test.RecordFieldDiscoverySpec as RecordFieldDiscoverySpec
import qualified Test.RenderSpec as RenderSpec
import qualified Test.RepairNeverWorseSpec as RepairNeverWorseSpec
import qualified Test.ReplProjectSpec as ReplProjectSpec
import qualified Test.ResolveSpec as ResolveSpec
import qualified Test.ResourceLiveSpec as ResourceLiveSpec
import qualified Test.ResourceSpec as ResourceSpec
import qualified Test.RestartModeSpec as RestartModeSpec
import qualified Test.RtsGhcOptionsSpec as RtsGhcOptionsSpec
import qualified Test.ScratchpadRenderSpec as ScratchpadRenderSpec
import qualified Test.ScratchpadSilenceSpec as ScratchpadSilenceSpec
import qualified Test.SessionFactsSpec as SessionFactsSpec
import qualified Test.SessionGenSpec as SessionGenSpec
import qualified Test.SizaContractWireSpec as SizaContractWireSpec
import qualified Test.SourceNormalizeSpec as SourceNormalizeSpec
import qualified Test.StderrFailureSpec as StderrFailureSpec
import qualified Test.TimeoutEscalationSpec as TimeoutEscalationSpec
import qualified Test.ToolOutcomeWireSpec as ToolOutcomeWireSpec
import qualified Test.ToolParseSpec as ToolParseSpec
import qualified Test.UnshowableValueSpec as UnshowableValueSpec
import qualified Test.UploadSpec as UploadSpec
import qualified Test.UrlSpec as UrlSpec
import qualified Test.UsageEventSpec as UsageEventSpec
import qualified Test.UsageMergeSpec as UsageMergeSpec
import qualified Test.VerdictSpec as VerdictSpec
import qualified Test.WorkspaceRecoverySpec as WorkspaceRecoverySpec
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
    PackageIndexSpec.spec
    PathRepairSpec.spec
    PathGateSpec.spec
    FilesSpec.spec
    GitHubSpec.spec
    GateRepairSpec.spec
    GateFrontierSpec.spec
    GateLocaliseSpec.spec
    GateCandidatesSpec.spec
    UnshowableValueSpec.spec
    HarnessFrameSpec.spec
    PureEvalProtocolSpec.spec
    HoleFitsJsonSpec.spec
    FitRuleSpec.spec
    ReadCellFitsSpec.spec
    HoleNudgeSpec.spec
    HoleRewriteSpec.spec
    HoleRewriteTruthSpec.spec
    RecordFieldDiscoverySpec.spec
    HintsSpec.spec
    StderrFailureSpec.spec
    ParquetSpec.spec
    PeekDataSpec.spec
    PeekVerdictSpec.spec
    ArtefactViewSpec.spec
    ArtefactSampleSpec.spec
    PeekArtefactSpec.spec
    ReadMissSpec.spec
    ReadSurfaceSpec.spec
    PathFactsSpec.spec
    CellShapeSpec.spec
    WriteBoundarySpec.spec
    PendingRefusalSpec.spec
    CellDefinesSpec.spec
    CheckTypeStructSpec.spec
    CheckTypeRouteSpec.spec
    QualifiedSeamSpec.spec
    SessionFactsSpec.spec
    QueryDistillSpec.spec
    PlatformSpec.spec
    OutputSpec.spec
    ProseRoundTripSpec.spec
    AiDocSpec.spec
    AiHandlesSpec.spec
    AiHistorySpec.spec
    AiRestSpec.spec
    CacheControlSpec.spec
    ProviderAdapterSpec.spec
    ConfigWireSpec.spec
    OllamaParseSafetySpec.spec
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
    KernelVocabSpec.spec
    VerdictSpec.spec
    BindingsLiveSpec.spec
    PromptBuiltinsSpec.spec
    BusyWindowSpec.spec
    ResourceSpec.spec
    RtsGhcOptionsSpec.spec
    WorkspaceRecoverySpec.spec
    CellWireSpec.spec
    NotebookStateSpec.spec
    WriteAckRetrySpec.spec
    WriteAckShapeSpec.spec
    KernelStateHolderSpec.spec
    BuildBlameSpec.spec
    RepairNeverWorseSpec.spec
    RestartModeSpec.spec
    EvCellResultWireSpec.spec
    KernelErrorWireSpec.spec
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
    liveSpecs $ do
        GateFrontierLiveSpec.spec
        HarnessHonestyLiveSpec.spec
        PreinstalledSpec.spec
        GenerationSpec.spec
        OrchestratorLoopSpec.spec
        ProviderSelectSpec.spec
        AtomicAdmissionSpec.spec
        DiscoveryToolsPreSessionSpec.spec
        AwaitIdleSpec.spec
        BuiltinSearchLiveSpec.spec
        AwaitIdleAgreementSpec.spec
        ResourceLiveSpec.spec
        WriteAckSpec.spec
        WriteAckLiveSpec.spec
        KernelStateWireSpec.spec
        KernelStateIntegritySpec.spec
        ToolOutcomeWireSpec.spec
        ProvenanceWireSpec.spec
        SessionGenSpec.spec
        SourceNormalizeSpec.spec
