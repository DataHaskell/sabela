module Test.AllSpecsA (allSpecsA) where

import qualified Test.AdviceTruthSpec as AdviceTruthSpec
import qualified Test.ArgRepairSpec as ArgRepairSpec
import qualified Test.BridgeGraphSpec as BridgeGraphSpec
import qualified Test.BrowseCardParseSpec as BrowseCardParseSpec
import qualified Test.BrowseCardSanitizeSpec as BrowseCardSanitizeSpec
import qualified Test.BrowseCardSpec as BrowseCardSpec
import qualified Test.CapabilityApiSpec as CapabilityApiSpec
import qualified Test.CapabilitySearchSpec as CapabilitySearchSpec
import qualified Test.CapabilitySpec as CapabilitySpec
import qualified Test.CheckTypeScopeSpec as CheckTypeScopeSpec
import qualified Test.ClassifyErrorSpec as ClassifyErrorSpec
import qualified Test.CompileEscalationSpec as CompileEscalationSpec
import qualified Test.CompileGateRenderSpec as CompileGateRenderSpec
import qualified Test.CompileGateSpec as CompileGateSpec
import qualified Test.CompileGateWireSpec as CompileGateWireSpec
import qualified Test.CompiledPlanSpec as CompiledPlanSpec
import qualified Test.DefaultExtsSpec as DefaultExtsSpec
import qualified Test.DeferredModeSpec as DeferredModeSpec
import qualified Test.DepRepairSpec as DepRepairSpec
import qualified Test.DiagnoseAdviceSpec as DiagnoseAdviceSpec
import qualified Test.DiagnoseSpec as DiagnoseSpec
import qualified Test.DiagnosticMitigationCompoundSpec as DiagnosticMitigationCompoundSpec
import qualified Test.DiagnosticMitigationLiveSpec as DiagnosticMitigationLiveSpec
import qualified Test.DiagnosticMitigationSpec as DiagnosticMitigationSpec
import qualified Test.DiscoverGrammarSpec as DiscoverGrammarSpec
import qualified Test.DiscoverToolSpec as DiscoverToolSpec
import qualified Test.DiscoveryBenchSpec as DiscoveryBenchSpec
import qualified Test.DisposableAttributionSpec as DisposableAttributionSpec
import qualified Test.EditDispatchSpec as EditDispatchSpec
import qualified Test.EditNoOpSpec as EditNoOpSpec
import qualified Test.EditSignificanceSpec as EditSignificanceSpec
import qualified Test.EnvStaleSpec as EnvStaleSpec
import qualified Test.EnvironmentFaultSpec as EnvironmentFaultSpec
import qualified Test.ErrorIndexSpec as ErrorIndexSpec
import qualified Test.ErrorsJsonSpec as ErrorsJsonSpec
import qualified Test.ExampleSearchSpec as ExampleSearchSpec
import qualified Test.ExecuteCellSpec as ExecuteCellSpec
import qualified Test.ExportRankSpec as ExportRankSpec
import qualified Test.ExportSpec as ExportSpec
import qualified Test.ExtRepairSpec as ExtRepairSpec
import qualified Test.GateArmingSpec as GateArmingSpec
import qualified Test.GateBinderGuardSpec as GateBinderGuardSpec
import qualified Test.GateCheckedSpec as GateCheckedSpec
import qualified Test.GateDefaultingSpec as GateDefaultingSpec
import qualified Test.GateErrorClassSpec as GateErrorClassSpec
import qualified Test.GatePartialRepairSpec as GatePartialRepairSpec
import qualified Test.GateSourceTruthSpec as GateSourceTruthSpec
import qualified Test.GateStageWireSpec as GateStageWireSpec
import qualified Test.GrammarRouteSpec as GrammarRouteSpec
import qualified Test.GraphEdgesSpec as GraphEdgesSpec
import qualified Test.HarnessBinderSpec as HarnessBinderSpec
import qualified Test.HealthSpec as HealthSpec
import qualified Test.HoleFitsSpec as HoleFitsSpec
import qualified Test.HoleProbeSpec as HoleProbeSpec
import qualified Test.HoogleDbSpec as HoogleDbSpec
import qualified Test.HoogleProseSpec as HoogleProseSpec
import qualified Test.HoogleRankSpec as HoogleRankSpec
import qualified Test.HoogleResolveSpec as HoogleResolveSpec
import Test.Hspec (Spec)
import qualified Test.ImportRepairSpec as ImportRepairSpec
import qualified Test.IndexAnswerSpec as IndexAnswerSpec
import qualified Test.InsertGateRouteSpec as InsertGateRouteSpec
import qualified Test.InsertSupersedeSpec as InsertSupersedeSpec
import qualified Test.JsonDiagSpec as JsonDiagSpec
import Test.Live (liveSpecs)
import qualified Test.MaterializeSpec as MaterializeSpec
import qualified Test.ModuleCardRankSpec as ModuleCardRankSpec
import qualified Test.ModuleCardTruthSpec as ModuleCardTruthSpec
import qualified Test.ModuleResolveSpec as ModuleResolveSpec
import qualified Test.NonThreadedForkSpec as NonThreadedForkSpec
import qualified Test.NormalizeGateSpec as NormalizeGateSpec
import qualified Test.NormalizeProposalsSpec as NormalizeProposalsSpec
import qualified Test.NotebookAnimSpec as NotebookAnimSpec
import qualified Test.NotebookCheckSpec as NotebookCheckSpec
import qualified Test.NotebookEditSpec as NotebookEditSpec
import qualified Test.NotebookFrpSpec as NotebookFrpSpec
import qualified Test.NotebookPictureSpec as NotebookPictureSpec
import qualified Test.NotebookViolationSpec as NotebookViolationSpec
import qualified Test.OrphanModuleReconcileSpec as OrphanModuleReconcileSpec
import qualified Test.OwnedSpec as OwnedSpec
import qualified Test.PreludeGhciSpec as PreludeGhciSpec
import qualified Test.PreludeScopeSpec as PreludeScopeSpec
import qualified Test.PromptUnifySpec as PromptUnifySpec
import qualified Test.PureEvalLiveSpec as PureEvalLiveSpec
import qualified Test.QualifiedNameSpec as QualifiedNameSpec
import qualified Test.ReactivityDoorSpec as ReactivityDoorSpec
import qualified Test.RefinementFitSpec as RefinementFitSpec
import qualified Test.RepairEngineSpec as RepairEngineSpec
import qualified Test.RepairGateSpec as RepairGateSpec
import qualified Test.RepairGuardSpec as RepairGuardSpec
import qualified Test.RepairTraceWireSpec as RepairTraceWireSpec
import qualified Test.RunModeWireSpec as RunModeWireSpec
import qualified Test.ScratchScopeSpec as ScratchScopeSpec
import qualified Test.ScratchVetSpec as ScratchVetSpec
import qualified Test.ScratchpadKeySpec as ScratchpadKeySpec
import qualified Test.SearchCacheFreshSpec as SearchCacheFreshSpec
import qualified Test.SelfHealSpec as SelfHealSpec
import qualified Test.SessionLiveSpec as SessionLiveSpec
import qualified Test.SessionLoopSpec as SessionLoopSpec
import qualified Test.SessionResetSpec as SessionResetSpec
import qualified Test.SessionSpec as SessionSpec
import qualified Test.SkipFeedbackSpec as SkipFeedbackSpec
import qualified Test.SpineSpec as SpineSpec
import qualified Test.StaleRunSpec as StaleRunSpec
import qualified Test.SubmissionSpec as SubmissionSpec
import qualified Test.SwitchNotebookSpec as SwitchNotebookSpec
import qualified Test.ThrowawayExecuteSpec as ThrowawayExecuteSpec
import qualified Test.ToolInputRewriteSpec as ToolInputRewriteSpec
import qualified Test.TopoSpec as TopoSpec
import qualified Test.TriageSpec as TriageSpec
import qualified Test.TrialWarningSpec as TrialWarningSpec
import qualified Test.TryCacheSpec as TryCacheSpec
import qualified Test.TryContainedSeamSpec as TryContainedSeamSpec
import qualified Test.TryFrontierSpec as TryFrontierSpec
import qualified Test.TryHoleProbeSpec as TryHoleProbeSpec
import qualified Test.TryLiveRouteSpec as TryLiveRouteSpec
import qualified Test.TryOutcomeWireSpec as TryOutcomeWireSpec
import qualified Test.TryPlanSpec as TryPlanSpec
import qualified Test.TrySpec as TrySpec
import qualified Test.TypeDirectedResolveSpec as TypeDirectedResolveSpec
import qualified Test.TypeDiscoverySpec as TypeDiscoverySpec
import qualified Test.TypecheckClassifySpec as TypecheckClassifySpec
import qualified Test.TypecheckPrimitiveSpec as TypecheckPrimitiveSpec
import qualified Test.UnshowableSpec as UnshowableSpec
import qualified Test.ValueEchoSpec as ValueEchoSpec
import qualified Test.ValueSynopsisSpec as ValueSynopsisSpec
import qualified Test.VerifyDownstreamSpec as VerifyDownstreamSpec
import qualified Test.WidgetsSpec as WidgetsSpec

allSpecsA :: Spec
allSpecsA = do
    NotebookFrpSpec.spec
    NotebookEditSpec.spec
    NotebookPictureSpec.spec
    NotebookAnimSpec.spec
    NotebookCheckSpec.spec
    WidgetsSpec.spec
    StaleRunSpec.spec
    TopoSpec.spec
    CompiledPlanSpec.spec
    CompileGateRenderSpec.spec
    CompileGateWireSpec.spec
    GateStageWireSpec.spec
    GateBinderGuardSpec.spec
    GateErrorClassSpec.spec
    InsertGateRouteSpec.spec
    GateDefaultingSpec.spec
    GateSourceTruthSpec.spec
    GatePartialRepairSpec.spec
    GateArmingSpec.spec
    GateCheckedSpec.spec
    DisposableAttributionSpec.spec
    EnvironmentFaultSpec.spec
    TrialWarningSpec.spec
    SubmissionSpec.spec
    ClassifyErrorSpec.spec
    AdviceTruthSpec.spec
    HarnessBinderSpec.spec
    ErrorsJsonSpec.errorsJsonSpec
    ErrorIndexSpec.spec
    JsonDiagSpec.jsonDiagSpec
    ExecuteCellSpec.spec
    ExampleSearchSpec.spec
    DiagnoseSpec.diagnoseSpec
    DiagnoseAdviceSpec.diagnoseAdviceSpec
    DiagnosticMitigationSpec.spec
    SpineSpec.spineSpec
    ModuleResolveSpec.spec
    ModuleCardTruthSpec.spec
    ModuleCardRankSpec.spec
    DiscoverToolSpec.spec
    DepRepairSpec.spec
    ExtRepairSpec.spec
    ImportRepairSpec.spec
    RepairGuardSpec.spec
    HealthSpec.spec
    OwnedSpec.spec
    RepairEngineSpec.spec
    RepairTraceWireSpec.spec
    VerifyDownstreamSpec.spec
    ToolInputRewriteSpec.spec
    PromptUnifySpec.spec
    DiscoverGrammarSpec.spec
    DiscoveryBenchSpec.spec
    ExportRankSpec.spec
    HoogleProseSpec.spec
    HoogleRankSpec.spec
    HoogleResolveSpec.spec
    TypeDirectedResolveSpec.spec
    TypeDiscoverySpec.spec
    TryPlanSpec.spec
    TryFrontierSpec.spec
    TryLiveRouteSpec.spec
    TryContainedSeamSpec.spec
    TryCacheSpec.spec
    ThrowawayExecuteSpec.spec
    NonThreadedForkSpec.spec
    SelfHealSpec.spec
    UnshowableSpec.spec
    NormalizeGateSpec.spec
    NormalizeProposalsSpec.spec
    ValueEchoSpec.spec
    ValueSynopsisSpec.spec
    ScratchVetSpec.spec
    PreludeScopeSpec.spec
    PreludeGhciSpec.spec
    QualifiedNameSpec.spec
    ScratchScopeSpec.spec
    CheckTypeScopeSpec.spec
    ScratchpadKeySpec.spec
    HoogleDbSpec.spec
    SearchCacheFreshSpec.spec
    ArgRepairSpec.spec
    RefinementFitSpec.spec
    InsertSupersedeSpec.spec
    TriageSpec.spec
    CapabilitySearchSpec.spec
    CapabilityApiSpec.spec
    BrowseCardSpec.spec
    BrowseCardSanitizeSpec.spec
    BrowseCardParseSpec.spec
    CapabilitySpec.spec
    NotebookViolationSpec.spec
    GrammarRouteSpec.grammarRouteSpec
    HoleFitsSpec.spec
    IndexAnswerSpec.spec
    HoleProbeSpec.spec
    ExportSpec.spec
    DefaultExtsSpec.spec
    SessionResetSpec.spec
    EnvStaleSpec.spec
    EditDispatchSpec.spec
    DeferredModeSpec.spec
    RunModeWireSpec.spec
    EditSignificanceSpec.spec
    EditNoOpSpec.spec
    GraphEdgesSpec.spec
    OrphanModuleReconcileSpec.spec
    ReactivityDoorSpec.spec
    BridgeGraphSpec.spec
    SkipFeedbackSpec.spec
    TypecheckClassifySpec.spec
    liveSpecs $ do
        SessionSpec.spec
        SessionLoopSpec.spec
        SessionLiveSpec.spec
        PureEvalLiveSpec.spec
        MaterializeSpec.spec
        SwitchNotebookSpec.spec
        CompileEscalationSpec.spec
        CompileGateSpec.spec
        DiagnosticMitigationLiveSpec.spec
        DiagnosticMitigationCompoundSpec.spec
        RepairGateSpec.spec
        TypecheckPrimitiveSpec.spec
        TrySpec.spec
        TryOutcomeWireSpec.spec
        TryHoleProbeSpec.spec
