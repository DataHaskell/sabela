module Test.AllSpecsA (allSpecsA) where

import Test.Hspec (Spec)
import qualified Test.ArgRepairSpec as ArgRepairSpec
import qualified Test.BrowseCardSanitizeSpec as BrowseCardSanitizeSpec
import qualified Test.BrowseCardSpec as BrowseCardSpec
import qualified Test.CapabilityApiSpec as CapabilityApiSpec
import qualified Test.CapabilitySearchSpec as CapabilitySearchSpec
import qualified Test.CapabilitySpec as CapabilitySpec
import qualified Test.ClassifyErrorSpec as ClassifyErrorSpec
import qualified Test.CompileEscalationSpec as CompileEscalationSpec
import qualified Test.CompileGateRenderSpec as CompileGateRenderSpec
import qualified Test.CompileGateSpec as CompileGateSpec
import qualified Test.CompileGateWireSpec as CompileGateWireSpec
import qualified Test.CompiledPlanSpec as CompiledPlanSpec
import qualified Test.DefaultExtsSpec as DefaultExtsSpec
import qualified Test.DepRepairSpec as DepRepairSpec
import qualified Test.DiagnoseSpec as DiagnoseSpec
import qualified Test.DiagnosticMitigationCompoundSpec as DiagnosticMitigationCompoundSpec
import qualified Test.DiagnosticMitigationLiveSpec as DiagnosticMitigationLiveSpec
import qualified Test.DiagnosticMitigationSpec as DiagnosticMitigationSpec
import qualified Test.DiscoverGrammarSpec as DiscoverGrammarSpec
import qualified Test.DiscoverToolSpec as DiscoverToolSpec
import qualified Test.ErrorIndexSpec as ErrorIndexSpec
import qualified Test.ErrorsJsonSpec as ErrorsJsonSpec
import qualified Test.ExampleSearchSpec as ExampleSearchSpec
import qualified Test.ExecuteCellSpec as ExecuteCellSpec
import qualified Test.ExportSpec as ExportSpec
import qualified Test.ExtRepairSpec as ExtRepairSpec
import qualified Test.GrammarRouteSpec as GrammarRouteSpec
import qualified Test.HealthSpec as HealthSpec
import qualified Test.HoleFitsSpec as HoleFitsSpec
import qualified Test.HoleProbeSpec as HoleProbeSpec
import qualified Test.HoogleIntentSpec as HoogleIntentSpec
import qualified Test.HoogleProseSpec as HoogleProseSpec
import qualified Test.HoogleResolveSpec as HoogleResolveSpec
import qualified Test.ImportRepairSpec as ImportRepairSpec
import qualified Test.InsertSupersedeSpec as InsertSupersedeSpec
import qualified Test.JsonDiagSpec as JsonDiagSpec
import qualified Test.MaterializeSpec as MaterializeSpec
import qualified Test.ModuleResolveSpec as ModuleResolveSpec
import qualified Test.NonThreadedForkSpec as NonThreadedForkSpec
import qualified Test.NormalizeGateSpec as NormalizeGateSpec
import qualified Test.NormalizeProposalsSpec as NormalizeProposalsSpec
import qualified Test.NotebookAnimSpec as NotebookAnimSpec
import qualified Test.NotebookEditSpec as NotebookEditSpec
import qualified Test.NotebookFrpSpec as NotebookFrpSpec
import qualified Test.NotebookPictureSpec as NotebookPictureSpec
import qualified Test.NotebookViolationSpec as NotebookViolationSpec
import qualified Test.OwnedSpec as OwnedSpec
import qualified Test.PromptUnifySpec as PromptUnifySpec
import qualified Test.PureEvalLiveSpec as PureEvalLiveSpec
import qualified Test.QualifiedNameSpec as QualifiedNameSpec
import qualified Test.RefinementFitSpec as RefinementFitSpec
import qualified Test.RepairEngineSpec as RepairEngineSpec
import qualified Test.RepairGateSpec as RepairGateSpec
import qualified Test.RepairGuardSpec as RepairGuardSpec
import qualified Test.RepairTraceWireSpec as RepairTraceWireSpec
import qualified Test.ScratchScopeSpec as ScratchScopeSpec
import qualified Test.ScratchVetSpec as ScratchVetSpec
import qualified Test.SelfHealSpec as SelfHealSpec
import qualified Test.SessionLiveSpec as SessionLiveSpec
import qualified Test.SessionLoopSpec as SessionLoopSpec
import qualified Test.SessionSpec as SessionSpec
import qualified Test.SpineSpec as SpineSpec
import qualified Test.StaleRunSpec as StaleRunSpec
import qualified Test.SwitchNotebookSpec as SwitchNotebookSpec
import qualified Test.ThrowawayExecuteSpec as ThrowawayExecuteSpec
import qualified Test.ToolInputRewriteSpec as ToolInputRewriteSpec
import qualified Test.TopoSpec as TopoSpec
import qualified Test.TriageSpec as TriageSpec
import qualified Test.TryCacheSpec as TryCacheSpec
import qualified Test.TryHoleProbeSpec as TryHoleProbeSpec
import qualified Test.TryOutcomeWireSpec as TryOutcomeWireSpec
import qualified Test.TryPlanSpec as TryPlanSpec
import qualified Test.TrySpec as TrySpec
import qualified Test.TypeDirectedResolveSpec as TypeDirectedResolveSpec
import qualified Test.TypeDiscoverySpec as TypeDiscoverySpec
import qualified Test.TypecheckPrimitiveSpec as TypecheckPrimitiveSpec
import qualified Test.ValueEchoSpec as ValueEchoSpec
import qualified Test.VerifyDownstreamSpec as VerifyDownstreamSpec
import qualified Test.WidgetsSpec as WidgetsSpec

allSpecsA :: Spec
allSpecsA = do
        NotebookFrpSpec.spec
        NotebookEditSpec.spec
        NotebookPictureSpec.spec
        NotebookAnimSpec.spec
        WidgetsSpec.spec
        SessionSpec.spec
        SessionLoopSpec.spec
        SessionLiveSpec.spec
        PureEvalLiveSpec.spec
        MaterializeSpec.spec
        StaleRunSpec.spec
        SwitchNotebookSpec.spec
        TopoSpec.spec
        CompiledPlanSpec.spec
        CompileEscalationSpec.spec
        CompileGateSpec.spec
        CompileGateRenderSpec.spec
        CompileGateWireSpec.spec
        ClassifyErrorSpec.spec
        ErrorsJsonSpec.errorsJsonSpec
        ErrorIndexSpec.spec
        JsonDiagSpec.jsonDiagSpec
        ExecuteCellSpec.spec
        ExampleSearchSpec.spec
        DiagnoseSpec.diagnoseSpec
        DiagnosticMitigationSpec.spec
        DiagnosticMitigationLiveSpec.spec
        DiagnosticMitigationCompoundSpec.spec
        SpineSpec.spineSpec
        ModuleResolveSpec.spec
        DiscoverToolSpec.spec
        DepRepairSpec.spec
        ExtRepairSpec.spec
        ImportRepairSpec.spec
        RepairGuardSpec.spec
        HealthSpec.spec
        OwnedSpec.spec
        RepairEngineSpec.spec
        RepairGateSpec.spec
        RepairTraceWireSpec.spec
        VerifyDownstreamSpec.spec
        ToolInputRewriteSpec.spec
        PromptUnifySpec.spec
        DiscoverGrammarSpec.spec
        HoogleIntentSpec.spec
        HoogleProseSpec.spec
        HoogleResolveSpec.spec
        TypeDirectedResolveSpec.spec
        TypeDiscoverySpec.spec
        TypecheckPrimitiveSpec.spec
        TryPlanSpec.spec
        TrySpec.spec
        TryOutcomeWireSpec.spec
        TryHoleProbeSpec.spec
        TryCacheSpec.spec
        ThrowawayExecuteSpec.spec
        NonThreadedForkSpec.spec
        SelfHealSpec.spec
        NormalizeGateSpec.spec
        NormalizeProposalsSpec.spec
        ValueEchoSpec.spec
        ScratchVetSpec.spec
        QualifiedNameSpec.spec
        ScratchScopeSpec.spec
        ArgRepairSpec.spec
        RefinementFitSpec.spec
        InsertSupersedeSpec.spec
        TriageSpec.spec
        CapabilitySearchSpec.spec
        CapabilityApiSpec.spec
        BrowseCardSpec.spec
        BrowseCardSanitizeSpec.spec
        CapabilitySpec.spec
        NotebookViolationSpec.spec
        GrammarRouteSpec.grammarRouteSpec
        HoleFitsSpec.spec
        HoleProbeSpec.spec
        ExportSpec.spec
        DefaultExtsSpec.spec
