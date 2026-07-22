module Main (main) where

import Test.AdviceSpec (adviceSpec)
import Test.AgentCheckSpec (agentCheckSpec)
import Test.CandidateRankSpec (candidateRankSpec)
import Test.CandidateSpec (candidateSpec)
import Test.CardGateSpec (cardGateSpec)
import Test.ContextCharsSpec (contextCharsSpec)
import Test.CounterexampleSpec (counterexampleSpec)
import Test.CrossSeamSpec (crossSeamSpec)
import Test.DiscoverAbsentBoundSpec (discoverAbsentBoundSpec)
import Test.DiscoverCatalogueSpec (discoverCatalogueSpec)
import Test.DiscoverClosureSpec (discoverClosureSpec)
import Test.DiscoverConstructSpec (discoverConstructSpec)
import Test.DiscoverEmissionSpec (discoverEmissionSpec)
import Test.DiscoverEnvelopeSpec (discoverEnvelopeSpec)
import Test.DiscoverGoalRankSpec (discoverGoalRankSpec)
import Test.DiscoverGoalSpec (discoverGoalSpec)
import Test.DiscoverHiddenDbSpec (discoverHiddenDbSpec)
import Test.DiscoverHistorySpec (discoverHistorySpec)
import Test.DiscoverInvariantSpec (discoverInvariantSpec)
import Test.DiscoverInventorySpec (discoverInventorySpec)
import Test.DiscoverLedgerSpec (discoverLedgerSpec)
import Test.DiscoverMissSpec (discoverMissSpec)
import Test.DiscoverModeGridSpec (discoverModeGridSpec)
import Test.DiscoverRankPlainSpec (discoverRankPlainSpec)
import Test.DiscoverRecordBudgetSpec (discoverRecordBudgetSpec)
import Test.DiscoverRequestSpec (discoverRequestSpec)
import Test.DiscoverResolvedSpec (discoverResolvedSpec)
import Test.DiscoverScopeConserveSpec (discoverScopeConserveSpec)
import Test.DiscoverScopeLedgerSpec (discoverScopeLedgerSpec)
import Test.DiscoverSeedSpec (discoverSeedSpec)
import Test.DiscoverToolClientSpec (discoverToolSpec)
import Test.DiscoverTruthSpec (discoverTruthSpec)
import Test.DiscoverUnitScrubSpec (discoverUnitScrubSpec)
import Test.DiscoverVariantDedupSpec (discoverVariantDedupSpec)
import Test.DiscoverWorldChangeSpec (discoverWorldChangeSpec)
import Test.DoneSignalSpec (doneSignalSpec)
import Test.EmitLedgerProtectSpec (emitLedgerProtectSpec)
import Test.EmitLedgerSpec (emitLedgerSpec)
import Test.EscalateSpec (escalateSpec)
import Test.FactSelectSpec (factSelectSpec)
import Test.FutilitySpec (futilitySpec)
import Test.GoalHonestySpec (goalHonestySpec)
import Test.GrammarCardSpec (grammarCardSpec)
import Test.HoleDirectedInvariantSpec (holeDirectedInvariantSpec)
import Test.Hspec
import Test.HubTokenSpec (hubTokenSpec)
import Test.KernelVocabClientSpec (kernelVocabClientSpec)
import Test.LanguageSpec (
    annotateSpec,
    contractSpec,
    parseSpec,
    securitySpec,
 )
import Test.LeverSurfaceSpec (leverSurfaceSpec)
import Test.LoginSpec (loginSpec)
import Test.McpSpec (mcpSpec)
import Test.NormalizeFindabilitySpec (normalizeFindabilitySpec)
import Test.NoteLedgerSpec (noteLedgerSpec)
import Test.NudgeSpec (nudgeSpec)
import Test.OutcomeDistillSpec (outcomeDistillSpec)
import Test.PostNudgeGateSpec (postNudgeGateSpec)
import Test.ProvenanceSpec (chainSpec, provenanceSpec, retroSpec)
import Test.ReEchoFixtureSpec (reEchoFixtureSpec)
import Test.RedStreakSpec (redStreakSpec)
import Test.ReenterContrastSpec (reenterContrastSpec)
import Test.RenderContractSpec (renderContractSpec)
import Test.RepairCascadeSpec (repairCascadeSpec)
import Test.RepairDispatchSpec (repairDispatchSpec)
import Test.RepairGridSpec (repairGridSpec)
import Test.RepairGuardSpec (repairGuardSpec)
import Test.RepairTierGenSpec (repairTierGenSpec)
import Test.RoutedUnblockSpec (routedUnblockSpec)
import Test.ScaffoldChatSpec (scaffoldChatSpec)
import Test.SchemaRecoverySpec (schemaRecoverySpec)
import Test.SteerFeedSpec (steerFeedSpec)
import Test.SteerLoopSpec (steerLoopSpec)
import Test.SteerSpec (steerSpec)
import Test.ToolRouteSpec (toolRouteSpec)
import Test.TransportFailureSpec (transportFailureSpec)
import Test.TransportSpec (transportSpec)
import Test.VerdictSurfaceSpec (verdictSurfaceSpec)
import Test.VerifierSurfaceSpec (verifierSurfaceSpec)
import Test.VerifyDiagSpec (verifyDiagSpec)
import Test.WorldCardSpec (worldCardSpec)
import Test.WorldChangeGateSpec (worldChangeGateSpec)
import Test.WrapUpLoopSpec (wrapUpLoopSpec)
import Test.WrapUpSpec (wrapUpSpec)
import Test.WriteAckClientSpec (writeAckClientSpec)

main :: IO ()
main = hspec $ do
    parseSpec
    securitySpec
    annotateSpec
    contractSpec
    provenanceSpec
    chainSpec
    retroSpec
    crossSeamSpec
    transportSpec
    transportFailureSpec
    futilitySpec
    hubTokenSpec
    loginSpec
    mcpSpec
    agentCheckSpec
    kernelVocabClientSpec
    writeAckClientSpec
    discoverToolSpec
    discoverCatalogueSpec
    discoverHiddenDbSpec
    discoverTruthSpec
    discoverUnitScrubSpec
    discoverLedgerSpec
    discoverScopeLedgerSpec
    discoverWorldChangeSpec
    discoverConstructSpec
    discoverSeedSpec
    discoverInvariantSpec
    discoverVariantDedupSpec
    normalizeFindabilitySpec
    reenterContrastSpec
    discoverMissSpec
    discoverRequestSpec
    discoverEnvelopeSpec
    discoverHistorySpec
    discoverClosureSpec
    discoverEmissionSpec
    cardGateSpec
    verifierSurfaceSpec
    discoverInventorySpec
    discoverModeGridSpec
    discoverScopeConserveSpec
    discoverResolvedSpec
    discoverRankPlainSpec
    discoverRecordBudgetSpec
    discoverAbsentBoundSpec
    steerFeedSpec
    discoverGoalSpec
    discoverGoalRankSpec
    repairDispatchSpec
    repairGuardSpec
    repairTierGenSpec
    repairGridSpec
    holeDirectedInvariantSpec
    worldChangeGateSpec
    goalHonestySpec
    worldCardSpec
    repairCascadeSpec
    redStreakSpec
    verifyDiagSpec
    counterexampleSpec
    contextCharsSpec
    noteLedgerSpec
    nudgeSpec
    outcomeDistillSpec
    postNudgeGateSpec
    factSelectSpec
    steerSpec
    steerLoopSpec
    wrapUpSpec
    wrapUpLoopSpec
    toolRouteSpec
    schemaRecoverySpec
    emitLedgerSpec
    emitLedgerProtectSpec
    adviceSpec
    candidateSpec
    candidateRankSpec
    escalateSpec
    routedUnblockSpec
    renderContractSpec
    reEchoFixtureSpec
    doneSignalSpec
    grammarCardSpec
    leverSurfaceSpec
    verdictSurfaceSpec
    scaffoldChatSpec
