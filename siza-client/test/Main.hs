module Main (main) where

import Test.AdviceSpec (adviceSpec)
import Test.AgentCheckSpec (agentCheckSpec)
import Test.ArtifactSpec (artifactSpec)
import Test.CandidateRankSpec (candidateRankSpec)
import Test.CandidateSpec (candidateSpec)
import Test.CardGateSpec (cardGateSpec)
import Test.CatalogueHonestySpec (catalogueHonestySpec)
import Test.ChatExportSpec (chatExportSpec)
import Test.CheckGateSpec (checkGateSpec)
import Test.CheckScopeSpec (checkScopeSpec)
import Test.CompactProtectSpec (compactProtectSpec)
import Test.CompactSpec (compactSpec)
import Test.ConsultedStatusSpec (consultedStatusSpec)
import Test.ContextCharsSpec (contextCharsSpec)
import Test.CounterexampleSpec (counterexampleSpec)
import Test.CrossSeamSpec (crossSeamSpec)
import Test.DeadlineStartSpec (deadlineStartSpec)
import Test.DiscoverAbsentBoundSpec (discoverAbsentBoundSpec)
import Test.DiscoverCardMatchSpec (discoverCardMatchSpec)
import Test.DiscoverCatalogueSpec (discoverCatalogueSpec)
import Test.DiscoverClosureSpec (discoverClosureSpec)
import Test.DiscoverConstructSpec (discoverConstructSpec)
import Test.DiscoverDemotionSpec (discoverDemotionSpec)
import Test.DiscoverDuplicatePayloadSpec (discoverDuplicatePayloadSpec)
import Test.DiscoverEmissionSpec (discoverEmissionSpec)
import Test.DiscoverEnvelopeSpec (discoverEnvelopeSpec)
import Test.DiscoverEscalateSpec (discoverEscalateSpec)
import Test.DiscoverGoalRankSpec (discoverGoalRankSpec)
import Test.DiscoverGoalSpec (discoverGoalSpec)
import Test.DiscoverHackageSpec (discoverHackageSpec)
import Test.DiscoverHiddenDbSpec (discoverHiddenDbSpec)
import Test.DiscoverHistorySpec (discoverHistorySpec)
import Test.DiscoverHitFieldSpec (discoverHitFieldSpec)
import Test.DiscoverHomonymSpec (discoverHomonymSpec)
import Test.DiscoverInvariantSpec (discoverInvariantSpec)
import Test.DiscoverInventorySpec (discoverInventorySpec)
import Test.DiscoverLedgerSpec (discoverLedgerSpec)
import Test.DiscoverMissSpec (discoverMissSpec)
import Test.DiscoverModeGridSpec (discoverModeGridSpec)
import Test.DiscoverNextSpec (discoverNextSpec)
import Test.DiscoverProducerHintSpec (discoverProducerHintSpec)
import Test.DiscoverQueryShapeSpec (discoverQueryShapeSpec)
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
import Test.DiscoverUseSpec (discoverUseSpec)
import Test.DiscoverVariantDedupSpec (discoverVariantDedupSpec)
import Test.DiscoverWorldChangeSpec (discoverWorldChangeSpec)
import Test.DiscoverWorldTruthSpec (discoverWorldTruthSpec)
import Test.DoneSignalSpec (doneSignalSpec)
import Test.EmitLedgerProtectSpec (emitLedgerProtectSpec)
import Test.EmitLedgerSpec (emitLedgerSpec)
import Test.EmitLedgerSummarySpec (emitLedgerSummarySpec)
import Test.EnvelopeBudgetSpec (envelopeBudgetSpec)
import Test.FactSelectSpec (factSelectSpec)
import Test.FitsBlobSpec (fitsBlobSpec)
import Test.FutilitySpec (futilitySpec)
import Test.GoalHonestySpec (goalHonestySpec)
import Test.GoalPlumbingSpec (goalPlumbingSpec)
import Test.GrammarCardSpec (grammarCardSpec)
import Test.HarnessTruthSpec (harnessTruthSpec)
import Test.HoleDirectedInvariantSpec (holeDirectedInvariantSpec)
import Test.HoleProbeSpec (holeProbeSpec)
import Test.Hspec
import Test.HubTokenSpec (hubTokenSpec)
import Test.InstallAffordanceSpec (installAffordanceSpec)
import Test.KernelVocabClientSpec (kernelVocabClientSpec)
import Test.LanguageSpec (
    annotateSpec,
    contractSpec,
    parseSpec,
    securitySpec,
 )
import Test.LeverSurfaceSpec (leverSurfaceSpec)
import Test.LoginSpec (loginSpec)
import Test.MarkerEchoSpec (markerEchoSpec)
import Test.McpCallSpec (mcpCallSpec)
import Test.McpSpec (mcpSpec)
import Test.McpSurfaceSpec (mcpSurfaceSpec)
import Test.NormalizeFindabilitySpec (normalizeFindabilitySpec)
import Test.NoteLedgerSpec (noteLedgerSpec)
import Test.OutcomeDistillSpec (outcomeDistillSpec)
import Test.OutcomeShapeSpec (outcomeShapeSpec)
import Test.PostNudgeGateSpec (postNudgeGateSpec)
import Test.ProvenanceSpec (chainSpec, provenanceSpec, retroSpec)
import Test.ReEchoFixtureSpec (reEchoFixtureSpec)
import Test.RecallSpec (recallSpec)
import Test.RecallSurfaceSpec (recallSurfaceSpec)
import Test.RecordFieldUseSpec (recordFieldUseSpec)
import Test.RedStreakSpec (redStreakSpec)
import Test.ReenterContrastSpec (reenterAlarmSpec, reenterContrastSpec)
import Test.RefutedReplaySpec (refutedReplaySpec)
import Test.RenderContractSpec (renderContractSpec)
import Test.RenderOutcomeSpec (renderOutcomeSpec)
import Test.RepairBudgetFlagSpec (repairBudgetFlagSpec)
import Test.RepairCascadeSpec (repairCascadeSpec)
import Test.RepairDispatchSpec (repairDispatchSpec)
import Test.RepairGridSpec (repairGridSpec)
import Test.RepairGuardSpec (repairGuardSpec)
import Test.RepairTierGenSpec (repairTierGenSpec)
import Test.RetroTranscriptSpec (retroTranscriptSpec)
import Test.RoutedUnblockSpec (routedUnblockSpec)
import Test.ScaffoldChatSpec (scaffoldChatSpec)
import Test.SchemaRecoverySpec (schemaRecoverySpec)
import Test.StackNoteDeliverySpec (stackNoteDeliverySpec)
import Test.StackParitySpec (stackParitySpec)
import Test.StackSessionSpec (stackSessionSpec)
import Test.StateQuerySpec (stateQuerySpec)
import Test.SteerFeedSpec (steerFeedSpec)
import Test.SteerSpec (steerSpec)
import Test.SystemPromptSpec (systemPromptSpec)
import Test.ToolRouteSpec (toolRouteSpec)
import Test.TransportFailureSpec (transportFailureSpec)
import Test.TransportSpec (toolTimeoutSpec, transportSpec)
import Test.TrySurfaceSpec (trySurfaceSpec)
import Test.UnconfirmedWriteSpec (unconfirmedWriteSpec)
import Test.VerdictEvidenceSpec (verdictEvidenceSpec)
import Test.VerdictProtectSpec (verdictProtectSpec)
import Test.VerdictStopSpec (verdictStopSpec)
import Test.VerdictSurfaceSpec (verdictSurfaceSpec)
import Test.VerifierSurfaceSpec (verifierSurfaceSpec)
import Test.VerifyDiagSpec (verifyDiagSpec)
import Test.VerifyToolSpec (verifyToolSpec)
import Test.VetSilenceSpec (vetSilenceSpec)
import Test.WorldCardSpec (worldCardSpec)
import Test.WorldChangeGateSpec (worldChangeGateSpec)
import Test.WrapUpIntegritySpec (wrapUpIntegritySpec)
import Test.WrapUpLoopSpec (wrapUpLoopSpec)
import Test.WrapUpSpec (wrapUpSpec)
import Test.WriteAckClientSpec (writeAckClientSpec)

main :: IO ()
main = hspec $ do
    stateQuerySpec
    parseSpec
    securitySpec
    annotateSpec
    contractSpec
    provenanceSpec
    chainSpec
    retroSpec
    crossSeamSpec
    transportSpec
    toolTimeoutSpec
    transportFailureSpec
    futilitySpec
    renderOutcomeSpec
    recordFieldUseSpec
    systemPromptSpec
    hubTokenSpec
    loginSpec
    mcpSpec
    agentCheckSpec
    installAffordanceSpec
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
    discoverHomonymSpec
    discoverCardMatchSpec
    discoverVariantDedupSpec
    normalizeFindabilitySpec
    reenterContrastSpec
    reenterAlarmSpec
    discoverMissSpec
    discoverRequestSpec
    discoverEnvelopeSpec
    discoverNextSpec
    artifactSpec
    compactSpec
    compactProtectSpec
    recallSpec
    recallSurfaceSpec
    consultedStatusSpec
    fitsBlobSpec
    discoverHackageSpec
    discoverHistorySpec
    discoverClosureSpec
    discoverDuplicatePayloadSpec
    discoverUseSpec
    envelopeBudgetSpec
    discoverHitFieldSpec
    discoverWorldTruthSpec
    refutedReplaySpec
    repairBudgetFlagSpec
    unconfirmedWriteSpec
    discoverEmissionSpec
    cardGateSpec
    verifierSurfaceSpec
    discoverInventorySpec
    discoverDemotionSpec
    discoverModeGridSpec
    discoverScopeConserveSpec
    discoverResolvedSpec
    discoverQueryShapeSpec
    discoverRankPlainSpec
    discoverRecordBudgetSpec
    discoverAbsentBoundSpec
    steerFeedSpec
    discoverGoalSpec
    discoverGoalRankSpec
    discoverEscalateSpec
    discoverProducerHintSpec
    repairDispatchSpec
    repairGuardSpec
    repairTierGenSpec
    repairGridSpec
    holeDirectedInvariantSpec
    worldChangeGateSpec
    goalHonestySpec
    worldCardSpec
    repairCascadeSpec
    stackSessionSpec
    mcpCallSpec
    stackParitySpec
    stackNoteDeliverySpec
    goalPlumbingSpec
    verifyToolSpec
    catalogueHonestySpec
    mcpSurfaceSpec
    vetSilenceSpec
    redStreakSpec
    verifyDiagSpec
    counterexampleSpec
    contextCharsSpec
    chatExportSpec
    noteLedgerSpec
    outcomeDistillSpec
    outcomeShapeSpec
    postNudgeGateSpec
    factSelectSpec
    steerSpec
    wrapUpSpec
    wrapUpLoopSpec
    wrapUpIntegritySpec
    toolRouteSpec
    schemaRecoverySpec
    emitLedgerSpec
    emitLedgerProtectSpec
    emitLedgerSummarySpec
    adviceSpec
    candidateSpec
    checkGateSpec
    deadlineStartSpec
    candidateRankSpec
    holeProbeSpec
    routedUnblockSpec
    renderContractSpec
    reEchoFixtureSpec
    doneSignalSpec
    markerEchoSpec
    checkScopeSpec
    verdictEvidenceSpec
    verdictStopSpec
    verdictProtectSpec
    harnessTruthSpec
    grammarCardSpec
    leverSurfaceSpec
    retroTranscriptSpec
    verdictSurfaceSpec
    trySurfaceSpec
    scaffoldChatSpec
