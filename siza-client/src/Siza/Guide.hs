{- |
The typed core of siza's neuro-symbolic techniques, as a reading index.
Each section makes one constraint-evading state unrepresentable or names one
pipeline; start here, each section names its enforcing module.
-}
module Siza.Guide (
    -- * The seams

    {- | Two calling conventions coexist: Dispatch (ToolCall-based, in
    "Siza.Agent.Stack", "Siza.Agent.Repair", "Siza.Agent.RepairLocate")
    and Call (ToolName-based, in "Siza.Agent.Check.Vet", "Siza.Agent.VerifyTool").
    -}
    Driver (..),
    SurfacePolicy (..),

    -- * The episode loop

    {- | The facade seeds the transcript; the machine is
    'Siza.Agent.Loop.Step.runTurns', with mutable state in
    "Siza.Agent.Loop.Episode".
    -}
    runEpisodeSeeded,

    -- * Only parsed, executed code counts as an artifact

    {- | Only an unexported 'Vetted' Haskell value reaches the server gate.
    "Siza.Agent.Owned" records health separately from execution;
    'landedArtifact' requires healthy, executed, substantive source.
    -}
    Vetted,
    preflight,
    landedArtifact,
    stopDecision,
    redSignature,
    noProgressStep,

    -- * Repair is a function of diagnostic class

    {- | The policy spine (@DiagClass@, @tiersFor@, @acceptRepair@) lives in
    sabela's src-contract, module @Sabela.AI.RepairDispatch@; 'repairOne'
    drives it and 'guardDispatch' stops futile repeats.
    -}
    repairOne,
    guardDispatch,

    -- * Verify before claim

    {- | 'verifyGate' proposes a check, 'vetVerdictAgainst' mutation-tests
    it, 'checkVerdict3With' extracts counterexamples, and 'verdictMsg'
    routes the claim onto its typed channel.
    -}
    CheckResult (..),
    NoVerdict (..),
    verifyGate,
    vetVerdictAgainst,
    checkVerdict3With,
    verdictMsg,

    -- * Discovery

    {- | The pipeline entry is @runDiscoverGoal@ in "Siza.Agent.DiscoverTool"
    (unexported; 'Siza.Agent.DiscoverTool.runDiscoverTool' is its exported
    caller). Evidence is never overstated; eviction is always disclosed.
    -}
    guardDiscover,
    boundEnvelope,
    missAdvice,
    standingGoal,

    -- * Context economy

    {- | 'mustKeep' is the never-drop contract: no elision loses a
    diagnostic, verdict, or failure. 'answerRecall' is the escrow a
    dropped result is read back from.
    -}
    mustKeep,
    compactWith,
    emitTurn,
    distillOutcome,
    answerRecall,
    retrieveForPrompt,

    -- * Default-off

    {- | Rejection sampling runs only when @SIZA_SAMPLE_K > 1@ (default 1 =
    off); exemplar memory only when @SIZA_EXEMPLAR_STORE@ is set.
    -}
    module Siza.Agent.Loop.Sampling,
) where

import Siza.Agent.Chat.Verify (verifyGate)
import Siza.Agent.Check (CheckResult (..), NoVerdict (..), checkVerdict3With)
import Siza.Agent.Check.Vet (vetVerdictAgainst)
import Siza.Agent.Compact (compactWith, mustKeep)
import Siza.Agent.Discover.Envelope (boundEnvelope)
import Siza.Agent.Discover.Goal (standingGoal)
import Siza.Agent.Discover.HistoryGuard (guardDiscover)
import Siza.Agent.Discover.MissLadder (missAdvice)
import Siza.Agent.EmitLedger (emitTurn)
import Siza.Agent.Exemplars (retrieveForPrompt)
import Siza.Agent.Futility (guardDispatch)
import Siza.Agent.Loop (runEpisodeSeeded)
import Siza.Agent.Loop.Sampling (dispatchCall)
import Siza.Agent.Loop.Types (Driver (..))
import Siza.Agent.Loop.Verdict (verdictMsg)
import Siza.Agent.OutcomeDistill (distillOutcome)
import Siza.Agent.Owned (
    landedArtifact,
    noProgressStep,
    redSignature,
    stopDecision,
 )
import Siza.Agent.Recall (answerRecall)
import Siza.Agent.Repair (repairOne)
import Siza.Agent.Stack (SurfacePolicy (..))
import Siza.Preflight (Vetted, preflight)
