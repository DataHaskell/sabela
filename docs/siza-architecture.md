# Siza: a reader's guide to the code

This document routes a reader through the siza codebase.

---

## 0. Where is the core?

A weak model proposes; typed symbolic gates decide what counts. The
gates live in five bounded contexts: Episode, Gating/Repair, Discovery,
Context Economy and Observation, each with one entry point and one
governing law. Table T1 maps each headline technique to the function
where it starts.

### T1. Technique-to-module map

| Technique | Entry function | File | Package |
|---|---|---|---|
| Compile gate (server) | `gatedCandidate` | `src/Sabela/AI/Capabilities/Edit/GateRepair.hs` | sabela |
| Compile gate (client echo) | `Siza.Agent.Owned.landedArtifact` | `siza-client/src/Siza/Agent/Owned.hs` | siza-client |
| Parse gate | `Siza.Preflight.preflight` (yields `Vetted`) | `siza-client/src/Siza/Preflight.hs` | siza-client |
| Repair tier ladder | `Siza.Agent.Repair.repairOne` | `siza-client/src/Siza/Agent/Repair.hs` | siza-client |
| Repair policy spine | `tiersFor`, `acceptRepair` | `src-contract/Sabela/AI/RepairDispatch.hs` | sabela-contract |
| Verify-before-claim | `Siza.Agent.Chat.Verify.verifyGate` | `siza-client/src/Siza/Agent/Chat/Verify.hs` | siza-client |
| Check vetting | `Siza.Agent.Check.Vet.vetVerdictAgainst` | `siza-client/src/Siza/Agent/Check/Vet.hs` | siza-client |
| Marker-cell check run | `Siza.Agent.Check.checkVerdict3With` | `siza-client/src/Siza/Agent/Check.hs`, `Check/Marker.hs` | siza-client |
| Verdict channels | `Siza.Agent.Loop.Verdict.verdictMsg` (`VerdictClass`) | `siza-client/src/Siza/Agent/Loop/Verdict.hs`; type in `src-contract/Sabela/AI/Verdict.hs` | both |
| Rejection sampling | `Siza.Agent.Loop.Sampling.dispatchCall` | `siza-client/src/Siza/Agent/Loop/Sampling.hs` + `Siza/Agent/Sample.hs`; off unless `SIZA_SAMPLE_K` > 1 | siza-client |
| Grounding before re-ask | `Siza.Agent.Loop.Support.groundingMsgs` | `siza-client/src/Siza/Agent/Loop/Support.hs` | siza-client |
| Discover pipeline | `Siza.Agent.DiscoverTool.runDiscoverGoal` | `siza-client/src/Siza/Agent/DiscoverTool.hs` | siza-client |
| Envelope eviction | `Siza.Agent.Discover.Envelope.boundEnvelope` | `siza-client/src/Siza/Agent/Discover/Envelope.hs` + `Evict.hs` | siza-client |
| Search ledger and miss ladder | `Siza.Agent.Discover.HistoryGuard.guardDiscover` | `siza-client/src/Siza/Agent/Discover/HistoryGuard.hs` + `MissLadder.hs` | siza-client |
| Context economy | `Siza.Agent.Compact.compactWith` | `siza-client/src/Siza/Agent/Compact.hs`, `EmitLedger.hs`, `OutcomeDistill.hs`, `Recall.hs` | siza-client |
| Futility trio | `Siza.Agent.Futility.guardDispatch` | `siza-client/src/Siza/Agent/Futility.hs`, `Streak.hs`, `Owned.hs` (`noProgressStep`) | siza-client |

### The reading path

Start at `siza-client/README.md`: the bounded-context map (Episode,
Gating/Repair, Discovery, Context Economy, Observation, Infrastructure)
and the two-package warning (the compile gate and repair policy spine
live in sabela rather than siza-client). Then `siza-client/src/Siza/Guide.hs`,
the compiler-checked table of contents. Then walk:

1. **The seam.** `Siza.Agent.Loop.Types`
   (`siza-client/src/Siza/Agent/Loop/Types.hs`), the `Driver` record
   (`drvChat`/`drvDispatch`/`drvNow`/`drvVerify`): the episode's
   Handle. The chat REPL and the eval harness build full `Driver`s;
   the MCP server shares the `drvDispatch` wire through the same
   dispatch stack. Every technique intercepts one wire.
2. **The machine.** `Siza.Agent.Loop.runEpisodeSeeded`
   (`siza-client/src/Siza/Agent/Loop.hs`; a facade plus transcript
   seeding, and note it wraps `drvChat` in `recoverTurn`), then
   `Siza.Agent.Loop.Step.runTurns`
   (`siza-client/src/Siza/Agent/Loop/Step.hs`;
   `calledTurn`/`noCallTurn`/`reenter`), with the mutable state in
   `siza-client/src/Siza/Agent/Loop/Episode.hs`.
3. **Life of a write, in the order the types enforce it.**
   `Siza.Preflight.preflight` (only a `Vetted`, constructor unexported,
   reaches the wire) → `Siza.Agent.Owned.landedArtifact` /
   `stopDecision` / `redSignature` (the client echo of the server-side
   compile gate, and when to stop) → `Siza.Agent.Repair.repairOne` with
   the policy spine `DiagClass`/`tiersFor`/`acceptRepair` in
   `src-contract/Sabela/AI/RepairDispatch.hs` → verify-before-claim:
   `Siza.Agent.Chat.Verify.verifyGate` proposes,
   `Siza.Agent.Check.Vet.vetVerdictAgainst` mutation-tests,
   `Siza.Agent.Check.Marker` runs under channel discipline,
   `Siza.Agent.Check.checkVerdict3With` extracts counterexamples,
   `Siza.Agent.Loop.Verdict.verdictMsg` routes the claim.
4. **The dispatch stack, walking `drvDispatch` down.**
   `Siza.Agent.ToolRoute.routeCallWith`
   (`siza-client/src/Siza/Agent/ToolRoute.hs`; call recovery, schema
   validation) → `Siza.Agent.Stack.stackDispatch`
   (`siza-client/src/Siza/Agent/Stack.hs`; goal capture, futility,
   ledger layers; `SurfacePolicy` is the chat/MCP parity handle) →
   `Siza.Agent.Stack.Call.runToolCall`
   (`siza-client/src/Siza/Agent/Stack/Call.hs`; blocked-write
   rerouting, display repair, healing).
5. **Discovery.** `Siza.Agent.DiscoverTool.runDiscoverGoal` (NOT
   `Siza.Agent.GrammarCards`, the grammar-card surface formerly named
   `Siza.Agent.Discover`) → `Discover/Merge.hs` and `Rank.hs` →
   `Envelope.boundEnvelope` + `Evict.hs` (disclosed eviction) →
   `HistoryGuard.guardDiscover` (`SearchLedger`) →
   `MissLadder.missAdvice` → `Goal.standingGoal`.
6. **Context economy.** `Siza.Agent.Compact.mustKeep` (the never-drop
   contract) → `compactWith` → `Siza.Agent.EmitLedger.emitTurn` →
   `Siza.Agent.OutcomeDistill.distillOutcome` →
   `Siza.Agent.Recall.answerRecall` (the escrow).
7. **Last, the dark corners.** `Siza.Agent.Loop.Sampling.dispatchCall`
   + `Siza/Agent/Sample.hs` (rejection sampling, off unless
   `SIZA_SAMPLE_K` > 1), `Siza.Agent.Exemplars`
   (`SIZA_EXEMPLAR_STORE`), and Observation
   (`Siza.Agent.Transcript.renderTranscript`,
   `Siza.Retro.Metrics.transcriptMetrics`).

---

## 1. The bounded contexts

Six contexts partition `siza-client/src/`. Each owns a directory, a
handful of core types, and one law the types enforce. The types follow
the discipline described in [LLMs and Haskell: constraint-evading
behaviour](https://blog.jle.im/entry/llms-and-haskell-1-constraint-evading-behavior.html):
each guarantee is a type that makes the corresponding invalid state
unrepresentable, so a weak model (or a hurried maintainer) cannot route
around it.

| Context | Purpose | Owned directory | Core types | Governing law |
|---|---|---|---|---|
| Episode | Run one prompt to a stop reason | `siza-client/src/Siza/Agent/Loop/` + `Loop.hs`, `Chat.hs` | `Driver`, `AgentRun`, `StopDecision`, `OwnedCell` | Every stop carries a typed reason (`stopDecision` in `Owned.hs`; reason tags on `AgentRun`) |
| Gating/Repair | Decide what counts as landed, and heal red cells | `Siza/Preflight.hs`, `Agent/Owned.hs`, `Agent/Repair*.hs`, `Agent/Check*`, `Agent/Futility.hs`, `Agent/Streak.hs`, `Agent/Ack.hs`, `Agent/Scaffold.hs` | `Vetted`, `DiagClass`, `RepairTier`, `CheckResult`, `NoVerdict` | `acceptRepair`: the target heals and no sibling regresses (`src-contract/Sabela/AI/RepairDispatch.hs`) |
| Discovery | Turn a query into bounded, grounded evidence | `Agent/DiscoverTool.hs` + `Agent/Discover/` (45 modules) | `SearchLedger`, `StandingGoal`, envelope `Value`s | Eviction is always disclosed (`Evict.evictionViolations` is the checkable spec) |
| Context Economy | Keep the transcript small without losing actionable bytes | `Agent/Compact.hs`, `EmitLedger.hs`, `OutcomeDistill.hs`, `Recall.hs`, `Exemplars.hs` | recall stubs and indices, `EmitLedger` | `mustKeep` never drops a diagnostic, verdict or failure (`Compact.hs`) |
| Observation | Measure what the model actually saw | `Agent/Transcript.hs`, `Retro.hs`, `Retro/`, `Provenance*.hs` | `SessionEvent`, `TranscriptMetrics` | An unknowable reading is reported unknown (`Retro.Metrics.transcriptMetrics`) |
| Infrastructure | Adapters, free of technique | `Transport*.hs`, `Mcp*.hs`, `Cli*.hs`, `Language.hs`, `Lang/Haskell.hs` | `ToolFailure` | Adapters carry no policy, with one stated exception (`Transport/Failure.hs` blame taxonomy) |

### The two-package warning

Two of the headline techniques do not live in siza-client at all.

- The **compile gate** is server-side: `gatedCandidate` in
  `src/Sabela/AI/Capabilities/Edit/GateRepair.hs` (the sabela package).
  siza-client holds only its JSON echo, read by
  `Siza.Agent.Owned.landedArtifact`.
- The **repair policy spine** (`DiagClass`, `RepairTier`, `tiersFor`,
  `acceptRepair`, `notRegressed`) lives in
  `src-contract/Sabela/AI/RepairDispatch.hs` (the sabela-contract
  package), shared by the server, the client and the eval harness so
  all three judge a repair by the same rules. siza-client holds
  candidate generation (`Agent/RepairTiers.hs`) and the cascade runner
  (`Agent/Repair.hs`).

Grepping siza-client for "compile gate" or `acceptRepair` therefore
finds nothing. Related contract types that also live outside
siza-client: `Sabela.AI.WriteAck` (ack envelope), `Sabela.AI.Verdict`
(`VerdictClass`), `Sabela.AI.Salvage` (prose salvage extractor),
`Sabela.AI.Health` (`DiagnosticKey`).

---

## 2. The seams

The seams follow the [Handle
pattern](https://jaspervdj.be/posts/2018-03-08-handle-pattern.html):
runtime state behind a small record, abstracted only after several
concrete implementations existed.

### The Driver record

`Siza.Agent.Loop.Types` (`siza-client/src/Siza/Agent/Loop/Types.hs`)
defines the episode's only effect surface:

```haskell
data Driver = Driver
    { drvChat :: [Value] -> IO (Either Text Turn)
    , drvDispatch :: ToolCall -> IO (Either Text ToolOutcome)
    , drvNow :: IO Double
    , drvVerify :: Map CellId OwnedCell -> IO (CheckResult, Maybe Text)
    }
```

This is a record of functions rather than a typeclass, and it is an
earned abstraction: several concrete constructions exist before the
record does any abstracting. The chat REPL builds one
(`siza-client/src/Siza/Agent/Chat.hs`) and the eval harness builds two
(`eval/neuro-symbolic/src/Eval/Agent.hs`, `runEpisode` and
`runEpisodeDebug`). The MCP server (`siza-client/src/Siza/Mcp.hs`)
has no model to chat with, so it shares only the `drvDispatch` wire,
through the same dispatch stack. Every technique in the Episode
context intercepts one of the four wires: `recoverTurn` wraps
`drvChat`, the stack wraps `drvDispatch`, budgets read `drvNow`, and
verify-before-claim is `drvVerify`.

### Two calling conventions

Tool-facing code uses one of two shapes, and knowing which is which
explains why the repair and verify pipelines look unrelated:

- `type Dispatch = ToolCall -> IO (Either Text ToolOutcome)`
  (defined in `Siza.Agent.Stack`, `Agent/Repair.hs`,
  `Agent/RepairLocate.hs`, `Agent/GrammarCards.hs`). Used by the dispatch
  stack, the repair cascade, futility, scaffold and discovery: code
  that must see the call as the model made it.
- `type Call = ToolName -> Value -> IO (Either Text ToolOutcome)`
  (defined in `Agent/VerifyTool.hs`; the same shape threads through
  `Check`, `Check/Vet` and `Ack`). Used by harness-initiated calls,
  where the tool name is already typed and there is no model call to
  recover.

### StackSession and SurfacePolicy

`Siza.Agent.Stack` (`siza-client/src/Siza/Agent/Stack.hs`) is the
chat/MCP parity handle. `newStackSession` builds per-session state,
`stackDispatch` layers normalisation, goal capture, the discover
ledger and futility over any `Dispatch`, and `SurfacePolicy` derives
the per-surface differences (who heals, who elides, who folds
discovery) from the surface enum. `stackLayers` is the declared layer
order, asserted by a parity spec, so a layer added lands on both
surfaces. The MCP side enters through `Siza.Mcp.mcpEnvOver` and
`gateDispatch` (`siza-client/src/Siza/Mcp.hs`).

### The one process-global exception

`Siza.Agent.Recall` (`siza-client/src/Siza/Agent/Recall.hs`) holds the
recall store in a process-global `IORef` (`unsafePerformIO`,
`NOINLINE`). This is deliberate: two independent elision paths
(Compact and EmitLedger) deposit into it and one tool reads it back.
The cost is a convention: multi-episode drivers must call
`resetRecallStore` between episodes. Everything else with a lifetime
is per-episode state in `Loop/Episode.hs` or per-session state in
`StackSession`.

---

## 3. Life of a write

A source write travels five stages, in the order the types enforce.

1. **Preflight.** `Siza.Preflight.preflight` parses the source with the
   real GHC parser and runs the security scan. Only the `Vetted`
   newtype (constructor unexported) reaches the wire, so unparsed
   source cannot be submitted.
2. **Compile gate.** The server's `gatedCandidate` typechecks the
   candidate against the notebook prefix in a disposable
   reconstruction. A rejection carries the diagnostic; nothing
   commits. The client reads the echo via
   `Siza.Agent.Owned.landedArtifact`, after `Siza.Agent.Ack.reconcileWrite`
   settles any `executing` ack.
3. **Repair cascade.** A red owned cell enters
   `Siza.Agent.Repair.repairOne`: classify the diagnostic
   (`DiagClass`), select tiers (`tiersFor`), generate candidates
   (`Agent/RepairTiers.hs`), apply and verify under the health law
   (`acceptRepair`), revert on failure.
4. **Verify-before-claim.** A "done" claim triggers `drvVerify`:
   `verifyGate` proposes a boolean check, `vetVerdictAgainst`
   mutation-tests it, `Check/Marker.hs` runs it in a bracketed scratch
   cell, `checkVerdict3With` extracts a counterexample on failure.
5. **Verdict channel.** `Siza.Agent.Loop.Verdict.verdictMsg` routes the
   result: only a check that ran and failed may present detail as a
   counterexample; every other outcome exits on the could-not-run
   channel, stamped with a `VerdictClass` marker.

### T2. Stages and policy constants

| Stage | Module | Entry | Policy constants |
|---|---|---|---|
| Parse and security preflight | `siza-client/src/Siza/Preflight.hs` | `preflight` | none; refusal carries diagnostics |
| Write settlement | `siza-client/src/Siza/Agent/Ack.hs` | `reconcileWrite` | `maxAwaitRounds = 8` |
| Compile gate | `src/Sabela/AI/Capabilities/Edit/GateRepair.hs`; echo in `siza-client/src/Siza/Agent/Owned.hs` | `gatedCandidate`; `landedArtifact` | server-side |
| Repair cascade | `siza-client/src/Siza/Agent/Repair.hs` + `src-contract/Sabela/AI/RepairDispatch.hs` | `repairOne` | `repairBudget = 4` |
| Streak hints | `siza-client/src/Siza/Agent/Streak.hs` | `bumpStreak` | `streakThreshold = 3` |
| Verify-before-claim | `siza-client/src/Siza/Agent/Chat/Verify.hs`, `Check/Vet.hs`, `Check/Marker.hs` | `verifyGate`, `vetVerdictAgainst` | `maxOwnedBindings = 8` |
| Verdict channel | `siza-client/src/Siza/Agent/Loop/Verdict.hs` | `verdictMsg` | closed `NoVerdict` vocabulary |

---

## 4. Technique catalogue

One table per context. Entry column gives function and file; files
under `siza-client/src/Siza/` unless stated.

### T3. Episode

| Technique | Entry | Collaborating modules | Invariant | Beginner trap |
|---|---|---|---|---|
| Turn/episode state machine | `runTurns`, `Agent/Loop/Step.hs` | `Loop/Episode.hs`, `Loop/Types.hs` | Every stop carries an enumerated reason tag | `Agent/Loop.hs` is a re-export facade; the machine is `Step.hs`, the state is `Episode.hs` |
| Verify-before-claim wiring | `verifyGate`, `Agent/Chat/Verify.hs` | `Check.hs`, `Check/Vet.hs`, `Loop/Verdict.hs` | A done claim never ends the run unchecked | The gate is the injected `drvVerify`; nothing in the Loop namespace names the implementation |
| Rejection sampling on red writes | `dispatchCall`, `Agent/Loop/Sampling.hs` | `Agent/Sample.hs`, `Loop/Support.hs` | First healthy candidate kept; original restored if none pass | Dark by default: `sampleK` reads `SIZA_SAMPLE_K`, default 1 = off |
| Grounding before re-ask | `groundingMsgs`, `Agent/Loop/Support.hs` | `Loop/Sampling.hs` | Retries cite names the live index returned | `Support.hs` is a grab-bag; nothing signals it is half of the sampling technique |
| Budgeted wrap-up | `wrapUpOnce`, `Agent/Loop/WrapUp.hs` | `Loop/Episode.hs` (`preTurn`) | The marker asserts only what the fired budget guarantees | `WrapUp.hs` also holds write-echo accounting and the stop-line renderer |
| Done-signal probe | `doneSignalProbe`, `Agent/Loop/Episode.hs` | `Loop/Step.hs` | A pass is reported adjacent to the write it confirms | The adjacency invariant is stated only in the doc comment |
| Tool-call recovery | `routeCallWith`, `Agent/ToolRoute.hs` | `Agent/Tools.hs`, `Agent/Loop.hs` (`recoverTurn`) | Unfixable calls return targeted hints | Recovery applies at two hidden seams: `recoverTurn` and dispatch |
| Prose salvage | `noCallTurn`, `Agent/Loop/Step.hs` | `Sabela.AI.Salvage` (sabela) | Fires only when nothing was written yet | The extractor lives in the sabela package |
| Unconfirmed-write accounting | `countUnconfirmed`, `Agent/Loop/WrapUp.hs` | `Sabela.AI.WriteAck` (src-contract) | An unheard write leaves the notebook state reported unknown | Filed beneath the nudge-message builders |

### T4. Gating and repair

| Technique | Entry | Collaborating modules | Invariant | Beginner trap |
|---|---|---|---|---|
| Compile gate | `gatedCandidate`, `src/Sabela/AI/Capabilities/Edit/GateRepair.hs` | `Agent/Owned.hs` (`landedArtifact`) | Source that does not compile never commits | The gate is server-side; siza-client holds only the JSON echo |
| Parse/security preflight | `preflight`, `Preflight.hs` | `Security.hs`, `Lang/Haskell.hs` | Only a `Vetted` reaches the wire | `Siza.Agent.ProviderCheck` is an unrelated provider check |
| Repair tier ladder | `repairOne`, `Agent/Repair.hs` | `Agent/RepairTiers.hs`, `src-contract/Sabela/AI/RepairDispatch.hs` | Diagnostic class selects tiers; a library name never does | The spine is in sabela-contract, a different package |
| Verify-and-revert | `runCascade`, `Agent/Repair.hs` | `RepairDispatch.acceptRepair`, `Agent/Repair/Blocking.hs` | Target heals and no sibling regresses, else revert | `runCascade` is internal; only `repairOne` is the public entry. `substituteAndVerify` is a legacy hole-fit sibling |
| Repair grounding | `discoverModules`, `Agent/RepairLocate.hs` | `Agent/RepairGuard.hs`, `Agent/Repair.hs` (`queryHoleFits`) | Candidates come from tool answers rather than guesses | `fitsBlob` re-renders JSON into GHC's prose format on purpose |
| Marker-cell check | `checkVerdict3With`, `Agent/Check.hs` | `Check/Marker.hs`, `Agent/CheckExtract.hs` | A verdict is read only from the executed cell's output channel | A refusal echoes source containing the pass token; never classify it |
| Check vetting | `vetVerdictAgainst`, `Agent/Check/Vet.hs` | `Check/Gate.hs` | A check no perturbation can falsify proves nothing | "Gate" here names vet refusals; the compile gate is elsewhere |
| Futility guard | `guardDispatch`, `Agent/Futility.hs` | `Agent/Streak.hs`, `Agent/Owned.hs` | An identical call with an identical failure earns a note | Three repetition trackers in three modules with no cross-reference |
| Write settlement | `reconcileWrite`, `Agent/Ack.hs` | `Sabela.AI.WriteAck` | An `executing` ack settles before any consumer reads it | Lives at the dispatch layer while `Scaffold` lives at the episode layer |
| Scaffold pre-seeding | `runScaffoldStage`, `Agent/Scaffold.hs` | `Agent/Deliverable.hs` | A pre-committed cell is disclosed in the transcript | The CSV recipe is a hard-coded literal |

### T5. Discovery

| Technique | Entry | Collaborating modules | Invariant | Beginner trap |
|---|---|---|---|---|
| Multi-source typed grounding | `runDiscoverGoal`, `Agent/DiscoverTool.hs` | `Discover/Interpret.hs`, `Fetch.hs`, `Merge.hs`, `Rank.hs` | Install state never stated stronger than this session proved | The pipeline is in `DiscoverTool.hs`; `Agent/GrammarCards.hs` is a grammar-card namesake |
| Bounded envelope with disclosed eviction | `boundEnvelope`, `Agent/Discover/Envelope.hs` | `Evict.hs`, `Beside.hs`, `ModuleList.hs` | Every shed class is named in `elided` with an exact count | Ladder order in `Envelope.hs`, steps and laws in `Evict.hs` |
| History guard | `guardDiscover`, `Agent/Discover/HistoryGuard.hs` | `Ledger.hs`, `History.hs`, `Dedup.hs` | A duplicate query consults no backend | `SearchLedger` is split across four files |
| Miss ladder | `missAdvice`, `Agent/Discover/MissLadder.hs` | `History.hs`, `Closure.hs`, `Guidance.hs` | Rung 4 and above is a hard stop | The rung counter hides in `History.ledgerRecord`'s where-clause |
| Goal escalation | `standingGoal`, `Agent/Discover/Goal.hs` | `GoalEscalate.hs`, `Steer.hs` | One type-axis query per goal cluster | `_goal`/`_recent` keys are smuggled through tool-call JSON |
| Verify-before-deny | `guardDiscover` (fact ledger), `Agent/Discover/HistoryGuard.hs` | `Facts.hs`, `Resolved.hs`, `FactSelect.hs` | The type checker outranks the index | The machinery is private where-level code inside the guard |

### T6. Context economy

| Technique | Entry | Collaborating modules | Invariant | Beginner trap |
|---|---|---|---|---|
| Transcript compaction | `compactWith`, `Agent/Compact.hs` | `Agent/Chat.hs` (`seedTranscript`) | Stubs are recallable; `mustKeep` content is exempt | Fires only between chat prompts |
| Emit ledger | `emitTurn`, `Agent/EmitLedger.hs` | `Loop/Step.hs`, `Loop.hs` | A block transmits verbatim once; repeats become back-references or deltas | Wired from `Loop/Step.hs`, outside its own directory |
| Outcome distillation | `distillOutcome`, `Agent/OutcomeDistill.hs` | `Agent/Render.hs`, `Mcp.hs` | Bounded head plus the true character count | Two wrappers make one technique look like two features |
| Recall escrow | `answerRecall`, `Agent/Recall.hs` | `Compact.hs`, `EmitLedger.hs` | Every elided byte is readable back by index | Process-global store; `resetRecallStore` is a manual convention |
| Never-drop contract | `mustKeep`, `Agent/Compact.hs` | `EmitLedger.hs` | Diagnostics, verdicts and failures are never elided | Enforced by import convention and substring search over JSON |
| Exemplar memory | `retrieveForPrompt`, `Agent/Exemplars.hs` | `Loop.hs`, `Loop/Episode.hs` | Only verified solutions are saved | Dark unless `SIZA_EXEMPLAR_STORE` is set |

### T7. Observation

| Technique | Entry | Collaborating modules | Invariant | Beginner trap |
|---|---|---|---|---|
| Transcript render/parse round trip | `renderTranscript`, `Agent/Transcript.hs` | `Retro/Episode.hs` | One renderer serves chat export and MCP rounds | Its parser lives in the Retro namespace; the coupling is pinned by a spec |
| Retro metrics | `transcriptMetrics`, `Retro/Metrics.hs` | `Retro/Result.hs`, `Retro/Report.hs` | An unknowable reading is reported unknown | Two metrics stacks share a vocabulary; the flat `Retro.hs` is the shallow one |
| Provenance log | `recordEvent`, `Provenance.hs` | `Provenance/Event.hs`, `Provenance/Log.hs` | Events are append-only; the hash chain is verifiable | Chaining has no live producer (section 7) |
| Transport fault taxonomy | `classifyException`, `Transport/Failure.hs` | `Transport.hs` | Blame is stated per fault class | Reads as plumbing; the blame prose is load-bearing |

---

## 5. Ubiquitous-language glossary

One term, one meaning, and the names that carry it in code, in the
eval harness (`eval/neuro-symbolic/src/Eval/`) and in retired doc
prose.

| Term | Meaning | Code name | Eval-harness name | Retired doc alias |
|---|---|---|---|---|
| Acceptance law | A repair is kept only if the target heals and no sibling regresses | `acceptRepair`, `src-contract/Sabela/AI/RepairDispatch.hs` | the property suite imports the same module | "acceptance law", "health law" |
| Compile gate | Only compiling source commits | `gatedCandidate`, `src/Sabela/AI/Capabilities/Edit/GateRepair.hs` | `Eval.Gate*` grades episodes; `siza-gate` is the held-out fold, a different gate | "G1 gate" |
| Covering check | A proposed boolean check, vetted, run in a marker cell | `drvVerify`; `checkVerdict3With`, `siza-client/src/Siza/Agent/Check.hs` | `Eval.VerdictLint` audits its phrasing; `Eval.FitCheck` grades fit answers | "covering check" |
| Episode | One prompt driven to a stop reason | `runEpisodeSeeded`, `siza-client/src/Siza/Agent/Loop.hs` | `Eval.Episode`, `Eval.GateResult` (`grPass`, `grCtxChars`) | "run" |
| Owned cell | A cell this episode wrote and is answerable for | `OwnedCell`, `siza-client/src/Siza/Agent/Owned.hs` | transcript grading reads the same echo | "tracked cell" |
| Verdict | A harness claim about a check, on a typed channel | `VerdictClass` (`src-contract/Sabela/AI/Verdict.hs`); `verdictMsg` | `Eval.VerdictLint` | "health_gate message" |
| Envelope | The bounded discover payload | `boundEnvelope`, `siza-client/src/Siza/Agent/Discover/Envelope.hs` | `grCtxChars` measures its spend | "truthful one-call answer" |
| Search ledger | Per-session record of resolved, refuted and probed queries | `SearchLedger`, `siza-client/src/Siza/Agent/Discover/Ledger.hs` | transcript lint counts discover calls | "history guard" |

### Known name clashes

- **Gate** means the compile gate everywhere except
  `Siza.Agent.Check.Gate`
  (`siza-client/src/Siza/Agent/Check/Gate.hs`), which holds
  check-vetting refusals.
- **`Siza.Agent.ProviderCheck`** is a provider (Ollama) reachability
  check; the parse gate is `Siza.Preflight`. `Siza.Agent.Preflight`
  survives as a one-line alias for the eval harness.
- **`Siza.Agent.GrammarCards`** is grammar-card surfacing; the search
  tool is `Siza.Agent.DiscoverTool`, and `Siza.Discover` locates the
  running server. `Siza.Agent.Discover` survives as a one-line alias
  for the eval harness.
- **`Siza.Agent.Render`** (outcome text) and
  **`Siza.Agent.RenderContract`** (display repair) are unrelated.
- **`SearchLedger`** (Discovery) and **`EmitLedger`** (Context
  Economy) are different ledgers with different laws.

---

## 6. Layering and lifetimes

### Layering

Each context splits into a pure policy core, an orchestration layer
and adapters.

| Layer | Members |
|---|---|
| Pure policy core | `Sabela.AI.RepairDispatch` (`src-contract/`), `Discover/MissLadder.hs` (`missAdvice`), `Discover/Evict.hs` (steps and `evictionViolations`), `Discover/Rank.hs`, `Agent/Owned.hs` (`redSignature`, `stopDecision`), `Loop/Verdict.hs` (routing) |
| Orchestration | `Loop/Step.hs` (`runTurns`), `Agent/Stack.hs` (`stackDispatch`), `Discover/HistoryGuard.hs` (`guardDiscover`), `Agent/Repair.hs` (`runCascade`) |
| Infrastructure adapters | `Transport.hs` + `Transport/Failure.hs`, `Mcp.hs` + `Mcp/Rpc.hs` + `Mcp/Surface.hs`, `Cli.hs`, `Lang/Haskell.hs` behind `Language.hs` |

The policy cores are pure functions over typed inputs, so the property
suites test them without a kernel. The orchestration layer owns the
`IORef`s. The adapters are technique-free, with the one declared
exception of the blame taxonomy in `Transport/Failure.hs`.

### Lifetimes

Lifetime discipline follows the split described in the [resourcet
overview](https://www.yesodweb.com/blog/2013/03/resourcet-overview):
statically nested lifetimes use bracket shapes; anything else is a
named convention.

- **Marker cells are bracket-shaped.** `Check/Marker.hs` creates a
  scratch cell, reads its output channel, and deletes it again, with
  the refusal path also cleaning up. The scratch cell can never
  outlive the check.
- **Per-episode state dies with the episode.** The `Episode` record
  (`Loop/Episode.hs`), the `EmitLedger` and the `SearchLedger` are
  `IORef`s created at episode start and dropped at the end.
- **The recall store is the one dynamic lifetime.** It is
  process-global; `resetRecallStore` between episodes is the stated
  convention (section 2).

### T8. Budget-constant index

Context spend is the scarcest resource, so every budget constant is
indexed here with its home module.

| Constant | Value | Home module |
|---|---|---|
| `envelopeCharBudget` | 2000 | `siza-client/src/Siza/Agent/Discover/Envelope.hs` |
| `factsByteBudget` | 800 | `siza-client/src/Siza/Agent/Discover/FactSelect.hs` |
| `outcomeHeadBudget` | 1200 | `siza-client/src/Siza/Agent/OutcomeDistill.hs` |
| `outcomeCharBudget` | 2500 | `siza-client/src/Siza/Agent/OutcomeDistill.hs` |
| render truncation (`trunc`) | 6000 | `siza-client/src/Siza/Agent/Render.hs` |
| `stubFloor` | 200 | `siza-client/src/Siza/Agent/Compact.hs` |
| `previewBudget` | 400 | `siza-client/src/Siza/Agent/Compact.hs` |
| `blockFloor` | 160 | `siza-client/src/Siza/Agent/EmitLedger.hs` |
| `backRefLimit` | 140 | `siza-client/src/Siza/Agent/EmitLedger.hs` |
| `noteCharBudget` | 2500 | `siza-client/src/Siza/Agent/Stack/Call.hs` |

---

## 7. Status

What a reader should know before trusting a code path they have only
read.

### Dark by default

| Switch | Feature | Entry |
|---|---|---|
| `SIZA_SAMPLE_K` (default 1 = off) | Rejection sampling on red writes | `dispatchCall`, `siza-client/src/Siza/Agent/Loop/Sampling.hs`; kernel in `Agent/Sample.hs` |
| `SIZA_EXEMPLAR_STORE` (off unless set) | Cross-episode exemplar memory | `retrieveForPrompt`, `siza-client/src/Siza/Agent/Exemplars.hs` |

Reading these paths in a default run exercises neither.

### Dead or unwired

- `Siza.Agent.NoteLedger`
  (`siza-client/src/Siza/Agent/NoteLedger.hs`) is imported only by
  specs. It reads like a live gate and is dormant.
- Provenance hash-chaining (`chainEvents`, `verifyChain` in
  `siza-client/src/Siza/Provenance.hs`) has no live producer: the only
  logger stubs the chain fields, and the chat and MCP loops record no
  events.
- `substituteAndVerify` in `siza-client/src/Siza/Agent/Repair.hs` is a
  legacy hole-fit sibling of the tier cascade.

