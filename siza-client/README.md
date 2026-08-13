<h1 align="center">siza-client</h1>
<p align="center">a typed client and agent loop for live Sabela notebooks</p>

siza-client connects an agent to a running [Sabela](../README.md) notebook.
It ships a command-line client, `siza`, that validates every tool call
against the typed contract before the call leaves the machine, an
interactive driver, `siza chat`, that runs a local model against the
notebook, and the agent loop that the eval harness in
`eval/neuro-symbolic` replays in batch.

The loop is built for small local models. A small model fails in two ways:
it fills its context with tool output, or it claims more than it verified.
Most of the code here exists to make those two failures hard, and the
types, not the prompts, do the enforcing.

## Use

Build once, then drive everything through the one executable.

```bash
cabal build exe:siza
```

| Command | Answers |
|---|---|
| `siza discover` | which live servers answer; the first is the target |
| `siza health` | whether the target responds, and its work directory |
| `siza tool <name> '<json>'` | one validated tool call; the exit code is non-zero when the tool reports an error |
| `siza check [FILE]` | the parse and security verdict for cell source, without touching the kernel |
| `siza annotate <cell>` | inferred signatures for a cell's unsigned top-level binds |
| `siza await-idle [secs]` | nothing until the kernel settles to idle |
| `siza login`, `siza logout` | a short-lived token for a notebook behind the hub |
| `siza mcp` | the same tool surface, served over MCP on stdio for any MCP host |
| `siza tools` | the tools an agent is offered, with usage |
| `siza chat` | an interactive local-model session against the notebook |
| `siza retro <file>` | session metrics recomputed from a provenance log |

Every mutating call is parsed and security-scanned before it is sent, and
every call is appended to a provenance log under
`~/.local/state/sabela/sessions/`. The log is an intent record: writing it
never changes a result and never blocks a call.

## Two rules the whole client obeys

The harness returns results, never advice. No tool output tells the model
what to do next, because steering text trains the model to wait for
steering text.

Nothing is removed silently. When a reply is bounded, the eviction is
disclosed and the removed content stays reachable; a model that cannot
tell a short answer from a shortened answer re-reads everything.

## The shape of the code

The transport, CLI, and MCP plumbing are adapters. The technique lives in
five contexts. Each context has one entry point, a small set of core
types, and one rule that the types enforce; the rule is stated below as
the sentence the code makes true.

| Context | Where | Entry point | The rule |
|---|---|---|---|
| Episode | `src/Siza/Agent/Loop*` | `Loop.Step.runTurns` | every stop carries a typed reason |
| Gating and repair | `Preflight.hs`, `Agent/Owned.hs`, `Agent/Repair*`, `Agent/Check*`, `Agent/Futility.hs`, `Agent/Streak.hs` | `Repair.repairOne`, `Chat.Verify.verifyGate` | a repair is accepted only when the target improves and no sibling regresses |
| Discovery | `Agent/Discover/`, `Agent/DiscoverTool.hs` | `DiscoverTool.runDiscoverGoal` | evidence is never overstated, and eviction is always disclosed |
| Context economy | `Agent/Compact.hs`, `EmitLedger.hs`, `OutcomeDistill.hs`, `Recall.hs`, `Exemplars.hs` | `Compact.compactWith` | no elision drops a diagnostic, a verdict, or a failure |
| Observation | `Provenance*`, `Retro*`, `Agent/Transcript.hs` | `Retro.Metrics.transcriptMetrics` | measure the transcript, never steer it |

`src/Siza/Agent/Loop.hs` is a re-export facade plus transcript seeding.
The one exception to technique-free adapters is the fault-blame taxonomy
in `Transport/Failure.hs`.

## What the types guarantee

| Guarantee | How | Where |
|---|---|---|
| only parsed source reaches the wire | `Vetted` has no exported constructor; `preflight` is the only way in | `src/Siza/Preflight.hs` |
| only compiling code counts as delivered | the server gate decides; `landedArtifact` reads its echo | `src/Siza/Agent/Owned.hs` |
| a done claim needs a vetted, executed check | `CheckResult` and `NoVerdict`; `vetVerdictAgainst` mutation-tests the check | `src/Siza/Agent/Check.hs`, `Check/Vet.hs` |
| repair is a total function of diagnostic class | `DiagClass` to `RepairTier` dispatch | `../src-contract/Sabela/AI/RepairDispatch.hs`, used by `Agent/Repair.hs` |
| verdicts leave on a typed channel | `VerdictClass`, routed by `verdictMsg` | `src/Siza/Agent/Loop/Verdict.hs` |

## Two packages, one gate

The compile gate itself, the repair policy spine, `Salvage`, the verdict
markers, and `WriteAck` live in the sabela server (`../src`) and the
shared contract (`../src-contract`), not here. siza-client holds the
client half: the parse and security preflight (`Siza.Preflight`) and the
gate's echo (`Siza.Agent.Owned`). When you go looking for the gate, look
in sabela.

## Namesakes

Several modules share a surname and are unrelated.

- `Siza.Preflight` is the parse and security gate. The Ollama
  reachability check is `Siza.Agent.ProviderCheck`.
- `Siza.Discover` locates the running server. `Siza.Agent.DiscoverTool`
  is the search tool, and `Siza.Agent.GrammarCards` surfaces grammar
  cards. The `Siza.Agent.Discover.*` submodules belong to the search
  pipeline.
- `Siza.Agent.Render` produces outcome text. `Siza.Agent.RenderContract`
  is display repair.

The old names `Siza.Agent.Preflight` and `Siza.Agent.Discover` survive as
one-line re-export shims because the eval harness imports them. Delete
the shims when eval moves to the new names.

## Off by default

| Switch | Feature | Default |
|---|---|---|
| `SIZA_SAMPLE_K` | rejection sampling (`Loop/Sampling.hs`, `Sample.hs`) | 1, which is off |
| `SIZA_EXEMPLAR_STORE` | exemplar memory (`Agent/Exemplars.hs`) | off unless set |

Kept deliberately despite having no live producer: `NoteLedger` is
exercised only by specs, and provenance hash-chaining has no caller.

## Reading path

Read in this order. Each stop is a module and its entry function.

1. `Siza.Agent.Loop.Types`: the `Driver` record, the episode's handle.
2. `Siza.Agent.Loop`: `runEpisodeSeeded`, the facade.
3. `Siza.Agent.Loop.Step`: `runTurns`, the turn machine.
4. `Siza.Preflight`: `preflight`, the only door to the wire.
5. `Siza.Agent.Owned`: `landedArtifact`, the compile-gate echo.
6. `Siza.Agent.Repair`: `repairOne`, with the spine in
   `../src-contract/Sabela/AI/RepairDispatch.hs`.
7. `Siza.Agent.Chat.Verify`: `verifyGate`, propose a check.
8. `Siza.Agent.Check.Vet`: `vetVerdictAgainst`, mutation-test it.
9. `Siza.Agent.Check.Marker`: `runMarkerWith`, run it under channel
   discipline.
10. `Siza.Agent.Loop.Verdict`: `verdictMsg`, route the claim.
11. `Siza.Agent.ToolRoute`: `routeCallWith`, call recovery.
12. `Siza.Agent.Stack`: `stackDispatch`, the surface stack.
13. `Siza.Agent.Stack.Call`: `runToolCall`, per-call repair.
14. `Siza.Agent.DiscoverTool`: `runDiscoverGoal`, the search pipeline.
15. `Siza.Agent.Discover.Envelope`: `boundEnvelope`, the bounded reply.
16. `Siza.Agent.Discover.HistoryGuard`: `guardDiscover`, the search
    ledger.
17. `Siza.Agent.Compact`, `EmitLedger`, `OutcomeDistill`, `Recall`:
    `compactWith`, `emitTurn`, `distillOutcome`, `answerRecall`.

## Further reading

`Siza.Guide` (`src/Siza/Guide.hs`) is the compiler-checked table of
contents. Every claim in it is a re-export, so it cannot drift.
`../docs/siza-architecture.md` holds the long-form architecture notes.
