---
name: siza
description: Drive a running Sabela Haskell notebook from Claude Code. Use whenever the user asks to list, read, run, edit, debug, or analyse cells; explore a dataset; propose changes; or pair-program on a notebook they have open in the browser (typically localhost:3000).
allowed-tools: Bash
---

# siza — Sabela notebook pair programming

The user has a Sabela notebook open in their browser. You are pairing with them: every change you make appears live in their UI. The notebook state is shared, including a long-lived GHCi session.

## Hard rule

**Every notebook operation goes through `${CLAUDE_PLUGIN_ROOT}/skills/siza/scripts/siza`.** Do not `curl` `/api/cell/*`, `/api/load`, `/api/notebook`, or any non-`/api/ai/*` endpoint, and do not `ps aux` looking for the server. Raw endpoints bypass the AI bridge: they skip the browser-refresh broadcast, skip optimistic-concurrency checks on cell hashes, and skip large-output handle stashing. If `siza` doesn't expose what you need, tell the user — don't reach around it. **One sanctioned exception:** setting widget state (slider/dropdown/lasso) via `POST /api/widget`, since siza has no widget-set tool — see [reference/widgets.md](reference/widgets.md). It still needs user sign-off before you `curl` it.

**Never edit the notebook's `.md` file directly with file tools (Write/Edit/sed).** The live session the user is looking at is the source of truth; editing the file on disk diverges from it, skips validation (the cell never runs), and skips the browser broadcast. Make every cell change through siza (`insert_cell`/`replace_cell_source`/`delete_cell`) so it is applied, auto-run, and visible. Saving the session back to disk is the user's action in the browser, not a file you write.

## Discovery and invoking

The primary entrypoint is `${CLAUDE_PLUGIN_ROOT}/skills/siza/scripts/siza` — a thin shim over the compiled typed client (`exe:siza`). It validates tool names and JSON against the typed contract, runs the pre-flight parse + security scan on mutating code, and records each call to a provenance log (see [Provenance](#provenance)).

```bash
${CLAUDE_PLUGIN_ROOT}/skills/siza/scripts/siza discover            # JSON array of live servers; the first is the target
${CLAUDE_PLUGIN_ROOT}/skills/siza/scripts/siza health              # probe the first live server's health
${CLAUDE_PLUGIN_ROOT}/skills/siza/scripts/siza tool <name> '<json_input>'
${CLAUDE_PLUGIN_ROOT}/skills/siza/scripts/siza check [--strict] [-|FILE]   # pre-flight parse + security scan of cell source
${CLAUDE_PLUGIN_ROOT}/skills/siza/scripts/siza annotate <cell_id> [--source]   # infer signatures for a cell's unsigned binds
${CLAUDE_PLUGIN_ROOT}/skills/siza/scripts/siza await-idle [SECONDS]   # block until the kernel settles to idle (re-loops past timedOut)
```

`siza discover` lists each `~/.local/state/sabela/servers/<port>.json` that responds to `/api/ai/health`. Every subcommand reads the same env vars, so you don't pass URLs around: `SABELA_URL` short-circuits discovery; `SABELA_AI_TOKEN` is the bearer token when `authRequired: true`; `SABELA_SESSION` is the `X-Sabela-Session` header (defaults to a per-terminal id; isolates `explore_result` handles between clients). For an online notebook behind the hub, `siza login` handles auth — see [Hub-hosted notebooks](#hub-hosted-notebooks). `$SIZA_BIN` overrides where the shim finds the binary; otherwise it resolves `cabal list-bin exe:siza` from the repo root (build it once with `cabal build exe:siza`).

### Hub-hosted notebooks

To drive a notebook running online behind the hub (e.g. `https://sabela.datahaskell.com`) instead of `localhost:3000`, run `siza login` once. It opens the hub's authorize page in the browser; the user (already signed into the hub there) clicks **Approve**, and the hub mints a **short-lived token** bound to that browser session. siza saves it locally and sends it as the bearer on every `/api/ai/*` call. The hub recognises the token at its boundary and strips `Authorization` before forwarding, so the backend stays no-auth.

```bash
export SABELA_URL="https://sabela.datahaskell.com"
${CLAUDE_PLUGIN_ROOT}/skills/siza/scripts/siza login        # browser-approved; saves a short-lived token
${CLAUDE_PLUGIN_ROOT}/skills/siza/scripts/siza tool list_cells
${CLAUDE_PLUGIN_ROOT}/skills/siza/scripts/siza logout       # forget the saved token
```

`siza login [HUB_URL]` defaults the URL to `$SABELA_URL`. Setting `SABELA_URL` also makes `discover` probe the hub directly (the local registry is empty for online notebooks), and `newConn` auto-attaches the saved token whenever it targets that hub. `X-Sabela-Session` is **not** stripped by the proxy, so per-terminal session isolation still works through the hub. The non-localhost warning on stderr is expected here. The token expires on its own (hub `HUB_CLI_TOKEN_TTL_MIN`, default 8h) and dies with the browser session it was approved from — when calls start returning the login redirect or siza prints `saved hub token expired`, run `siza login` again.

For `siza tool`, `<json_input>` defaults to `{}` if omitted. Output is pretty JSON on stdout. Exit code is non-zero when the tool returns `isError: true`. Pass `--strict` to a mutating `tool` (or to `check`) to **block** on a denied capability instead of only advising.

`siza check` parses and security-scans cell source **without touching the kernel** — use it before a `propose_edit`/`replace_cell_source`/`insert_cell` to catch syntax errors and flagged operations up front. `siza annotate <cell_id>` infers type signatures for a cell's unsigned top-level binds (`--source` emits the annotated source instead of a report).

## Provenance

Each `siza tool` call is recorded to an append-only JSONL provenance log under `~/.local/state/sabela/sessions/<notebook-id>/<session-id>.jsonl` (honouring `$XDG_STATE_HOME`), beside the server registry. One line per call records the time, session, notebook, actor, tool name, input, pre-flight verdict, outcome, kernel-state-before, and the `(session, ebGeneration)` correlation key. The server writes its own authoritative log at the tool seam; the client log is the intent layer. This is a side effect — it never changes a tool's output or blocks a call (a log-write failure is swallowed). Review a session's metrics with `siza retro <file>` or `siza retro --notebook <nb> --session <sid>`.

## Which mutation tool?

| You want to… | Use |
|---|---|
| Edit a cell **the user wrote** | `propose_edit` — pending patch they accept/reject in the UI. Always pass `expected_hash`. |
| Edit a cell **you just inserted** | `replace_cell_source` — applied + auto-run immediately. Iterate on your own scaffolding. Pass `expected_hash`. |
| Add a new cell | `insert_cell`. `after_cell_id: -1` puts it at the top. |
| Remove a cell | `delete_cell` — immediate, no undo. Widgets disappear from the rendered page. |

Never `delete_cell` + `insert_cell` to "edit" — use `replace_cell_source` (yours) or `propose_edit` (theirs).

## Tool reference

All tools accept a JSON object input; outputs below are abbreviated.

| Tool | Input | Output | Notes |
|---|---|---|---|
| `list_cells` | `{}` | `{title, cells:[{id,hash,position,type,lang,firstLine,hasError,dirty}]}` | `firstLine` truncated to 80 chars. `dirty: true` means source changed but not run. |
| `read_cell` | `{cell_id}` | `{id,hash,type,lang,source,outputs,error}` | Full source + rendered outputs. Large outputs may be a handle. |
| `read_cell_output` | `{cell_id}` | `{id,outputs,error}` | Cheaper than `read_cell` when you already know the source. |
| `find_cells_by_content` | `{pattern}` | `{matches:[{id,lang,matchingLines:[{line,text}]}]}` | Case-sensitive substring. Up to 5 matching lines per cell, 120 chars each. |
| `insert_cell` | `{after_cell_id,source,cell_type,language?}` | `{cellId,hash,execution}` | Auto-runs Haskell code cells; `execution` is a typed `CellResult` (`{outcome,outputs,warnings,ok}`), `null` for Python or prose. **Pass `cell_type` AND `language` on _every_ insert — prose cells included.** Omitting `cell_type` fails with `Unknown cell_type: .` and omitting `language` fails with `Unknown language: .` even for a `ProseCell` (use `"Haskell"` if there's no real language to give). |
| `delete_cell` | `{cell_id}` | `{deleted:true,cellId}` | Irreversible. |
| `replace_cell_source` | `{cell_id,new_source,expected_hash?}` | `{cellId,hash,execution}` | Auto-runs; `execution` is a typed `CellResult`. Pass `expected_hash` to detect concurrent edits. |
| `propose_edit` | `{cell_id,new_source,expected_hash?}` | `{editId,cellId,status:"pending"}` | Does **not** apply or run. Re-proposing supersedes prior pending edit on the same cell. |
| `execute_cell` | `{cell_id}` | `{cellId,outcome,outputs,warnings,ok}` | Reactive: downstream cells re-run automatically. ~120s timeout. |
| `kernel_status` | `{}` | `{state:{state,ksGen,building},ksGen,ebGeneration}` | Lock-free; answers even while a cell holds the run-lock. See [Kernel discipline](#kernel-discipline). |
| `interrupt` | `{}` | `{interrupted:true}` | Best-effort group SIGINT to a runaway cell. |
| `kernel_restart` | `{}` | `{restartInitiated:true}` | Forked; returns immediately, then poll `kernel_status`. **Destructive** — wipes every binding and compiled module. |
| `export_notebook` | `{}` | `{title, cells:[{id,position,type,lang,source,…}]}` | Every cell's source in one call — re-sync after a human edits. |
| `scratchpad` | `{code,language?}` | `{stdout,stderr}` | Throwaway, isolated session. See the explore recipe. |
| `ghci_query` | `{op:"type"\|"info"\|"kind"\|"browse"\|"doc",arg}` | `{op,arg,result}` | Cheap introspection of the live Haskell session. No side effects. Requires at least one cell already executed. |
| `api_reference` | `{module?}` | `{module,reference}` | The embedded card. See [reference/dataframe-granite.md](reference/dataframe-granite.md). |
| `explore_result` | `{handle_id,op:"head"\|"tail"\|"slice"\|"grep",n?,from?,to?,pattern?}` | `{lines,totalLines}` or `{hits,totalLines}` | Drill into a stashed large payload. **Handles expire at turn end** — drill or extract the same turn. `slice` is 1-based inclusive. |

You can fetch the authoritative schemas at any time with `curl -s "$base/api/ai/tools" | jq`.

## Cell-result and kernel-status semantics

The tool surface is **sum-typed**: a cell result is one `outcome` tag and a kernel status is one `state` tag, so you match a tag instead of recombining loose booleans.

**Cell result.** For Haskell code-cell mutations (`insert_cell`, `replace_cell_source`, `execute_cell`) the `execution` (or `execute_cell` result) is a `CellResult`:

- `outcome: {tag, …}` — one of `Succeeded`, `Raised {message}` (runtime exception), `Rejected {errors:[{line?,col?,message}]}` (structured compile errors), or `Aborted {reason}` (`Interrupted` | `Superseded` | `TimedOut`).
- `outputs: [{mime, output}]` — rendered outputs. Individual outputs >40 lines or >4 KB become handles (`{handleId, summary, totalLines, totalBytes}`).
- `warnings: [{line?, col?, message}]` — non-fatal diagnostics.
- `ok: bool` — `true` iff `outcome.tag == "Succeeded"`. Match `outcome.tag` for *why* it failed; read `ok` for the quick yes/no.

Python and prose cells return `execution: null` — branch on cell type first, don't read `execution.ok`. If `ok: false`, **fix in the same turn**; downstream cells won't run.

**Kernel status.** `kernel_status` returns `{state: {state, ksGen, building}, ksGen, ebGeneration}`. `state.state` is one tag — `cold` (no session), `idle`, `executing` (a cell or query holds the run-lock), or `building` (off-lock build work: a cabal-env install, a cold-start GHCi spawn, or a `-- compile` module build). `building` and `executing` are independent axes that can co-occur: while a cell runs the tag stays `executing` and `state.building` carries the rebuild, so read `state.building` when you need both. **Only `state == "idle"` is genuinely settled.** `ksGen` bumps only on a GHCi restart, so a constant `ksGen` through a long run is normal — it is not a wedge signal. `ebGeneration` is a distinct event-bus fence, separate from `ksGen`.

## Reactivity

Editing or running a cell reruns every cell that textually depends on it (Sabela tracks `let`, `data`, `type`, `newtype`, `class`, and value bindings against later cells' identifier use). You don't manually rerun dependents. Dependency tracking is **textual, not semantic** — renaming a symbol without updating callers is allowed; the breakage surfaces on next run.

**The asymmetry to watch:** a cell that *failed to compile* is not recorded as the provider of the names it would have bound, so the dependency edge is missing. After you **fix** a previously-broken upstream cell, its downstream consumers can stay red — re-run them by hand once. (Conversely, one broken upstream cell floods every downstream cell with `Variable not in scope`; trace those back to the single root rather than treating each as separate.)

## Kernel discipline

There is **one** GHCi kernel per notebook behind a single run-lock. The notebook, the session, and the lock are shared per notebook: browser runs, widget updates, run-all, and your tools all drive the same lock. A human taking over is a normal event.

- **One call at a time.** A second tool call that needs the kernel while a cell holds the lock is **bounced immediately** with `busy` — it does **not** queue or stack server-side. So a `busy` bounce is a wasted round-trip that tells you nothing new: serialise (issue one, wait for its response, then the next) rather than spray.
- **A timeout is not a failure and not a cancel.** When `execute_cell` returns a `curl` timeout, the cell is very likely still running: the server waits on the cell's result for up to ~130 s while the client's ~60 s request timeout abandons the HTTP response first. The work continues; you have only stopped watching. **DO NOT retry** — a retry bounces `busy`, and killing the client does not cancel the cell already running on the server. Wait for the kernel to drain by polling the **lock-free** `kernel_status` (or `list_cells`, `/api/ai/health`); they answer even while a cell holds the lock, which is how you tell "busy" from "wedged".
- **Never attribute warm-up to the cell.** The *first* `execute_cell` after a (re)start, a `-- compile` edit, or a `-- cabal:` edit blocks on the cold-start **and** the `-O2` project compile (often 1–3 min) *before your cell even runs*. Do not benchmark a cell from a cold call. Warm the kernel first (run a trivial cell, or poll `kernel_status` to idle), then time the cell you care about.
- **Beware the Bash auto-background trap.** Long commands get auto-backgrounded, so an impatient retry of a slow loop launches a *second concurrent* loop. The moment the kernel goes idle a stray loop can fire a run you did not intend, and killing the client does **not** cancel a cell already on the server. **Never retry a backgrounded siza loop** — let the first finish.
- **Heavy exploration goes in the `scratchpad`,** never a long `execute_cell` against the live kernel — a blocking cell wedges the *whole* notebook for the user too.
- **A `kernel_status` change you did not cause means a human acted** (a `state` flip you didn't trigger, or a bumped `ksGen`). Back off and re-sync with one `export_notebook` call rather than retrying.

### `-- cabal:` and `-- compile` costs

- **`-- cabal:` edit = full restart.** Editing a `-- cabal:` line rebuilds the package env and **silently restarts GHCi**, wiping every binding and every loaded `-- compile` module — a cold session, one to three minutes. `list_cells` flags (`hasError`/`dirty`) go stale across a restart; don't trust them. **Batch all dep additions up front** so you pay the restart + recompile once.
- **`-- compile` cell = multi-minute `-O2` relaunch.** A `-- compile` line relaunches the *entire* kernel as object code and wipes the interpreted context. The server **re-warms it for you**: after the wipe it automatically re-runs every interpreted cell in dependency order (see Part 2.4 of the design). So fire one run and await idle — don't hand-sequence a re-warm. A compile that *fails* still escalates (downstream cells come back red; read them and fix the root), and a *no-op* edit that doesn't reload won't escalate.

## No rebinding a name across cells

Sabela tracks top-level definitions globally, so binding a name a *different* cell already defines fails — e.g. `df <- …` (or `let df = …`) when `df` is bound elsewhere returns `Duplicate definition: 'df' is already defined in cell N (which takes precedence)`. To transform a value, bind a **new** name (`let featured = … df …`) and thread it forward; `propose_edit` the downstream consumers to read the new name. (Re-running the *same* cell that owns a binding is fine; this only bites when a second cell redefines it.)

## Recipes

Each recipe is a small composition over the primitives above.

- **Fix a red cell in the same turn** (the highest-frequency loop): read the error from `errors`/`error`, diagnose, `replace_cell_source` (yours) or `propose_edit` (theirs), re-check `ok`. Mind the reactivity asymmetry — re-run a downstream consumer by hand once after fixing a previously-broken upstream cell.
- **Edit a user's cell:** `read_cell` to capture the `hash`, then `propose_edit(expected_hash)`. They accept or reject in the UI.
- **Edit your own cell:** `replace_cell_source(expected_hash)` — applied and auto-run immediately.
- **Run and wait** = run → await-idle → read. Issue `execute_cell` (or a mutation), then `scripts/siza await-idle [SECONDS]` (a bounded ~45s long-poll on the execution-done fence; the subcommand re-loops past `timedOut` to an overall budget) until its `status.state.state` is `idle`, then `read_cell_output` for the settled result. The underlying `siza tool await_idle` stays available if you want each poll visible — the calls stay visible so you see the `busy` and partial-output states; there is deliberately no fused run-and-wait verb.
- **Sync the notebook:** one `export_notebook` call beats N `read_cell` calls, and is the right move after any `kernel_status` change you did not cause.
- **Explore:** `ghci_query` against the live session for type and scope (it sees `-- cabal:` deps and the real CWD). Use `scratchpad` only for self-contained slices — **the scratchpad has its own package env and CWD and cannot resolve `-- cabal:` deps or relative data paths.** Write `x = 10` at top level there, not `let x = 10`; non-empty stderr flips `isError: true`; after 3 consecutive scratchpad errors in a turn the response carries a `_sabelaHint` — stop and ask the user.
- **Safe rename:** tracking is textual, so a rename that misses a caller surfaces only on next run. Dry-run the rename in `scratchpad` first.
- **Add deps:** batch the `-- cabal:` edits into one change, expect one restart (one to three minutes), then fire one run and await idle. See the cost note above.
- **Recover a wedge** (conservative, human-gated): a true wedge is a call that never returns while `kernel_status` shows `state == "idle"` with an *unchanged* `ksGen` — distinct from a long compile (`state == "building"`) or a long run (`state == "executing"`), neither of which is wedged. Do not restart on a hunch; a restart is irreversible and wipes all work. Sequence: `interrupt` first (best-effort, async — may not free a non-interruptible FFI call or a tight loop); re-check `kernel_status`; only then propose `kernel_restart` to the user; after a restart, wait for `ksGen` to strictly increase; if it is still unresponsive, tell the user to restart the server process (a fresh server also resets `SABELA_CELL_TIMEOUT_SECONDS`).

## Concurrent-edit recovery

If a mutation rejects with a hash mismatch (`{"error":"Hash mismatch — re-read the cell and retry.","cellId":5,"currentHash":"…","expectedHash":"…"}`), the user edited the cell out from under you. Re-`read_cell`, decide whether your change still applies, and retry with the fresh hash.

## Other gotchas

- **`api_reference` first for unfamiliar libs**, and prefer the lightest package (`dataframe-core` over the `dataframe` umbrella). Both live in [reference/dataframe-granite.md](reference/dataframe-granite.md).
- **In-cell Haskell traps** (the `fromList` type application, the `dropWhileEnd` no-op, the `do`-block rule, reconciling non-additive metrics) live in [reference/haskell-cell-traps.md](reference/haskell-cell-traps.md) — read it when a cell type-checks but behaves wrongly.
- **Write clear, notebook-friendly code:** separate logic from display, one concept per cell, top-level bindings over `do let`, narrate findings not housekeeping. The full guidance is in [reference/notebook-style.md](reference/notebook-style.md).
- **Token cap.** Responses cap at 4096 tokens; very large `propose_edit` / `replace_cell_source` payloads can truncate mid-JSON. Split: `insert_cell` a small stub, then patch it in follow-up calls.
- **The live session is the source of truth; the disk `.md` is a written artifact.** "Save" is the user's browser action, not a siza tool. **DO NOT** author a fixed `.md` to disk and ask for a reload as recovery — file edits won't reach the live kernel, and they diverge silently (the open browser auto-saves its old cells back over your file, a reload renumbers every cell id, and cached outputs are dropped).
- **Non-localhost URLs trigger a stderr warning** from the client. If you see it, double-check with the user that the remote target is intentional.
