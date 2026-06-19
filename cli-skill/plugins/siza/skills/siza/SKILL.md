---
name: siza
description: Drive a running Sabela Haskell notebook from Claude Code. Use whenever the user asks to list, read, run, edit, debug, or analyse cells; explore a dataset; propose changes; or pair-program on a notebook they have open in the browser (typically localhost:3000).
allowed-tools: Bash
---

# siza — Sabela notebook pair programming

The user has a Sabela notebook open in their browser. You are pairing with them: every change you make appears live in their UI. The notebook state is shared, including a long-lived GHCi session.

## Hard rule

**Every notebook operation goes through `${CLAUDE_PLUGIN_ROOT}/skills/siza/scripts/siza-tool.sh`.** Do not `curl` `/api/cell/*`, `/api/load`, `/api/notebook`, or any non-`/api/ai/*` endpoint, and do not `ps aux` looking for the server. Raw endpoints bypass the AI bridge: they skip the browser-refresh broadcast, skip optimistic-concurrency checks on cell hashes, and skip large-output handle stashing. If `siza-tool.sh` doesn't expose what you need, tell the user — don't reach around it. **One sanctioned exception:** setting widget state (slider/dropdown/lasso) via `POST /api/widget`, since siza has no widget-set tool — see [Driving widgets](#driving-widgets) below. It still needs user sign-off before you `curl` it.

## Discovery

```bash
${CLAUDE_PLUGIN_ROOT}/skills/siza/scripts/siza-discover.sh
```

Returns a JSON array of live servers (one entry per `~/.local/state/sabela/servers/<port>.json` that responds to `/api/ai/health`). Pick the first. Honors:

- `SABELA_URL` — short-circuits discovery and probes a specific URL.
- `SABELA_AI_TOKEN` — bearer token, required if the server has `authRequired: true`.
- `SABELA_SESSION` — `X-Sabela-Session` header value, defaults to a per-terminal id. Isolates `explore_result` handles between concurrent clients.

`siza-tool.sh` reads the same env vars, so you don't pass URLs around.

## Invoking tools

```bash
${CLAUDE_PLUGIN_ROOT}/skills/siza/scripts/siza-tool.sh <tool_name> '<json_input>'
```

`<json_input>` defaults to `{}` if omitted. Output is pretty JSON on stdout. Exit code is non-zero when the tool returns `isError: true`.

## Worked example — "analyse X in my notebook"

```bash
# 1. Confirm a server is up.
${CLAUDE_PLUGIN_ROOT}/skills/siza/scripts/siza-discover.sh

# 2. See what's already in the notebook.
${CLAUDE_PLUGIN_ROOT}/skills/siza/scripts/siza-tool.sh list_cells

# 3. Read the cells that look relevant (note their hashes).
${CLAUDE_PLUGIN_ROOT}/skills/siza/scripts/siza-tool.sh read_cell '{"cell_id":7}'

# 4. Dry-run new code in the throwaway scratchpad before touching the notebook.
${CLAUDE_PLUGIN_ROOT}/skills/siza/scripts/siza-tool.sh scratchpad \
  '{"language":"Haskell","code":"import qualified DataFrame as D\nlangs <- D.readCsv \"./examples/data/languages.csv\"\nD.dimensions langs"}'

# 5. If the dry-run is clean, insert the cell after the last relevant one.
#    Always pass BOTH cell_type and language — required on every cell, prose included (see gotcha).
${CLAUDE_PLUGIN_ROOT}/skills/siza/scripts/siza-tool.sh insert_cell \
  '{"after_cell_id":7,"cell_type":"CodeCell","language":"Haskell","source":"import qualified DataFrame as D\nlangs <- D.readCsv \"./examples/data/languages.csv\"\nD.take 5 langs"}'

# 6. Read execution.ok in the response. If false, fix in place with replace_cell_source
#    using the hash returned by insert_cell — never delete + re-insert.
```

## Which mutation tool?

| You want to… | Use |
|---|---|
| Edit a cell **the user wrote** | `propose_edit` — pending patch they accept/reject in the UI. Always pass `expected_hash`. |
| Edit a cell **you just inserted** | `replace_cell_source` — applied + auto-run immediately. Iterate on your own scaffolding. |
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
| `insert_cell` | `{after_cell_id,source,cell_type,language?}` | `{cellId,hash,execution}` | Auto-runs Haskell code cells; `execution: null` for Python or prose. **Pass `cell_type` AND `language` on _every_ insert — prose cells included.** The schema marks them optional, but omitting `cell_type` fails with `Unknown cell_type: .` and omitting `language` fails with `Unknown language: .` even for a `ProseCell` (use `"Haskell"` if there's no real language to give). |
| `delete_cell` | `{cell_id}` | `{deleted:true,cellId}` | Irreversible. |
| `replace_cell_source` | `{cell_id,new_source,expected_hash?}` | `{cellId,hash,execution}` | Auto-runs. Pass `expected_hash` to detect concurrent edits. |
| `propose_edit` | `{cell_id,new_source,expected_hash?}` | `{editId,cellId,status:"pending"}` | Does **not** apply or run. Re-proposing supersedes prior pending edit on the same cell. |
| `execute_cell` | `{cell_id}` | `{cellId,outputs,error,errors}` | Reactive: downstream cells re-run automatically. ~120s timeout. |
| `scratchpad` | `{code,language?}` | `{stdout,stderr}` | Throwaway, isolated session. See "Scratchpad rules" below. |
| `ghci_query` | `{op:"type"\|"info"\|"kind"\|"browse"\|"doc",arg}` | `{op,arg,result}` | Cheap introspection of the live Haskell session. No side effects. Requires at least one cell already executed. |
| `api_reference` | `{module?}` | `{module,reference}` | Pre-generated `:browse` for DataFrame, DataFrame.Typed, DataFrame.Functions, DataFrame.Display.Web.Plot, Granite.Svg. Substring match on section header; empty returns all. |
| `explore_result` | `{handle_id,op:"head"\|"tail"\|"slice"\|"grep",n?,from?,to?,pattern?}` | `{lines,totalLines}` or `{hits,totalLines}` | Drill into a stashed large payload. See "Handle lifecycle" below. |

You can fetch the authoritative schemas at any time with `curl -s "$base/api/ai/tools" | jq` — `siza-discover.sh` gives you `$base`.

## Execution result semantics

For Haskell code-cell mutations (`insert_cell`, `replace_cell_source`, `execute_cell`), the response carries:

- `outputs: [{mime, output}]` — rendered outputs. Individual outputs >40 lines or >4 KB become handles (`{handleId, summary, totalLines, totalBytes}`).
- `error: string | null` — aggregated runtime stderr.
- `errors: [{line?, col?, message}]` — **structured compile errors**, possibly empty.
- `ok = (errors is empty) AND (error is null)`. A cell can have both at once (compile error with warnings).

Python cells and prose cells return `execution: null`. Don't read `execution.ok` on those — branch on cell type first.

If `ok: false`, **fix in the same turn** before moving on. Downstream cells won't run.

## Reactivity

Editing or running a cell reruns every cell that textually depends on it (Sabela tracks `let`, `data`, `type`, `newtype`, `class`, and value bindings against later cells' identifier use). You don't need to manually rerun dependents. Dependency tracking is **textual, not semantic** — renaming a symbol without updating callers is allowed; the breakage surfaces on next run. Always dry-run renames in `scratchpad` first.

**One asymmetry to watch:** a cell that *failed to compile* is not recorded as the provider of the names it would have bound, so the dependency edge is missing. After you **fix** a previously-broken upstream cell, its downstream consumers can stay red — re-run them by hand once. (A cell going green for the first time doesn't retro-trigger the cells that were waiting on it.) Conversely, a single broken upstream cell produces a *flood* of `Variable not in scope` errors across every downstream cell; trace the not-in-scope errors back to the one root cell rather than treating each as separate.

## Handle lifecycle

Large payloads are auto-compacted:

- Individual cell outputs: stashed when >40 lines or >4 KB.
- Whole tool results: stashed when the JSON exceeds ~8 KB; you'll see `_compacted: true` and a `_large.handleId` in the response.

Drill in with `explore_result`:

- `head n` / `tail n` — first/last n lines (default 20).
- `slice from to` — **1-based inclusive** range.
- `grep pattern` — substring match, up to 50 hits.

**Handles expire at turn end.** Drill in the same turn or extract what you need into a notebook cell before the turn closes.

## Scratchpad rules

- **Per-language, per-turn.** Switching language kills the previous scratchpad. State doesn't survive across turns.
- **Haskell:** write `x = 10` at top level, **not** `let x = 10`. The runner uses scripths, which wraps multi-line definitions in `:{ … :}` for you. `import …`, `data …`, `class …`, etc. all work normally.
- **The scratchpad can't resolve `-- cabal:` deps** — it has a fixed package env and its own CWD. Code that needs an extra package (or relative data paths) can only be dry-run in the live notebook, not the scratchpad. Use `ghci_query` against the live session for type/scope checks instead.
- **Non-empty stderr flips `isError: true`** even if stdout looks fine. Don't ignore it.
- **Circuit breaker:** after 3 consecutive scratchpad errors in one turn, the response gets a `_sabelaHint`. Stop and ask the user — don't keep retrying.

## Concurrent-edit recovery

If a mutation rejects with a hash mismatch:

```json
{"error":"Hash mismatch — re-read the cell and retry.","cellId":5,"currentHash":"…","expectedHash":"…"}
```

The user edited the cell out from under you. Re-`read_cell`, decide whether your change still applies, and retry with the fresh hash.

## Kernel discipline — concurrency, timeouts, restarts

There is **one** GHCi kernel per notebook behind a single run-lock with no admission control. Almost all lost wall-clock in practice comes from mishandling it. These are hard rules, not suggestions:

- **NEVER run `execute_cell` (or any mutation) concurrently.** Issue one, wait for its response, then the next. In-flight requests queue **server-side**; the client `curl` gives up at 60 s but the **server keeps processing the backlog**, so the queue outlives every timeout.
- **A timeout is not a failure and not a cancel.** When `execute_cell` returns a `curl` timeout, the cell is very likely still running (a cold `-O2` compile or a >60 s computation easily exceeds the curl ceiling). **STOP. DO NOT retry** — each retry adds another job to the queue and makes the wedge worse. Wait for the kernel to drain by polling the **lock-free** endpoints (`siza-tool.sh list_cells`, or `/api/ai/health`); they answer even while a cell holds the lock, which is how you tell "busy" from "wedged".
- **Beware the Bash auto-background trap.** Long-running commands get auto-backgrounded, so an impatient retry of a slow loop (e.g. a per-cell read/sync loop) launches a *second concurrent* loop — several pile up and flood the queue. Killing the client (`pkill`/`Ctrl-C`) does **NOT** cancel the server-side work already queued. **Never retry a backgrounded siza loop** — let the first finish.
- **Heavy exploration goes in the `scratchpad`,** never a long `execute_cell` against the live kernel — a blocking or long cell wedges the *whole* notebook for the user too.
- **There is no `kernel_restart` over the bridge.** A truly wedged kernel only recovers via a **browser kernel restart** by the user. Tell them; don't keep probing.

### After a `-- cabal:` change or a restart

Editing a `-- cabal:` line rebuilds the package env and **silently restarts GHCi**, wiping every binding and every loaded `-- compile` module — a cold session. The traps:

- **`list_cells` flags go stale.** `hasError`/`dirty` reflect *pre-restart* state, so cells can report `ok` while their bindings are gone and downstream cells fail `Not in scope`. Don't trust the flags across a restart.
- **The reactive re-run may not fully fire.** After any restart, manually re-run the `-- compile` cells and binding cells **in dependency order** before touching downstream cells.
- **Batch all dep additions up front** so you pay the restart + recompile once, not mid-build. A `-- cabal:` edit that lands while a cell is mid-flight races the running cell.

## Other gotchas

- **No rebinding a name across cells.** Sabela tracks top-level definitions globally, so binding a name a *different* cell already defines fails — e.g. `df <- …` (or `let df = …`) when `df` is bound elsewhere returns `Duplicate definition: 'df' is already defined in cell N (which takes precedence)`. To transform a value, bind a **new** name (`let featured = … df …`) and thread it forward — `propose_edit` the downstream consumers to read the new name instead of trying to overwrite the original. (Re-running the *same* cell that owns a binding is fine; this only bites when a second cell redefines it.)
- **Cabal metadata.** Cells declare deps with `-- cabal:` comments. Changing those triggers a package-env rebuild and a silent GHCi restart — see [Kernel discipline](#kernel-discipline--concurrency-timeouts-restarts) for the cold-session recovery. Batch dep changes.
- **Prefer the lightest package.** `build-depends: dataframe` is an **umbrella** that drags in plotting/typed/ML/web (and granite) — a heavy env whose `-O2` object-code startup can hang GHCi long enough to need a browser restart. For plain frames use **`dataframe-core`** (base/containers/primitive/text/vector only). Nothing in the toolchain warns you the umbrella is heavy or that a `-core` exists — reach for the narrow package first.
- **Compiled cells (`-- compile`).** A cell containing a `-- compile` line (or `-- compile: Module.Name`) is built into a generated module at `-O2` — native code, 10–300× faster for tight pure-Haskell loops. **Cost to weigh first:** a single `-- compile` line relaunches the *entire* kernel as `ghc --interactive -fobject-code -O2 -fexpose-all-unfoldings`, so even interpreted cells pay a multi-minute object-code startup, and every later restart pays it again — `-- compile` **plus** a heavy dep like `dataframe` is the classic hard-wedge combination. Rules: declarations only (no bare expressions, no `x <- …`, no widgets/`display*`); downstream cells use the names directly; a compiled cell cannot use an interpreted cell's name (pass values as arguments); cells sharing a `-- compile:` name merge into one module and may be mutually recursive. Editing a compiled cell costs a recompile (~0.5–1s warm) and re-runs dependents of the affected modules — batch compiled-cell edits, and tweak call sites in interpreted cells (which never recompiles). Propose it only when a pure-Haskell loop is the real bottleneck; skip it when the time is inside native library calls (large tensor ops, dataframe internals).
- **Single GHCi kernel per notebook.** A long-running `execute_cell` blocks every other cell and every siza tool call (they all wait on the run-lock). See [Kernel discipline](#kernel-discipline--concurrency-timeouts-restarts).
- **Token cap.** Responses cap at 4096 tokens; very large `propose_edit` / `replace_cell_source` payloads can truncate mid-JSON. Split: `insert_cell` an empty cell (or a small stub), then patch it in follow-up calls.
- **`api_reference` first for unfamiliar libs.** Before guessing DataFrame / Granite APIs, call `api_reference '{"module":"DataFrame.Typed"}'` (or the relevant slice). Cheaper and more current than recalling from training. Plotting shold primarily be done with Granite and you should browser the API.
- **The live session is the source of truth; the disk `.md` is a written artifact.** "Save" is a separate endpoint not exposed via siza — if the user asks to save, tell them to save in the browser. **DO NOT** author a fixed `.md` to disk and ask for a reload as a recovery path. File edits **WILL NOT** reach the live kernel, and they diverge silently: the still-open browser session auto-saves its *old* cells back over your file, a reload renumbers every cell id, and the saved file's cached outputs are dropped (forcing a full rerun that can re-trigger the heavy compile you were escaping). Keep all work flowing through the live session via siza tools.
- **Non-localhost URLs trigger a stderr warning** from `siza-tool.sh`. If you see it, double-check with the user that the remote target is intentional before sending data.

### In-cell Haskell traps

A handful of things type-check or run but behave wrongly in the live session. These cost real debugging time because the failure is silent or the error misleading.

- **`fromList` needs a type application.** `DataFrame`'s `fromList` is return-type-polymorphic (an `Unboxable`/`Columnable` constraint it can't infer from the list), so a bare `fromList xs` fails with `GHC-80003: Non type-variable argument in the constraint: SBoolI (… Unboxable a1)` **and a flatly wrong fix suggestion** ("use record field `TruncateConfig.maxRows`"). Pin it at the call site: `fromList @Text xs`, `fromList @Double xs`. (A top-level signature on the whole builder also works but is heavier.)
- **`Data.Text.dropWhileEnd` can silently no-op in-session.** `T.dropWhileEnd C.isDigit "abc99"` has returned `"abc99"` unchanged in a live cell (suspected fusion/RULES interaction; unconfirmed) — a *correctness* trap with no error, no warning. If a trailing-strip silently does nothing, rewrite as `T.reverse . T.dropWhile p . T.reverse`.
- **Multiple effects need a `do` block.** Two bare `putStrLn (…)` statements on consecutive lines parse as one application — `putStrLn (…) putStrLn (…)` — and fail with a confusing "applied to N visible arguments". Some statement pairs (e.g. consecutive `showTable …`) *do* split, so the behaviour is inconsistent; the safe rule is **multiple effectful statements → wrap them in a `do` block**.
- **Reconcile non-additive metrics before you headline them.** Ranking by an inherited/subtree/`%`/ratio metric as if it summed produces a plausible, confident, *wrong* story (a measured case: an inherited-cost metric summed to ~80× the whole program). Nothing in the toolchain pushes back. Before narrating "the biggest contributor is X", check a reconciliation invariant — e.g. Σ(individual deltas) == total delta — in a scratch cell.

## Driving widgets

Widgets — `scatterSelectWith` lassoes, `slider`, `dropdown`, `checkbox`, `textInput` — read their state from a **server-side `WidgetStore`**, which `Sabela.Bridge.widgetPreamble` writes into the in-session `_sabelaWidgetRef` **before every cell run**. So you **cannot** set a widget by `modifyIORef _sabelaWidgetRef` from a cell — it's clobbered on the next run. The only writer is the browser bridge, and siza exposes no widget-set tool.

When the user asks you to set a widget's value from chat — a lasso's selection, a slider position, a dropdown choice — use the browser's own endpoint, the lone sanctioned raw POST (get user OK first):

```bash
base="$(${CLAUDE_PLUGIN_ROOT}/skills/siza/scripts/siza-discover.sh | jq -r '.[0].baseUrl')"
curl -s -o /dev/null -w 'HTTP %{http_code}\n' -X POST "$base/api/widget" \
  -H 'Content-Type: application/json' \
  --data "$(jq -nc --rawfile v /tmp/sel.txt \
    '{wuCellId:<widget-cell-id>, wuName:"<widget-name>", wuValue:($v|rtrimstr("\n"))}')"
```

This is safe in the ways the Hard rule worries about: `setWidgetH` writes the store, then `handleWidgetCell` → `executeAffected wuCellId` re-renders the widget cell **and** reruns downstream cells reactively + broadcasts over SSE — identical to a real browser interaction. There's no cell-source hash to guard.

- **`wuCellId`** = the cell that *renders* the widget (the `scatterSelectWith`/`slider`/… cell). Must be that cell, or its highlight/control won't refresh.
- **`wuName`** = the widget's name argument — the first string you passed it (the `name` in `scatterSelectWith name …`, `slider name …`, etc.).
- **`wuValue`** = the value `show`-serialized, read back via `reads`: a lasso is a `[Int]` literal of **0-based positions** into the points list passed to the widget (= DataFrame row order if `pts` was built from columns in order); a slider an int; dropdown/text a string.
- **Big selections:** compute the indices in a throwaway cell and `writeFile "/tmp/sel.txt" (show idx)`, then feed the file to `jq --rawfile` rather than pasting thousands of ints through tool output. Delete the temp cell afterward.
