---
name: siza
description: Drive a running Sabela Haskell notebook from Claude Code. Use whenever the user asks to list, read, run, edit, debug, or analyse cells; explore a dataset; propose changes; or pair-program on a notebook they have open in the browser (typically localhost:3000).
allowed-tools: Bash
---

# siza — Sabela notebook pair programming

The user has a Sabela notebook open in their browser. You are pairing with them: every change you make appears live in their UI. The notebook state is shared, including a long-lived GHCi session.

## Hard rule

**Every notebook operation goes through `${CLAUDE_PLUGIN_ROOT}/skills/siza/scripts/siza-tool.sh`.** Do not `curl` `/api/cell/*`, `/api/load`, `/api/notebook`, or any non-`/api/ai/*` endpoint, and do not `ps aux` looking for the server. Raw endpoints bypass the AI bridge: they skip the browser-refresh broadcast, skip optimistic-concurrency checks on cell hashes, and skip large-output handle stashing. If `siza-tool.sh` doesn't expose what you need, tell the user — don't reach around it.

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
${CLAUDE_PLUGIN_ROOT}/skills/siza/scripts/siza-tool.sh insert_cell \
  '{"after_cell_id":7,"source":"import qualified DataFrame as D\nlangs <- D.readCsv \"./examples/data/languages.csv\"\nD.take 5 langs"}'

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
| `insert_cell` | `{after_cell_id,source,cell_type?,language?}` | `{cellId,hash,execution}` | Auto-runs Haskell code cells; `execution: null` for Python or prose. |
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
- **Non-empty stderr flips `isError: true`** even if stdout looks fine. Don't ignore it.
- **Circuit breaker:** after 3 consecutive scratchpad errors in one turn, the response gets a `_sabelaHint`. Stop and ask the user — don't keep retrying.

## Concurrent-edit recovery

If a mutation rejects with a hash mismatch:

```json
{"error":"Hash mismatch — re-read the cell and retry.","cellId":5,"currentHash":"…","expectedHash":"…"}
```

The user edited the cell out from under you. Re-`read_cell`, decide whether your change still applies, and retry with the fresh hash.

## Other gotchas

- **Cabal metadata.** Cells declare deps with `-- cabal:` comments. Changing those triggers a package-env rebuild and a GHCi restart — slow. Batch dep changes when you can.
- **Single GHCi kernel per notebook.** A long-running `execute_cell` blocks every other cell. Use `scratchpad` for heavy exploration; only commit to a notebook cell once you're confident.
- **Token cap.** Responses cap at 4096 tokens; very large `propose_edit` / `replace_cell_source` payloads can truncate mid-JSON. Split: `insert_cell` an empty cell (or a small stub), then patch it in follow-up calls.
- **`api_reference` first for unfamiliar libs.** Before guessing DataFrame / Granite APIs, call `api_reference '{"module":"DataFrame.Typed"}'` (or the relevant slice). Cheaper and more current than recalling from training.
- **Notebooks are Markdown on disk.** "Save" is a separate endpoint not exposed via siza — if the user asks to save, tell them to save in the browser.
- **Non-localhost URLs trigger a stderr warning** from `siza-tool.sh`. If you see it, double-check with the user that the remote target is intentional before sending data.
