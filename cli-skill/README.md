# siza — Sabela CLI pair mode

Pair a separate Claude Code terminal with a running Sabela notebook. Claude can list, read, run, and edit cells in the notebook you have open in your browser.

## How it works

Sabela exposes its AI tools over REST at `/api/ai/*`. A compiled, typed Haskell client (`exe:siza`, in the `siza-client` package) speaks that contract; the `siza` / `siza.cmd` shim in this directory locates and execs it. The client validates tool calls against the typed contract, runs a client-side pre-flight parse + security scan on mutating code, and records each call to a provenance log.

- `GET /api/ai/health` — liveness probe
- `GET /api/ai/tools` — the tools + their JSON schemas
- `GET /api/ai/notebook` — structured cell snapshot
- `POST /api/ai/tool` — invoke a tool; body `{ name, input }`, optional `X-Sabela-Session` header

On startup, Sabela writes `~/.local/state/sabela/servers/<port>.json` so the skill can auto-discover running servers.

## Repo layout

```
cli-skill/
├── .claude-plugin/marketplace.json     # marketplace entry
└── plugins/siza/
    ├── .claude-plugin/plugin.json      # plugin manifest (optional, for metadata)
    └── skills/siza/
        ├── SKILL.md                    # the prompt loaded into Claude when triggered
        └── scripts/
            ├── siza                    # shim over the compiled typed client (exe:siza)
            └── siza.cmd                # Windows shim
```

Claude Code's plugin loader only discovers `SKILL.md` files at `<plugin>/skills/<name>/SKILL.md`. A bare `SKILL.md` at the plugin root is silently ignored.

## Running locally

1. **Start Sabela.** In terminal A:
   ```bash
   cd /path/to/sabela
   cabal run                       # localhost:3000, no auth
   ```
   Open the URL it prints in your browser and load a notebook.

2. **Build the typed client.** The skill drives a compiled binary, so build it once:
   ```bash
   cd /path/to/sabela
   cabal build exe:siza
   ```

3. **Install the skill in a second terminal.** In terminal B, from inside Claude Code:
   ```
   /plugin marketplace add /path/to/sabela/cli-skill
   /plugin install siza@sabela-tools
   /reload-plugins
   ```
   Confirm the reload line reports `… · 1 skill · …` (not `0 skills`). Then ask Claude things like _"what cells are in my Sabela notebook?"_, _"analyse the data in cell 4"_, or _"run cell 3 and tell me what it prints."_

4. **Point the shim at the client.** When installed, the plugin runs from the plugin cache, not your checkout, so the shim can't auto-find the binary. Set ONE of these (in your shell profile):
   ```bash
   export SABELA_REPO=/path/to/sabela            # shim resolves via `cabal list-bin`
   # or, explicit (required if you built with non-default flags like -O2):
   export SIZA_BIN="$(cd /path/to/sabela && cabal list-bin exe:siza)"
   ```

5. **Or skip the plugin and use the script directly** from the checkout — the in-repo `siza` shim auto-finds the binary, no env needed:
   ```bash
   cli-skill/plugins/siza/skills/siza/scripts/siza discover
   cli-skill/plugins/siza/skills/siza/scripts/siza tool list_cells
   cli-skill/plugins/siza/skills/siza/scripts/siza tool read_cell '{"cell_id":1}'
   cli-skill/plugins/siza/skills/siza/scripts/siza await-idle      # block until the kernel settles
   ```

## Running with a bearer token (hub / remote / shared machine)

Set `SABELA_AI_TOKEN` before starting Sabela. All `/api/ai/*` requests then require `Authorization: Bearer <token>`:

```bash
# Terminal A — server
SABELA_AI_TOKEN=$(openssl rand -hex 16) cabal run

# Terminal B — client
export SABELA_AI_TOKEN=<same value>
cli-skill/plugins/siza/skills/siza/scripts/siza tool list_cells
```

The rest of the UI (browser notebook, SSE events, static files) stays unauthenticated — only the AI bridge is gated.

## Environment variables

| Var | Scope | Purpose |
|-----|-------|---------|
| `SIZA_BIN` | shim | Explicit path to the built `siza` binary. Most reliable; required if you built with non-default flags (`-O2`). |
| `SABELA_REPO` | shim | Your sabela checkout; the shim resolves the binary via `cabal list-bin exe:siza`. Use when installed from the plugin cache. |
| `SABELA_AI_TOKEN` | server + client | Bearer token. Unset → local, no auth. |
| `SABELA_URL` | client | Override auto-discovery (e.g. `http://host:3000`). |
| `SABELA_SESSION` | client | `X-Sabela-Session` value; isolates `explore_result` handles. Defaults to a stable per-terminal id. |

## Raw curl examples

Everything Claude does you can do by hand:

```bash
# Health
curl -s localhost:3000/api/ai/health | jq

# All tool schemas
curl -s localhost:3000/api/ai/tools | jq '.[].name'

# Call a tool
curl -s localhost:3000/api/ai/tool \
  -H 'content-type: application/json' \
  -d '{"name":"read_cell","input":{"cell_id":1}}' | jq
```

With a token, add `-H "Authorization: Bearer $SABELA_AI_TOKEN"` to each request.

## Tool reference

[`plugins/siza/skills/siza/SKILL.md`](plugins/siza/skills/siza/SKILL.md) is the prose Claude reads when the skill triggers. It documents:

- the tools with input / output JSON shapes,
- a worked example for analysing data ("discover → list_cells → read_cell → scratchpad → insert_cell → match the typed `outcome` tag"),
- decision tree for `propose_edit` vs `replace_cell_source` vs `insert_cell`,
- the typed cell-result outcome (`Succeeded` / `Raised` / `Rejected` / `Aborted`), with outputs and warnings,
- handle lifecycle and the `explore_result` ops (`head` / `tail` / `slice` / `grep`),
- scratchpad rules (per-language, per-turn, no top-level `let`, 3-error circuit breaker),
- `expected_hash` optimistic concurrency and how to recover from a hash mismatch,
- the 4096-token response cap and how to split large `propose_edit` payloads.

If you're editing the skill, that's the file.
