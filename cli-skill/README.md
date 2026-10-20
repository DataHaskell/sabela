# siza — Sabela CLI pair mode

Pair a separate Claude Code terminal with a running Sabela notebook. Claude can list, read, run, and edit cells in the notebook you have open in your browser.

## How it works

Sabela exposes its AI tools over REST at `/api/ai/*`. A compiled, typed Haskell client (`exe:siza`, in the `siza-client` package) speaks that contract, and `siza mcp` serves it over the Model Context Protocol on stdio. The plugin's `.mcp.json` registers that server, so Claude Code sees the notebook as ordinary typed MCP tools (`mcp__plugin_siza_siza__list_cells`, `…__insert_cell`, and so on) instead of building shell command strings. The server validates every call against the typed contract, runs a client-side pre-flight parse + security scan on mutating code, and records each call to a provenance log under `~/.local/state/sabela/sessions/`.

- `GET /api/ai/health` — liveness probe
- `GET /api/ai/tools` — the tools + their JSON schemas
- `GET /api/ai/notebook` — structured cell snapshot
- `POST /api/ai/tool` — invoke a tool; body `{ name, input }`, optional `X-Sabela-Session` header

On startup, Sabela writes `~/.local/state/sabela/servers/<port>.json` so siza can auto-discover running servers.

## Repo layout

```
cli-skill/
├── .claude-plugin/marketplace.json     # marketplace entry
└── plugins/siza/
    ├── .claude-plugin/plugin.json      # plugin manifest
    ├── .mcp.json                       # registers `siza mcp` as the plugin's MCP server
    └── skills/siza/
        └── SKILL.md
```

Claude Code's plugin loader only discovers `SKILL.md` files at `<plugin>/skills/<name>/SKILL.md`. A bare `SKILL.md` at the plugin root is silently ignored.

## Running locally

1. **Start Sabela.** In terminal A:
   ```bash
   cd /path/to/sabela
   cabal run                       # localhost:3000, no auth
   ```
   Open the URL it prints in your browser and load a notebook.

2. **Put `siza` on your PATH.** The plugin spawns `siza mcp`, so the binary must be resolvable by name:
   ```bash
   cd /path/to/sabela
   cabal install exe:siza --overwrite-policy=always    # symlinks into ~/.cabal/bin
   ```
   Make sure `~/.cabal/bin` is on `PATH` for the shell that launches Claude Code.

3. **Install the plugin.** In terminal B, from inside Claude Code:
   ```
   /plugin marketplace add /path/to/sabela/cli-skill
   /plugin install siza@sabela-tools
   /reload-plugins
   ```
   `/mcp` should list `plugin:siza:siza` as connected. Then ask Claude things like _"what cells are in my Sabela notebook?"_, _"analyse the data in cell 4"_, or _"run cell 3 and tell me what it prints."_

4. **Use the CLI directly** when you want to drive the notebook by hand:
   ```bash
   siza discover
   siza tool list_cells
   siza tool read_cell '{"cell_id":1}'
   siza await-idle      # block until the kernel settles
   ```

## Running with a bearer token (hub / remote / shared machine)

Set `SABELA_AI_TOKEN` before starting Sabela. All `/api/ai/*` requests then require `Authorization: Bearer <token>`:

```bash
# Terminal A — server
SABELA_AI_TOKEN=$(openssl rand -hex 16) cabal run

# Terminal B — Claude Code, with the same value exported so the MCP server inherits it
export SABELA_AI_TOKEN=<same value>
```

The rest of the UI (browser notebook, SSE events, static files) stays unauthenticated — only the AI bridge is gated. For a notebook behind the hub, run `siza login` once in a terminal (it needs a human to approve in the browser); the MCP server picks up the saved token.

## Environment variables

The MCP server inherits the environment of the shell that launched Claude Code.

| Var | Scope | Purpose |
|-----|-------|---------|
| `SABELA_AI_TOKEN` | server + client | Bearer token. Unset → local, no auth. |
| `SABELA_URL` | client | Override auto-discovery (e.g. `http://host:3000` or a hub URL). |
| `SABELA_SESSION` | client | `X-Sabela-Session` value; isolates result handles. Defaults to a stable per-terminal id. |
| `SABELA_TOOL_TIMEOUT` | client | Tool-call HTTP timeout in seconds; `0` = no timeout. The plugin sets `0` so a long `execute_cell` is not abandoned. |

## Other MCP clients

`siza mcp` is a plain stdio MCP server, so goose, Claude Desktop, opencode, or any other MCP host can drive the notebook with the same config shape: command `siza`, args `["mcp"]`, env `SABELA_TOOL_TIMEOUT=0` (and `SABELA_URL` if not local). Prefer the **`execute_cell` then `await_idle` (don't retry)** idiom for long cells; a retry bounces `busy`.

### goose

```bash
export SABELA_URL=http://localhost:3000 SABELA_TOOL_TIMEOUT=0
goose run --with-extension "siza mcp" \
  -t "Call list_cells, then insert a cell defining factorial :: Integer -> Integer and run it."
```

Persistent config (`~/.config/goose/config.yaml`):

```yaml
extensions:
  siza:
    name: siza
    type: stdio
    cmd: siza
    args: ["mcp"]
    enabled: true
    envs:
      SABELA_URL: "http://localhost:3000"
      SABELA_TOOL_TIMEOUT: "0"
```

### Claude Desktop (`claude_desktop_config.json`)

```json
{
  "mcpServers": {
    "siza": {
      "command": "siza",
      "args": ["mcp"],
      "env": { "SABELA_URL": "http://localhost:3000", "SABELA_TOOL_TIMEOUT": "0" }
    }
  }
}
```

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

The authoritative catalogue is the live `tools/list` handshake against `siza mcp`; each tool's description documents its arguments and result. [`plugins/siza/skills/siza/SKILL.md`](plugins/siza/skills/siza/SKILL.md) is the short prose Claude reads when the skill triggers: the hard rules, which mutation tool to pick, and kernel discipline. If you're editing the skill, that's the file.
