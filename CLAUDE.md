# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**Sabela** is a reactive notebook environment for Haskell — a web-based IDE where users write executable Haskell code mixed with Markdown prose. Notebooks are stored as plain Markdown files. The name means "to respond" in Ndebele, reflecting its reactive execution model.

## Commands

```bash
# Build and run
cabal run                                  # Starts server on localhost:3000
cabal run sabela -- 3000 .                  # sabela <port> <work-dir> [global-file] [extra-pkgs...]

# Tests
cabal test                                 # Runs HSpec test suite

# Lint and format
./scripts/lint.sh                          # HLint on src/, app/, test/
./scripts/lint.sh --fix                    # Auto-fix lint issues on edited files
./scripts/format.sh                        # fourmolu (Haskell) + prettier (frontend)
./scripts/format.sh --haskell-only         # Skip the prettier pass
./scripts/format.sh --frontend-only        # Only static/*.{html,js,mjs,css}
./scripts/format.sh --check                # Dry-run; non-zero exit on drift
```

## Frontend (modular sources → bundled embeds)

The three served pages — `static/index.html` (the editor), `static/dashboard.html`,
and `static/slideshow.html` — are **generated build artifacts**. The binary
embeds them verbatim via Template Haskell (`embedFile` in
`Sabela.Server.Static`), so the single executable still ships fully
self-contained with no runtime asset fetches. Do **not** hand-edit those files;
edit the sources and rebuild.

### Layout

The editable sources live under `static/src/<page>/`:

```
static/src/index/
  index.html          # shell: <head>/<body> markup + ordered <link>/<script>/include refs
  css/*.css           # style partials, inlined in order (cascade preserved)
  js/*.js             # script partials, inlined in order (one shared global scope)
  html/icons.svg      # inline SVG icon sprite (an asset; inlined, not fetched)
  html/modals.html    # body markup fragment
```

`dashboard/` and `slideshow/` follow the same shape. The numeric filename
prefixes (`01-…`, `02-…`) encode inline order; the shell lists them explicitly,
so order is what the shell says, not directory sort.

### The bundler

`tools/build-frontend.mjs` reads each `static/src/<page>/<page>.html` shell and
inlines its **local** references, coalescing consecutive ones into single
blocks, then writes `static/<page>.html`:

- `<link rel="stylesheet" href="css/…">` → one `<style>` block
- `<script src="js/…"></script>` → one `<script>` block
- `<!-- include: html/… -->` → the fragment inlined verbatim
- remote (`https://…` CDN) tags pass through untouched

The JS partials are plain (non-module) scripts that share one global scope when
inlined — exactly as the original single `<script>` did — so functions stay
reachable from inline `onclick=` handlers. Keep them that way (no ES-module
`import`/`export` in these partials).

### Workflow

- `make frontend` (or `node tools/build-frontend.mjs`) — rebuild the embeds after
  editing any partial. **Rebuild sabela (`cabal build`) afterwards** so the
  TH-embedded pages refresh.
- `make frontend-check` / `node tools/build-frontend.mjs --check` — fail if any
  served page is stale vs its partials (run in CI / pre-commit).
- `./scripts/format.sh` formats the partials with prettier and then rebuilds the
  embeds; `--check` verifies both. Generated `static/*.html` are excluded from
  prettier (see `.prettierignore`) — they are artifacts.
- `dashboard.html` / `slideshow.html` double as **templates**: the static-export
  endpoints replace the `/*__SABELA_INJECT__*/` placeholder (in
  `js/head-extra.js`) with notebook JSON (`Sabela.Dashboard`). Preserve that
  exact placeholder string.
- Per the module-size cap, keep each `js/*.js` partial ≤ 300 lines
  (`scripts/check-module-size.sh` enforces it); split a growing one at a
  function boundary into a new ordered partial.

## Architecture

The system has three layers: a Haskell backend (Servant/Warp), a static frontend (`static/index.html` and friends, bundled from modular sources — see [Frontend](#frontend-modular-sources--bundled-embeds)), and a long-lived GHCi subprocess.

### Module Responsibilities

| Module | Role |
|--------|------|
| `Sabela.Model` | Core domain types: `Notebook`, `Cell`, `NotebookEvent`, `CellError` |
| `Sabela.State` | Composed `App` record with focused subsystems (`Environment`, `EventBus`, `NotebookStore`, `SessionManager`, `DependencyTracker`, `WidgetStore`, `BridgeStore`) |
| `Sabela.Session` | Spawns and manages a GHCi subprocess; uses marker-based output capture to separate cell results |
| `Sabela.LeanSession` | Lean 4 LSP client session management |
| `Sabela.PythonSession` | Python REPL subprocess management |
| `Sabela.SessionTypes` | `SessionBackend` record-of-functions interface shared by all language backends |
| `Sabela.Handlers` | Cell execution, reactivity engine, dependency tracking, session lifecycle |
| `Sabela.Reactivity` | Pure execution planning: `ExecutionPlan`, topological ordering, error message generation |
| `Sabela.Deps` | Pure metadata collection and merging for cabal dependencies |
| `Sabela.Bridge` | Pure preamble generation for cross-language bridge values and widgets |
| `Sabela.Topo` | Dependency graph construction and topological sort |
| `Sabela.Server` | Servant HTTP API, SSE event streaming, file explorer, IDE helpers |
| `Sabela.Api` | Request/response DTOs |
| `Sabela.Output` | MIME type parsing, rich output helpers (HTML, SVG, Markdown, LaTeX, JSON) |
| `Sabela.Errors` | Parses GHCi stderr to extract structured error locations |
| `Sabela.LeanLsp` | LSP wire protocol types and helpers for Lean integration |

### Key Data Flow

1. User edits a cell → `PUT /api/cell/{id}`
2. `handleCellEdit` bumps generation, runs `selectAffected` to find downstream cells
3. If package dependencies changed, `installAndRestart` resolves packages via `scripths`, restarts GHCi
4. For each affected cell: parse source → render to GHCi script → `runBlock` → parse output/errors → broadcast `EvCellResult` via SSE
5. Frontend receives events on `/api/events` (SSE) and updates the UI

### Reactivity Model

Dependency tracking is heuristic (textual, not compiler-based):
- `extractDefs`: parses `let`, `data`, `type`, `newtype`, `class`, and value bindings from a cell
- `extractTokens`: identifies all identifiers used in a cell
- `selectAffected`: builds a dependency DAG and finds cells transitively downstream of the edited cell

### Session Management

- Single long-lived GHCi process per notebook
- Output captured by a background thread using `TBQueue`; cells separated by unique `SABELA_MARKER_N` sentinels
- `withMVar` locks ensure only one cell runs at a time
- `displayPrelude` is injected at session start to enable rich MIME output

### Concurrency Primitives

- `MVar`: mutable state (notebook, session)
- `IORef`: counters, sets (installed deps/extensions)
- `TBQueue`: bounded queues for session output lines
- `TChan`: broadcast channel for SSE events (duplicated per client)
- `forkIO`: background tasks (dependency installation, cell execution)

### Dependency Resolution

Cells declare package metadata with `-- cabal:` comments. The `scripths` library merges metadata across all cells, then `cabal install --lib` creates a package environment in a temp directory. GHCi is invoked with `--package-env=<path>`.

### File Format

Notebooks are plain Markdown files. Code blocks become executable cells; prose between them becomes Markdown cells. On save, cells are reassembled back into Markdown.

## Key External Dependency

`scripths` (version `>= 0.2.0.1`) handles: markdown parsing, package/dependency resolution, and GHCi script rendering. Most cell execution logic in `Handlers.hs` delegates to it.

## Conventions

- Module namespace: `Sabela.*`
- HTTP handler functions use `H` suffix (e.g., `runCellH`, `getNotebookH`)
- State parameters named `app` (App) or `rn` (ReactiveNotebook)
- All file paths are canonicalized and security-checked to be within the work directory
- Formatter: fourmolu with 4-space indent, 80-column limit, leading commas (see `fourmolu.yaml`); frontend uses prettier with 2-space indent, 100-col default / 120-col for HTML (see `.prettierrc.json`). `scripts/format.sh` runs both.
- **Comments must be top-level (haddock or block above the declaration) and ≤3 lines,
  unless they contain a code example.** No inline narrative comments inside expressions
  or argument lists — refactor the explanation onto the binding's haddock instead.
  `scripts/check-comments.sh` enforces this on changed lines (diff vs `origin/main`);
  existing comments are grandfathered, so the rule tightens on new/changed code.
- **Module size: keep every module ≤ 300 lines** — split an oversized module into focused
  submodules (handler groups, per-tool modules, a big literal in its own module) rather
  than letting it grow. Applies to Haskell modules and the `static/*.js` frontend modules;
  run `scripts/check-module-size.sh` to check.
