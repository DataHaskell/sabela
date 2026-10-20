---
name: siza
description: Drive a running Sabela Haskell notebook from Claude Code. Use whenever the user asks to list, read, run, edit, debug, or analyse cells; explore a dataset; propose changes; or pair-program on a notebook they have open in the browser (typically localhost:3000).
---

# siza — Sabela notebook pair programming

The user has a Sabela notebook open in their browser. Every change you make appears live in their UI, and the GHCi session is shared with them.

All notebook operations are the MCP tools of the bundled `siza` server (`mcp__plugin_siza_siza__*`: `list_cells`, `read_cell`, `insert_cell`, `replace_cell_source`, `propose_edit`, `execute_cell`, `try`, `discover`, `verify`, `await_idle`, …). Each tool's description says what it is for and what it returns; read those, and call the tools with structured arguments.

## Hard rules

- **Only the siza tools touch the notebook.** Do not `curl` `/api/*` endpoints and do not edit the notebook's `.md` on disk; both bypass validation, the browser broadcast, and hash checks.
- **If the tools are missing,** the server is not running or `siza` is not on `PATH`: ask the user to run `cabal run` in the sabela checkout, `cabal install exe:siza`, and `/mcp` to reconnect. For a hub-hosted notebook they must run `siza login` themselves once.

## Which mutation tool?

| You want to… | Use |
|---|---|
| Edit a cell **the user wrote** | `propose_edit` with `expected_hash`; they accept it in the UI. |
| Edit a cell **you inserted** | `replace_cell_source` with `expected_hash`; applied and re-run at once. |
| Add a cell | `insert_cell`; always pass `cell_type` (and `language`). `after_cell_id: -1` is the top. |
| Remove a cell | `delete_cell`; no undo. |

Never delete-and-insert to edit. A hash mismatch means the user edited the cell: `read_cell` again and retry with the fresh hash.

## Kernel discipline

- One GHCi kernel, one run-lock, shared with the browser. A call that returns `busy` did not queue: wait with `await_idle`, then continue. Never retry a mutation or `execute_cell` that timed out; the cell is still running.
- A `-- cabal:` edit restarts the kernel (one to three minutes) and a `-- compile` cell relaunches it as object code. Batch dependency changes into one edit.
- Reactivity is textual: a run re-runs downstream cells, but a cell fixed after failing may leave its consumers red. Re-run them once.
- A kernel state change you did not cause means the user acted. Re-sync with `export_notebook`.
- Explore with `try`, `check_type`, `list_bindings`, `discover`; commit with `insert_cell`; prove a claim with `verify`.
