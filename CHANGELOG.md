# Revision history for sabela

## 0.1.0.0 -- 2026-06-21

* First release. A reactive notebook environment for Haskell.
* Notebooks are plain Markdown files; their code blocks are executable cells.
* Cells are evaluated by a long-lived GHCi backend. Editing a cell re-runs the
  cells that depend on it.
* Ships an embedded web IDE with slideshow and dashboard export.
