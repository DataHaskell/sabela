# DataFrame and Granite

## `api_reference` is the embedded card — consult it first

Before guessing DataFrame / Granite APIs, call the `api_reference` tool. It
serves the pre-generated `data/api-reference.txt` card embedded in the server — a
`:browse` of `DataFrame`, `DataFrame.Typed`, `DataFrame.Functions`,
`DataFrame.Display.Web.Plot`, and `Granite.Svg`. It is cheaper and more current
than recalling from training.

```bash
${CLAUDE_PLUGIN_ROOT}/skills/siza/scripts/siza tool api_reference '{"module":"DataFrame.Typed"}'
```

The `module` argument is a substring match on the section header; an empty input
returns the whole card. Plotting should primarily be done with Granite — browse
`Granite.Svg` rather than hand-rolling SVG.

## Typed vs untyped: a concrete trigger

DataFrame ships two front doors and the choice is rule-based, not taste.

- **Use `DataFrame.Typed`** — schema-safe — when a frame threads through **two
  or more downstream cells**. The schema is checked at the seam, so a column
  rename or a type mismatch is caught where it happens rather than surfacing as a
  runtime error three cells later. A frame you commit across the notebook earns
  the typed wrapper.
- **Use untyped `DataFrame`** for a **one-shot** `head` / `dimensions` /
  inspection in scratch, where the frame does not outlive the cell. The typed
  ceremony buys nothing for a value you read once.

## Prefer the lightest package

`build-depends: dataframe` is an **umbrella** that drags in
plotting/typed/ML/web (and granite) — a heavy env whose `-O2` object-code
startup can hang GHCi long enough to need a browser restart. For plain frames use
**`dataframe-core`** (base/containers/primitive/text/vector only). Nothing in the
toolchain warns you the umbrella is heavy or that a `-core` exists — reach for the
narrow package first. (This is also flagged in core; it lives here too because it
is part of the DataFrame decision.)
