# Driving widgets

siza exposes no widget-set tool. Setting a widget's value is the one sanctioned
exception to the Hard rule's "no raw endpoints", and it needs user sign-off
before you `curl` it.

## Why there is no tool

Widgets — `scatterSelectWith` lassoes, `slider`, `dropdown`, `checkbox`,
`textInput` — read their state from a **server-side `WidgetStore`**, which
`Sabela.Bridge.widgetPreamble` writes into the in-session `_sabelaWidgetRef`
**before every cell run**. So you **cannot** set a widget by
`modifyIORef _sabelaWidgetRef` from a cell — it's clobbered on the next run. The
only writer is the browser bridge.

## The sanctioned recipe

When the user asks you to set a widget's value from chat — a lasso's selection,
a slider position, a dropdown choice — use the browser's own endpoint, the lone
sanctioned raw POST (get user OK first):

```bash
base="$(${CLAUDE_PLUGIN_ROOT}/skills/siza/scripts/siza discover | jq -r '.[0].baseUrl')"
curl -s -o /dev/null -w 'HTTP %{http_code}\n' -X POST "$base/api/widget" \
  -H 'Content-Type: application/json' \
  --data "$(jq -nc --rawfile v /tmp/sel.txt \
    '{wuCellId:<widget-cell-id>, wuName:"<widget-name>", wuValue:($v|rtrimstr("\n"))}')"
```

This is safe in the ways the Hard rule worries about: `setWidgetH` writes the
store, then `handleWidgetCell` → `executeAffected wuCellId` re-renders the widget
cell **and** reruns downstream cells reactively + broadcasts over SSE — identical
to a real browser interaction. There's no cell-source hash to guard.

- **`wuCellId`** = the cell that *renders* the widget (the
  `scatterSelectWith`/`slider`/… cell). Must be that cell, or its
  highlight/control won't refresh.
- **`wuName`** = the widget's name argument — the first string you passed it (the
  `name` in `scatterSelectWith name …`, `slider name …`, etc.).
- **`wuValue`** = the value `show`-serialized, read back via `reads`: a lasso is
  a `[Int]` literal of **0-based positions** into the points list passed to the
  widget (= DataFrame row order if `pts` was built from columns in order); a
  slider an int; dropdown/text a string.
- **Big selections:** compute the indices in a throwaway cell and
  `writeFile "/tmp/sel.txt" (show idx)`, then feed the file to `jq --rawfile`
  rather than pasting thousands of ints through tool output. Delete the temp cell
  afterward.
