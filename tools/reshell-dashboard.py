#!/usr/bin/env python3
"""Re-shell a gallery dashboard export into the current template.

A dashboard/notebook export is the template with its `/*__SABELA_INJECT__*/`
placeholder replaced by `window.__SABELA_STATIC__ = <json>;` (plus, for
notebook-mode exports, the render-mode flag and the markdown literal). When the
template's chrome moves on (new theme picker, brand, styling) an old export keeps
its stale chrome. This lifts the injected data out of the old export and drops it
into the current template, so the outputs are preserved exactly with no re-run.

    tools/reshell-dashboard.py <old-export.html> <template.html> <out.html>

<template.html> is the current bundled template, e.g. static/dashboard.html,
which still contains the /*__SABELA_INJECT__*/ placeholder.
"""
import json
import sys

PLACEHOLDER = "/*__SABELA_INJECT__*/"


def extract_object(s, var):
    """Return the `{...}` JS object literal assigned to window.<var>, verbatim."""
    anchor = "window." + var + " = "
    i = s.find(anchor)
    if i < 0:
        return None
    j = i + len(anchor)
    while s[j] in " \t\r\n":
        j += 1
    if s[j] != "{":
        raise SystemExit(f"{var} is not an object literal at {j}")
    depth, in_str, esc = 0, False, False
    for k in range(j, len(s)):
        c = s[k]
        if in_str:
            if esc:
                esc = False
            elif c == "\\":
                esc = True
            elif c == '"':
                in_str = False
        elif c == '"':
            in_str = True
        elif c == "{":
            depth += 1
        elif c == "}":
            depth -= 1
            if depth == 0:
                return s[j : k + 1]
    raise SystemExit(f"unterminated object for {var}")


def extract_string(s, var):
    """Return the `"..."` JS string literal assigned to window.<var>, verbatim."""
    anchor = "window." + var + " = "
    i = s.find(anchor)
    if i < 0:
        return None
    j = i + len(anchor)
    while s[j] in " \t\r\n":
        j += 1
    if s[j] != '"':
        raise SystemExit(f"{var} is not a string literal at {j}")
    esc = False
    for k in range(j + 1, len(s)):
        c = s[k]
        if esc:
            esc = False
        elif c == "\\":
            esc = True
        elif c == '"':
            return s[j : k + 1]
    raise SystemExit(f"unterminated string for {var}")


def main():
    if len(sys.argv) != 4:
        raise SystemExit(__doc__)
    old, tmpl_path, out = sys.argv[1], sys.argv[2], sys.argv[3]
    src = open(old, encoding="utf-8").read()
    tmpl = open(tmpl_path, encoding="utf-8").read()
    if PLACEHOLDER not in tmpl:
        raise SystemExit(f"{tmpl_path} has no {PLACEHOLDER} placeholder")

    static = extract_object(src, "__SABELA_STATIC__")
    if static is None:
        raise SystemExit("no __SABELA_STATIC__ in source export")
    # Validate boundaries: the literal escapes </ as <\/ for script safety;
    # undo that and it must parse as JSON.
    json.loads(static.replace("<\\/", "</"))

    inject = f"window.__SABELA_STATIC__ = {static};"
    mode = extract_string(src, "__SABELA_RENDER_MODE__")
    if mode is not None:
        inject += f"\nwindow.__SABELA_RENDER_MODE__ = {mode};"
    md = extract_string(src, "__SABELA_MARKDOWN__")
    if md is not None:
        inject += f"\nwindow.__SABELA_MARKDOWN__ = {md};"

    open(out, "w", encoding="utf-8").write(tmpl.replace(PLACEHOLDER, inject, 1))
    print(f"re-shelled {old} -> {out} (static={len(static)}B, mode={mode}, md={'yes' if md else 'no'})")


if __name__ == "__main__":
    main()
