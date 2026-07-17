#!/usr/bin/env bash
set -euo pipefail

# Project rule (CLAUDE.md Conventions, funlen-style): keep functions short.
# Flags a top-level function/value binding whose body spans more than the limit
# in non-blank, non-comment lines. Haskell is terse, so the cap is low (20).
#
# Diff-only: a function is checked only when the change touches one of its lines,
# so existing long functions are grandfathered and the rule tightens on new/
# changed code (like check-comments.sh). Set FUNC_LINE_LIMIT to change the cap,
# BASE to compare against another ref, or ALL=1 to scan every function (audit).
#
# The measure is the equation block: from the defining equation through its
# where-clause. The signature and haddock are separate blocks and do not count.
#
# Test modules are exempt (a long hspec `spec` tree is not a smell) — mirroring
# the golangci `path: _test\.go` funlen exclusion. A body that is predominantly
# literal data (a prompt string, a tool catalogue) is exempt too: splitting a
# list of strings into helpers hurts rather than helps.
#
# Usage: ./scripts/check-func-length.sh

# A test module (exempt): anything under a test/ directory.
is_test() { case "$1" in */test/*|test/*) return 0 ;; *) return 1 ;; esac; }

LIMIT="${FUNC_LINE_LIMIT:-20}"
BASE="${BASE:-origin/main}"
ALL="${ALL:-0}"
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

base="$(git merge-base "$BASE" HEAD 2>/dev/null || git rev-parse HEAD)"

tmp="$(mktemp)"
trap 'rm -f "$tmp"' EXIT

# Fail loudly rather than reporting a vacuous pass: a scan that cannot parse is
# not a clean gate. Any awk failure aborts the run (set -e honours this).
scan_or_die() { scan "$@" || { echo "✗ scan failed on $1" >&2; exit 2; }; }

# Emit "file:line: function NAME is N lines" for every over-limit function block
# whose span intersects the added-line set (or all blocks when ALL=1).
scan() {
  local f="$1" added="$2"
  awk -v file="$f" -v limit="$LIMIT" -v added="$added" -v all="$ALL" '
    BEGIN { na = split(added, aa, " "); for (i = 1; i <= na; i++) addl[aa[i]] = 1 }
    # A body that is >= 60% string-literal / list-punctuation lines is data, not
    # logic: report only genuine code.
    function isData() { return (bLines > 0 && dataLines / bLines >= 0.6) }
    function finalize() {
      if (bStart == 0) return
      kind = bKind
      if (kind == "code") kind = (hasBody ? "func" : "sig")
      if (kind == "func" && bLines > limit && !isData() && (all == "1" || bTouched))
        printf "%s:%d: function %s is %d lines (limit %d)\n",
               file, bStart, bName, bLines, limit
      bStart = 0; bKind = ""; bLines = 0; bName = ""; bTouched = 0; hasBody = 0
      dataLines = 0
    }
    # Data-ish: a string literal, bare or applied to one helper (a tool catalogue
    # line like @, boolProp "..."@), or a line of pure list/tuple punctuation.
    function countLine(s) {
      bLines++
      if (s ~ /^[[:space:]]*[,\[\]\(\)]*[[:space:]]*"/ ||
          s ~ /^[[:space:]]*[,\[\]\(\)]*[[:space:]]*[a-zA-Z_][a-zA-Z0-9_.]*[[:space:]]+"/ ||
          s ~ /^[[:space:]]*[,\[\]\(\)]+[[:space:]]*$/)
        dataLines++
    }
    {
      blank = ($0 ~ /^[[:space:]]*$/)
      indented = ($0 ~ /^[[:space:]]/)
      commentLine = ($0 ~ /^[[:space:]]*(--|\{-)/)

      if (!blank && !indented) {
        finalize()
        bStart = NR
        bTouched = (NR in addl) ? 1 : 0
        name = $0; sub(/[[:space:]].*/, "", name); bName = name
        if ($0 ~ /^(--|\{-)/) bKind = "comment"
        else if ($0 ~ /^(module|import|data|type|newtype|class|instance|deriving|infixl|infixr|infix|foreign|default|pattern)([[:space:]]|$)/) bKind = "decl"
        else bKind = "code"
        hasBody = ($0 ~ /=/ || $0 ~ /^[^:]*\|/)
        bLines = 0; dataLines = 0
        if (!blank && !commentLine) countLine($0)
        next
      }

      if (bStart == 0) next
      if (NR in addl) bTouched = 1
      if (!blank && !commentLine) countLine($0)
      if ($0 ~ /=/ || $0 ~ /^[[:space:]]*\|/) hasBody = 1
    }
    END { finalize() }
  ' "$f"
}

if [ "$ALL" = "1" ]; then
  while IFS= read -r f; do
    [ -f "$f" ] || continue
    is_test "$f" && continue
    scan_or_die "$f" "" >> "$tmp"
    # --cached --others: tracked plus new untracked modules (--exclude-standard
    # honours .gitignore); a brand-new file is exactly where the cap matters.
  done < <(git ls-files --cached --others --exclude-standard '*.hs' | sort -u)
else
  while IFS= read -r f; do
    [ -f "$f" ] || continue
    is_test "$f" && continue
    added="$(git diff --unified=0 "$base" -- "$f" | awk '
      /^@@/ {
        match($0, /\+[0-9]+(,[0-9]+)?/)
        spec = substr($0, RSTART + 1, RLENGTH - 1)
        n = split(spec, p, ",")
        start = p[1] + 0; count = (n > 1 ? p[2] + 0 : 1)
        for (i = 0; i < count; i++) print start + i
      }' | tr '\n' ' ')"
    [ -z "$added" ] && continue
    scan_or_die "$f" "$added" >> "$tmp"
  done < <(git diff --name-only --diff-filter=d "$base" -- '*.hs')
fi

if [ -s "$tmp" ]; then
  echo "Functions over the ${LIMIT}-line cap (CLAUDE.md Conventions, funlen):" >&2
  sort -u "$tmp" >&2
  count=$(sort -u "$tmp" | wc -l | tr -d ' ')
  echo "" >&2
  echo "✗ ${count} function(s) exceed ${LIMIT} lines — split them into helpers." >&2
  exit 1
fi

echo "✓ No over-length functions on changed lines (base ${base:0:12})."
