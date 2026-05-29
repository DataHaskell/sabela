#!/usr/bin/env bash
set -euo pipefail

# Project rule (CLAUDE.md Conventions): keep every module <= 300 lines.
# Fails if any tracked Haskell module or static JS file exceeds the cap, listing
# the offenders (largest first). Override the limit with MODULE_LINE_LIMIT.
#
# Usage: ./scripts/check-module-size.sh

LIMIT="${MODULE_LINE_LIMIT:-300}"
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

tmp="$(mktemp)"
trap 'rm -f "$tmp"' EXIT

while IFS= read -r f; do
  [ -f "$f" ] || continue
  n=$(wc -l < "$f" | tr -d ' ')
  if [ "$n" -gt "$LIMIT" ]; then
    printf '%6d  %s\n' "$n" "$f" >> "$tmp"
  fi
done < <(git ls-files '*.hs' 'static/*.js')

if [ -s "$tmp" ]; then
  echo "Modules over the ${LIMIT}-line cap (see CLAUDE.md Conventions):" >&2
  sort -rn "$tmp" >&2
  count=$(wc -l < "$tmp" | tr -d ' ')
  echo "" >&2
  echo "✗ ${count} file(s) exceed ${LIMIT} lines — split them into focused submodules." >&2
  exit 1
fi

echo "✓ All tracked modules are within the ${LIMIT}-line cap."
