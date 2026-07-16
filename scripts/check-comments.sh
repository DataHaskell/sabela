#!/usr/bin/env bash
set -euo pipefail

# Project rule (CLAUDE.md Conventions): comments are top-level (a haddock or
# block comment on its own line above the declaration) and at most 3 lines,
# unless they contain a code example.
#
# Diff-only: flags violations on lines ADDED vs the base branch, so existing
# comments are grandfathered and the rule enforces on new/changed code. Set BASE
# to compare against a different ref; COMMENT_LINE_LIMIT to change the cap.
#
# Usage: ./scripts/check-comments.sh

LIMIT="${COMMENT_LINE_LIMIT:-3}"
BASE="${BASE:-origin/main}"
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

base="$(git merge-base "$BASE" HEAD 2>/dev/null || git rev-parse HEAD)"

tmp="$(mktemp)"
trap 'rm -f "$tmp"' EXIT

while IFS= read -r f; do
  [ -f "$f" ] || continue

  # New-side line numbers added in this file, from a zero-context diff.
  added="$(git diff --unified=0 "$base" -- "$f" | awk '
    /^@@/ {
      match($0, /\+[0-9]+(,[0-9]+)?/)
      spec = substr($0, RSTART + 1, RLENGTH - 1)
      n = split(spec, p, ",")
      start = p[1] + 0; count = (n > 1 ? p[2] + 0 : 1)
      for (i = 0; i < count; i++) print start + i
    }' | tr '\n' ' ')"
  [ -z "$added" ] && continue

  awk -v file="$f" -v limit="$LIMIT" -v added="$added" '
    BEGIN { na = split(added, aa, " "); for (i = 1; i <= na; i++) addl[aa[i]] = 1 }
    function isNew(ln) { return (ln in addl) }
    function isCode(s) {
      return (s ~ />>>/ || s ~ /^[[:space:]]*(--[[:space:]]*)?> / || s ~ /```/)
    }
    function report(ln, msg) { if (isNew(ln)) printf "%s:%d: %s\n", file, ln, msg }
    function flushRun() {
      if (run > limit && !runCode)
        report(runStart, "comment block spans " run " lines (limit " limit ")")
      run = 0; runCode = 0
    }
    {
      lead = $0; sub(/^[[:space:]]+/, "", lead)

      if (inBlock) {
        if (isCode($0)) blkCode = 1
        if ($0 ~ /-\}/) {
          # A delimiter-only "-}" line is not content; count it only if the
          # closing line also carries text.
          if ($0 !~ /^[[:space:]]*-\}[[:space:]]*$/) blk++
          if (blk > limit && !blkCode)
            report(blkStart, "block comment spans " blk " lines (limit " limit ")")
          inBlock = 0
        } else blk++
        next
      }

      if (lead ~ /^\{-[^#]/ || lead == "{-") {
        flushRun()
        blkStart = NR; blkCode = isCode($0)
        # A bare "{-" or "{- |" opener carries no content; start the count at 0.
        blk = (lead ~ /^\{-[[:space:]]*\|?[[:space:]]*$/) ? 0 : 1
        if ($0 !~ /-\}/) inBlock = 1
        next
      }

      if (lead ~ /^--/) {
        if (run == 0) runStart = NR
        run++
        if (isCode($0)) runCode = 1
        next
      }

      flushRun()

      if ($0 ~ /[^[:space:]].*[[:space:]]--[[:space:]]/ &&
          $0 !~ /--[[:space:]]*[\^|]/ && $0 !~ /"/ && $0 !~ /\{-#/)
        report(NR, "inline (non-top-level) comment")
    }
    END { flushRun() }
  ' "$f" >> "$tmp"
done < <(git diff --name-only --diff-filter=d "$base" -- '*.hs')

if [ -s "$tmp" ]; then
  echo "Comment-style violations on changed lines (CLAUDE.md: top-level, <= ${LIMIT} lines):" >&2
  sort -u "$tmp" >&2
  count=$(sort -u "$tmp" | wc -l | tr -d ' ')
  echo "" >&2
  echo "✗ ${count} new comment(s) violate the style (fix, or add a code example)." >&2
  exit 1
fi

echo "✓ No comment-style violations on changed lines (base ${base:0:12})."
