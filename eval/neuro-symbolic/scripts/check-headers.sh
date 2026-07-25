#!/usr/bin/env bash
# R8.1 scripted header check over a transcript directory: every .md must carry
# a complete episode-config header (task/arm/levers/seed/seeds-tried/model/
# stopped/final/lint/run-id/commit/build-time/run-time/endpoint/relink-probe),
# run-time must not predate build-time, and every byte-identical pair must
# carry a flag file.
set -u
dir="${1:?usage: check-headers.sh <transcript-dir>}"
fail=0
# 'final' must be PRESENT but may be empty: an empty final under a cap stop
# is the transcript lint's verdict (empty-final), not a missing header field.
fields="task arm levers seed seeds-tried model stopped lint run-id commit build-time run-time endpoint relink-probe"
present_fields="final"
for f in "$dir"/*.md; do
  [ -e "$f" ] || { echo "no transcripts in $dir"; exit 1; }
  head -n1 "$f" | grep -q '^<!-- episode-config$' || { echo "FAIL $f: no header"; fail=1; continue; }
  hdr=$(awk '/^-->$/{exit} NR>1{print}' "$f")
  for k in $fields; do
    echo "$hdr" | grep -q "^$k: ." || { echo "FAIL $f: missing/empty field '$k'"; fail=1; }
  done
  for k in $present_fields; do
    echo "$hdr" | grep -q "^$k:" || { echo "FAIL $f: missing field '$k'"; fail=1; }
  done
  bt=$(echo "$hdr" | sed -n 's/^build-time: //p')
  rt=$(echo "$hdr" | sed -n 's/^run-time: //p')
  if [ -n "$bt" ] && [ -n "$rt" ] && [ "$rt" \< "$bt" ]; then
    echo "FAIL $f: run-time $rt predates build-time $bt"; fail=1
  fi
done
# Identical off/on bodies must be flagged (VOID / SATURATED / NA).
for off in "$dir"/*-off.md; do
  [ -e "$off" ] || continue
  on="${off%-off.md}-on.md"; stem="${off%-off.md}"
  [ -e "$on" ] || continue
  boff=$(awk 'f{print} /^-->$/{f=1}' "$off"); bon=$(awk 'f{print} /^-->$/{f=1}' "$on")
  if [ "$boff" = "$bon" ]; then
    if [ ! -e "$stem.VOID" ] && [ ! -e "$stem.SATURATED" ] && [ ! -e "$stem.NA" ]; then
      echo "FAIL $stem: byte-identical pair carries NO flag"; fail=1
    fi
  fi
done
[ "$fail" -eq 0 ] && echo "OK: all headers complete, all identical pairs flagged"
exit "$fail"
