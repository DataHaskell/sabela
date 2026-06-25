#!/usr/bin/env bash
# Baseline harness: one fresh Sabela server (empty notebook, own GHCi kernel)
# per task, so tasks never contaminate each other. For each task we run the
# local model as the siza agent, grade it, and append a JSON line to results.
# Usage: ./run.sh [task-id ...]   (default: all tasks)
set -euo pipefail
cd "$(dirname "$0")"

MODEL="${SIZA_EVAL_MODEL:-gpt-oss:20b}"
TASKS=("$@")
[ ${#TASKS[@]} -eq 0 ] && TASKS=(double applyTwice safeDiv color mapMaybe treeFunctor quarterlyBars revenueTotal revenueChart)

echo "building binaries..." >&2
( cd ../.. && cabal build exe:sabela exe:siza-eval >/dev/null 2>&1 )
SABELA_BIN=$(cd ../.. && cabal list-bin exe:sabela)
EVAL_BIN=$(cd ../.. && cabal list-bin exe:siza-eval)

RESULTS="results-$(date +%Y%m%d-%H%M%S).jsonl"
: > "$RESULTS"
PORT=39000

for task in "${TASKS[@]}"; do
  PORT=$((PORT + 1))
  WORK=$(mktemp -d)
  : > "$WORK/global.md"   # empty globals: keep the kernel minimal (base only)
  echo "[$task] starting server on :$PORT (workdir $WORK)" >&2
  "$SABELA_BIN" "$PORT" "$WORK" "$WORK/global.md" >"$WORK/server.log" 2>&1 &
  SPID=$!

  # wait for readiness (notebook endpoint answers)
  for _ in $(seq 1 60); do
    curl -sf --max-time 2 "http://localhost:$PORT/api/notebook" >/dev/null 2>&1 && break
    sleep 0.5
  done

  SABELA_URL="http://localhost:$PORT" SIZA_EVAL_MODEL="$MODEL" \
    "$EVAL_BIN" "$task" | tee -a "$RESULTS" >&2 || echo "[$task] eval failed" >&2

  kill "$SPID" 2>/dev/null || true
  wait "$SPID" 2>/dev/null || true
  rm -rf "$WORK"
done

echo >&2
echo "=== summary ($MODEL) ===" >&2
python3 - "$RESULTS" >&2 <<'PY'
import json, sys
rows = [json.loads(l) for l in open(sys.argv[1]) if l.strip()]
p = sum(1 for r in rows if r.get("passed"))
for r in rows:
    mark = "PASS" if r.get("passed") else "fail"
    print(f"  {mark}  {r['task']:<12} turns={r['turns']:<2} tools={r['tool_calls']:<2} {r['seconds']:.0f}s")
print(f"  ---\n  {p}/{len(rows)} passed")
PY
echo "results: $RESULTS" >&2
