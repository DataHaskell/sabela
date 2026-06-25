#!/usr/bin/env bash
# Run ONE eval task against a fresh Sabela server (empty kernel, base only) and
# capture the model's session transcript to <sessions>/<task>.md.
# Usage: run-one.sh <task-id> <sessions-dir> [port]
# Assumes exe:sabela and exe:siza-eval are already built (the workflow builds
# them once up front). Prints the task's JSON result line to stdout.
set -uo pipefail
cd "$(dirname "$0")"

TASK="${1:?task id required}"
SESSIONS="${2:?sessions dir required}"
PORT="${3:-39500}"
MODEL="${SIZA_EVAL_MODEL:-gpt-oss:20b}"

mkdir -p "$SESSIONS"
SABELA_BIN=$(cd ../.. && cabal list-bin exe:sabela)
EVAL_BIN=$(cd ../.. && cabal list-bin exe:siza-eval)

WORK=$(mktemp -d)
: > "$WORK/global.md"   # empty globals: keep the kernel minimal (base only)
# Seed the dataset the dataframe tasks load; the kernel :cd's to $WORK at init.
printf 'month,revenue\nJan,100.0\nFeb,200.0\nMar,300.0\n' > "$WORK/revenue.csv"
"$SABELA_BIN" "$PORT" "$WORK" "$WORK/global.md" >"$SESSIONS/$TASK.server.log" 2>&1 &
SPID=$!
trap 'kill "$SPID" 2>/dev/null || true; rm -rf "$WORK"' EXIT

# wait for readiness (notebook endpoint answers)
for _ in $(seq 1 60); do
  curl -sf --max-time 2 "http://localhost:$PORT/api/notebook" >/dev/null 2>&1 && break
  sleep 0.5
done

# Generous tool timeout: a dep install + kernel restart far exceeds the 60s default.
SABELA_URL="http://localhost:$PORT" \
SIZA_EVAL_MODEL="$MODEL" \
SIZA_EVAL_SESSION_DIR="$SESSIONS" \
SIZA_EVAL_DEADLINE_SECS="${SIZA_EVAL_DEADLINE_SECS:-360}" \
SABELA_TOOL_TIMEOUT="${SABELA_TOOL_TIMEOUT:-300}" \
  "$EVAL_BIN" "$TASK"
