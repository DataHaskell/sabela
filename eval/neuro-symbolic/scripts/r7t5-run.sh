#!/usr/bin/env bash
# R7-T5 driver wrapper: run siza-bench detached from the repo root with the
# tool timeout pinned, logging stdout+stderr; the caller polls the log.
set -uo pipefail
cd "$(dirname "$0")/../../.."
LOG="${1:?usage: r7t5-run.sh <log-file> [task-csv]}"
TASKS="${2:-}"
export SIZA_BENCH_SEEDS=1
export SABELA_TOOL_TIMEOUT=300
[ -n "$TASKS" ] && export SIZA_BENCH_TASKS="$TASKS"
BENCH=$(cabal list-bin exe:siza-bench)
{
    echo "[r7t5] start $(date -u +%Y-%m-%dT%H:%M:%SZ) tasks=${TASKS:-ALL}"
    "$BENCH"
    echo "[r7t5] exit=$? end $(date -u +%Y-%m-%dT%H:%M:%SZ)"
} >"$LOG" 2>&1
