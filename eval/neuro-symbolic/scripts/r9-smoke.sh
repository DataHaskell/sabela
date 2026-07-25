#!/usr/bin/env bash
# R9 sequenced smoke (runs LAST, after the round's tasks are audit-green on
# a green gate — the round-4/7 sequencing lesson). Protocol:
#   barChart, topMonth, revenueTotal x seed 1 x both arms, strictly
#   sequential, fresh server per episode, SABELA_TOOL_TIMEOUT=300.
# Integrity rails (R8 close #5): the channel-attribution SELFTEST must pass
# first; server AND driver exes are force-purged from dist-newstyle before
# the rebuild (cabal can skip the final link, leaving a stale exe); the
# driver's dual relink probe then proves both binaries postdate the tree;
# transcripts land in a FRESH per-run-id directory; headers, pins, and
# metrics are graded from that directory only.
set -euo pipefail
cd "$(dirname "$0")"
ROOT="$(cd ../../.. && pwd)"

echo "== selftests (required before the smoke) =="
python3 r10-channel-attribution-test.py
bash check-pins-test.sh
python3 r10-factsheld-property-test.py

echo "== force-relink purge (dist-newstyle/**/x/{sabela,siza-bench}) =="
find "$ROOT/dist-newstyle" -type d -path "*/x/sabela" -prune -exec rm -rf {} + 2>/dev/null || true
find "$ROOT/dist-newstyle" -type d -path "*/x/siza-bench" -prune -exec rm -rf {} + 2>/dev/null || true

echo "== rebuild =="
(cd "$ROOT" && cabal build exe:sabela exe:siza-bench)
SABELA_BIN="$(cd "$ROOT" && cabal list-bin exe:sabela)"
BENCH_BIN="$(cd "$ROOT" && cabal list-bin exe:siza-bench)"

echo "== smoke: barChart,topMonth,revenueTotal seed 1, both arms =="
# SIZA_BENCH_TRANSCRIPTS stays unset: the driver defaults to a fresh
# /tmp/siza-bench-transcripts/<run-id> directory (R8.3).
(cd "$ROOT" && \
  SABELA_BIN="$SABELA_BIN" \
  SABELA_TOOL_TIMEOUT=300 \
  SIZA_BENCH_TASKS=barChart,topMonth,revenueTotal \
  SIZA_BENCH_SEEDS=1 \
  "$BENCH_BIN")

DIR="$(ls -dt /tmp/siza-bench-transcripts/run-* | head -1)"
echo "== grading $DIR =="
bash check-headers.sh "$DIR"
python3 r10-metrics.py "$DIR"
python3 check-pins.py "$DIR" pins.tsv || true
echo "== transcripts in $DIR =="
ls -la "$DIR"
