#!/usr/bin/env bash
# Drive one live episode end to end and capture the verbose transcript.
#
# Starts a throwaway sabela server on a free port with a fresh notebook, pipes
# the given prompts into `siza chat --verbose`, writes the audit to
# docs/discover/live/live_test<N>_verbose.md (next free N), and tears the
# server down. The transcript is the evidence a round is graded on.
#
# Usage: scripts/live-round.sh "first prompt" ["second prompt" ...]
#        LIVE_LABEL=g5-task4 scripts/live-round.sh "..."   # names the file
set -uo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

PORT="${LIVE_PORT:-3111}"
MODEL="${LIVE_MODEL:-gpt-oss:20b}"
MAX_TURNS="${LIVE_MAX_TURNS:-40}"
LIVE_DIR="docs/discover/live"

# Next free transcript number, so a round never clobbers its predecessors.
n=0
for f in "$LIVE_DIR"/live_test*.md; do
    b="$(basename "$f")"
    m="${b#live_test}"; m="${m%%[!0-9]*}"
    [ -n "$m" ] && [ "$m" -gt "$n" ] && n="$m"
done
n=$((n + 1))
LABEL="${LIVE_LABEL:-}"
OUT="$LIVE_DIR/live_test${n}${LABEL:+_$LABEL}_verbose.md"

WORKDIR="$(mktemp -d "${TMPDIR:-/tmp}/sabela-live-XXXXXX")"
SERVER_LOG="$WORKDIR/server.log"

# The drawing/animation vocabulary lives in the local checkout; without this
# the session resolves an older sabela-notebook and `plot`/`displayPicture`
# silently do not exist.
export SABELA_LOCAL_PACKAGES="$ROOT/sabela-notebook"

cleanup() {
    if [ -n "${SERVER_PID:-}" ] && kill -0 "$SERVER_PID" 2>/dev/null; then
        kill "$SERVER_PID" 2>/dev/null
        sleep 1
        kill -9 "$SERVER_PID" 2>/dev/null
    fi
}
trap cleanup EXIT INT TERM

echo "[live] work dir: $WORKDIR"
echo "[live] transcript: $OUT"

cabal run -v0 exe:sabela -- "$PORT" "$WORKDIR" > "$SERVER_LOG" 2>&1 &
SERVER_PID=$!

echo -n "[live] waiting for server"
ready=0
for _ in $(seq 1 120); do
    if curl -sf --max-time 2 "http://localhost:$PORT/api/ai/health" > /dev/null 2>&1; then
        ready=1
        break
    fi
    if ! kill -0 "$SERVER_PID" 2>/dev/null; then
        echo " — server exited early"
        tail -20 "$SERVER_LOG"
        exit 1
    fi
    echo -n "."
    sleep 2
done
echo
[ "$ready" = 1 ] || { echo "[live] server never became healthy"; tail -20 "$SERVER_LOG"; exit 1; }

printf '%s\n' "$@" |
    cabal run -v0 exe:siza -- chat \
        --verbose \
        --model "$MODEL" \
        --max-turns "$MAX_TURNS" \
        --url "http://localhost:$PORT" \
        > "$OUT" 2>&1
status=$?

# The notebook lives in the server's memory, not on disk, so read it back over
# the API before teardown: the cell sources are what a round is graded on.
{
    echo
    echo '## Resulting notebook'
    echo
    echo '```haskell'
    curl -s --max-time 20 "http://localhost:$PORT/api/notebook" |
        python3 -c '
import sys, json
try:
    nb = json.load(sys.stdin)
except Exception:
    print("[could not read notebook]"); raise SystemExit
cells = nb.get("nbCells", [])
if not cells:
    print("[no cells written]")
for c in cells:
    print("-- cell %s (%s)%s" % (c.get("cellId"), c.get("cellLang"),
          " ERROR" if c.get("cellError") else ""))
    print(c.get("cellSource", ""))
    print()
' 2>/dev/null || echo '[notebook read failed]'
    echo '```'
} >> "$OUT"

echo "[live] chat exit: $status"
echo "[live] wrote $OUT ($(wc -l < "$OUT") lines)"
