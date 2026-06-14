#!/usr/bin/env bash
set -euo pipefail
# Generate a gallery dashboard export from a notebook by running it in an
# ephemeral sabela server. This is the load -> run-all -> export cycle, scripted.
#
#   tools/gen-dashboard.sh <notebook.md> <output.html> [notebook|dashboard]
#
# <notebook.md>   path to the example, relative to the repo's examples/ dir
#                 (e.g. frp-tutorial.md) or an absolute path inside examples/.
# <output.html>   where to write the export (e.g. sabela-hub/scripts/dashboards/frp.html)
# mode            "notebook" (code + outputs, default) or "dashboard" (prose + outputs)
#
# The notebook is run twice: the second pass clears first-run stderr noise (the
# matplotlib font-cache notice, cold-session warmup) so the export is clean.
# Any cell still erroring after that is reported, and the script exits non-zero
# so a stale or broken notebook is never silently published.
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
NB="${1:?usage: gen-dashboard.sh <notebook.md> <output.html> [notebook|dashboard]}"
OUT="${2:?missing output path}"
MODE="${3:-notebook}"
NB="$(basename "$NB")"   # the work dir is examples/, so load by basename
PORT="${PORT:-3071}"

BIN="$(cd "$ROOT" && cabal list-bin exe:sabela 2>/dev/null)"
[ -x "$BIN" ] || { echo "sabela binary not found; run 'cabal build exe:sabela'" >&2; exit 1; }
[ -f "$ROOT/examples/$NB" ] || { echo "examples/$NB not found" >&2; exit 1; }

cleanup() { [ -n "${SRV:-}" ] && kill "$SRV" 2>/dev/null || true; }
trap cleanup EXIT

echo "=== starting ephemeral sabela on :$PORT (work dir: examples) ==="
( cd "$ROOT" && "$BIN" "$PORT" examples ) >/tmp/gen-dashboard.log 2>&1 &
SRV=$!
for _ in $(seq 1 30); do
  [ "$(curl -s -o /dev/null -w '%{http_code}' "http://localhost:$PORT/" 2>/dev/null)" = 200 ] && break
  sleep 1
done

api() { curl -s "http://localhost:$PORT/api/$1" "${@:2}"; }

echo "=== loading $NB ==="
api load -X POST -H 'Content-Type: application/json' -d "{\"lrPath\":\"$NB\"}" -o /dev/null

# Poll until every code cell is non-dirty and the output count holds steady.
settle() {
  local last=-1 stable=0
  for _ in $(seq 1 60); do
    local stat
    stat=$(api notebook | python3 -c "
import sys,json
d=json.loads(sys.stdin.read(), strict=False)
c=[x for x in d['nbCells'] if x.get('cellType')=='CodeCell']
print(sum(1 for x in c if x.get('cellDirty')), sum(1 for x in c if x.get('cellOutputs')))
" 2>/dev/null || echo "9 0")
    local dirty=${stat%% *} outs=${stat##* }
    if [ "$dirty" = 0 ] && [ "$outs" = "$last" ]; then
      stable=$((stable + 1)); [ "$stable" -ge 2 ] && return 0
    else
      stable=0
    fi
    last=$outs; sleep 4
  done
}

echo "=== run-all (first pass: builds deps, venv, warms session) ==="
api run-all -X POST -o /dev/null; settle
echo "=== run-all (second pass: clean outputs) ==="
api run-all -X POST -o /dev/null; settle

echo "=== exporting ($MODE) -> $OUT ==="
mkdir -p "$(dirname "$OUT")"
api "export/$MODE" -o "$OUT" -w 'export http=%{http_code} size=%{size_download}\n'

# Report any cell still erroring (e.g. a teaser cell that precedes its import
# and needs a manual re-run); fail loudly so it is fixed before publishing.
ERRS=$(api notebook | python3 -c "
import sys,json
d=json.loads(sys.stdin.read(), strict=False)
c=[x for x in d['nbCells'] if x.get('cellType')=='CodeCell']
bad=[i for i,x in enumerate(c) if x.get('cellError')]
print(len(bad), *bad)
")
N=${ERRS%% *}
if [ "$N" != 0 ]; then
  echo "WARNING: $N cell(s) still erroring after two passes: ${ERRS#* }" >&2
  echo "Open the notebook, re-run those cells (or fix them), and re-export." >&2
  exit 3
fi
echo "Done. $OUT generated cleanly."
