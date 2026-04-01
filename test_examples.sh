#!/bin/bash
# Test all example notebooks via curl against a running Sabela container.
# Usage: ./test_examples.sh [port] [timeout_per_notebook]

PORT=${1:-3003}
TIMEOUT=${2:-300}
BASE="http://localhost:$PORT"
PASS=0
FAIL=0
FAILURES=""

examples=(
  "examples/sparkline.md"
  "examples/analysis.md"
  "examples/plotting.md"
  "examples/matplotlib-demo.md"
  "examples/lean-proofs.md"
  "examples/tutorial-lean-integration.md"
  "examples/tutorial-python-integration.md"
  "examples/typed-ml-pipeline.md"
  "examples/verified-crypto.md"
  "examples/verified-eda.md"
  "examples/widgets.md"
)

for nb in "${examples[@]}"; do
  echo "=== Testing: $nb ==="

  # Reset session between notebooks
  curl -s --max-time 10 -X POST "$BASE/api/reset" > /dev/null 2>&1

  # Load notebook
  LOAD=$(curl -s --max-time 15 "$BASE/api/load" \
    -H 'Content-Type: application/json' \
    -d "{\"lrPath\":\"$nb\"}" 2>&1)

  NCELLS=$(echo "$LOAD" | python3 -c "
import sys,json
try:
    nb=json.load(sys.stdin)
    code=[c for c in nb['nbCells'] if c['cellType']=='CodeCell']
    print(len(code))
except:
    print('ERR')
" 2>/dev/null)

  if [ "$NCELLS" = "ERR" ] || [ -z "$NCELLS" ]; then
    echo "  FAIL: Could not load notebook"
    FAIL=$((FAIL+1))
    FAILURES="$FAILURES\n  $nb: load failed"
    continue
  fi
  echo "  Loaded: $NCELLS code cells"

  # Wait for executionDone via SSE stream (load already triggers run-all)
  EVENTS_FILE=$(mktemp)
  curl -sN --max-time "$TIMEOUT" "$BASE/api/events" > "$EVENTS_FILE" 2>&1 &
  SSE_PID=$!

  # Wait for executionDone event or timeout
  ELAPSED=0
  DONE=false
  while [ $ELAPSED -lt $TIMEOUT ]; do
    sleep 3
    ELAPSED=$((ELAPSED + 3))
    if grep -q '"executionDone"' "$EVENTS_FILE" 2>/dev/null; then
      DONE=true
      break
    fi
    if grep -q '"crashed"' "$EVENTS_FILE" 2>/dev/null; then
      DONE=true
      break
    fi
    # Progress: count cellResult events
    RESULTS=$(grep -c '"cellResult"' "$EVENTS_FILE" 2>/dev/null || echo 0)
    echo -ne "  [$ELAPSED s] $RESULTS/$NCELLS cells done\r"
  done
  echo ""

  kill $SSE_PID 2>/dev/null
  wait $SSE_PID 2>/dev/null

  # Analyze final notebook state (accounts for reruns overwriting earlier errors)
  CRASHED=$(grep -c '"crashed"' "$EVENTS_FILE" 2>/dev/null || echo 0)
  RESULT_COUNT=$(grep -c '"cellResult"' "$EVENTS_FILE" 2>/dev/null || echo 0)

  CELL_ERRORS=$(curl -s --max-time 10 "$BASE/api/notebook" 2>/dev/null | python3 -c "
import sys,json
nb=json.load(sys.stdin)
errors=[]
for c in nb['nbCells']:
    if c['cellType']=='CodeCell' and c.get('cellError'):
        errors.append(f'cell {c[\"cellId\"]}: {c[\"cellError\"][:120]}')
for e in errors:
    print(e)
" 2>/dev/null)

  ERR_COUNT=$(echo "$CELL_ERRORS" | grep -c . 2>/dev/null || echo 0)
  [ -z "$CELL_ERRORS" ] && ERR_COUNT=0

  echo "  Cells completed: $RESULT_COUNT/$NCELLS"

  if [ "$CRASHED" -gt 0 ]; then
    echo "  RESULT: CRASH"
    FAIL=$((FAIL+1))
    FAILURES="$FAILURES\n  $nb: kernel crashed"
  elif [ "$ERR_COUNT" -gt 0 ]; then
    echo "  Cell errors:"
    echo "$CELL_ERRORS" | sed 's/^/    /'
    echo "  RESULT: ERRORS ($ERR_COUNT)"
    FAIL=$((FAIL+1))
    FAILURES="$FAILURES\n  $nb: $ERR_COUNT cell error(s)"
  elif [ "$DONE" = true ]; then
    echo "  RESULT: PASS"
    PASS=$((PASS+1))
  else
    echo "  RESULT: TIMEOUT (${TIMEOUT}s)"
    FAIL=$((FAIL+1))
    FAILURES="$FAILURES\n  $nb: timed out after ${TIMEOUT}s"
  fi

  rm -f "$EVENTS_FILE"
  echo ""
done

echo "=============================="
echo "SUMMARY: $PASS passed, $FAIL failed out of ${#examples[@]}"
if [ -n "$FAILURES" ]; then
  echo -e "Failures:$FAILURES"
fi
