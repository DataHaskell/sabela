#!/bin/bash
# Test all example notebooks via curl against a running Sabela container
# Usage: ./test_examples.sh [port] [timeout_per_notebook]

PORT=${1:-3003}
TIMEOUT=${2:-300}
BASE="http://localhost:$PORT"
PASS=0
FAIL=0
ERRORS=""

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

  # Load notebook
  LOAD=$(curl -s --max-time 15 "$BASE/api/load" \
    -H 'Content-Type: application/json' \
    -d "{\"lrPath\":\"$nb\"}" 2>&1)

  if echo "$LOAD" | python3 -c "import sys,json; json.load(sys.stdin)" 2>/dev/null; then
    NCELLS=$(echo "$LOAD" | python3 -c "
import sys,json
nb=json.load(sys.stdin)
code=[c for c in nb['nbCells'] if c['cellType']=='CodeCell']
print(len(code))
")
    echo "  Loaded: $NCELLS code cells"
  else
    echo "  FAIL: Could not load notebook"
    FAIL=$((FAIL+1))
    ERRORS="$ERRORS\n  $nb: load failed"
    continue
  fi

  # Run all cells
  curl -s -X POST --max-time 5 "$BASE/api/run-all" > /dev/null 2>&1

  # Collect SSE events until executionDone or timeout
  EVENTS=$(curl -s --max-time "$TIMEOUT" "$BASE/api/events" 2>&1 &
    CURL_PID=$!
    # Wait for executionDone or timeout
    while kill -0 $CURL_PID 2>/dev/null; do
      sleep 2
    done
  )

  # Actually, simpler approach: poll notebook state until all cells have results or timeout
  ELAPSED=0
  INTERVAL=5
  ALL_DONE=false
  while [ $ELAPSED -lt $TIMEOUT ]; do
    sleep $INTERVAL
    ELAPSED=$((ELAPSED + INTERVAL))

    STATUS=$(curl -s --max-time 10 "$BASE/api/notebook" 2>&1 | python3 -c "
import sys,json
try:
    nb=json.load(sys.stdin)
    code=[c for c in nb['nbCells'] if c['cellType']=='CodeCell']
    done=[c for c in code if c['cellOutputs'] or c['cellError'] is not None]
    crashed=any(c.get('cellError','') and 'crashed' in str(c.get('cellError','')) for c in code)
    print(f'{len(done)}/{len(code)}')
    if len(done)==len(code) or crashed:
        print('COMPLETE')
except:
    print('ERROR')
" 2>&1)

    PROGRESS=$(echo "$STATUS" | head -1)
    echo "  [$ELAPSED s] $PROGRESS cells done"

    if echo "$STATUS" | grep -q "COMPLETE"; then
      ALL_DONE=true
      break
    fi
  done

  # Get final results
  RESULT=$(curl -s --max-time 10 "$BASE/api/notebook" 2>&1 | python3 -c "
import sys,json
nb=json.load(sys.stdin)
code=[c for c in nb['nbCells'] if c['cellType']=='CodeCell']
errors=[]
crashed=False
for c in code:
    e = c.get('cellError')
    if e:
        errors.append(f'cell {c[\"cellId\"]}: {e[:100]}')
        if 'crashed' in str(e).lower() or 'repl failed' in str(e).lower():
            crashed=True
print(f'CELLS={len(code)}')
print(f'ERRORS={len(errors)}')
print(f'CRASHED={crashed}')
for e in errors:
    print(f'  ERR: {e}')
" 2>&1)

  echo "$RESULT" | sed 's/^/  /'

  HAS_CRASH=$(echo "$RESULT" | grep "CRASHED=True")
  ERR_COUNT=$(echo "$RESULT" | grep "ERRORS=" | sed 's/ERRORS=//')

  if [ -n "$HAS_CRASH" ]; then
    echo "  RESULT: CRASH"
    FAIL=$((FAIL+1))
    ERRORS="$ERRORS\n  $nb: kernel crashed"
  elif [ "$ERR_COUNT" != "0" ] && [ -n "$ERR_COUNT" ]; then
    echo "  RESULT: ERRORS ($ERR_COUNT)"
    FAIL=$((FAIL+1))
    ERRORS="$ERRORS\n  $nb: $ERR_COUNT cell errors"
  elif [ "$ALL_DONE" = true ]; then
    echo "  RESULT: PASS"
    PASS=$((PASS+1))
  else
    echo "  RESULT: TIMEOUT"
    FAIL=$((FAIL+1))
    ERRORS="$ERRORS\n  $nb: timed out after ${TIMEOUT}s"
  fi
  echo ""
done

echo "=============================="
echo "SUMMARY: $PASS passed, $FAIL failed out of ${#examples[@]}"
if [ -n "$ERRORS" ]; then
  echo -e "Failures:$ERRORS"
fi
