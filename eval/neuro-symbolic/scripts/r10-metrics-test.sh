#!/usr/bin/env bash
# Self-test for r10-metrics.py against a miniature fixture pair with known
# counts: discover calls, duplicate/not_found closures, rephrasings, response
# budget breaches, empty finals. Run before trusting the script on a real dir.
set -euo pipefail
cd "$(dirname "$0")"

fix=$(mktemp -d)
trap 'rm -rf "$fix"' EXIT

hdr() {
    cat <<EOF
<!-- episode-config
task: $1
arm: $2
levers: grammar=$2
seed: 1
seeds-tried: 1
model: test-model
stopped: $3
final: $4
lint: ok
run-id: run-test
commit: deadbeef
build-time: 2026-07-20T00:00:00Z
run-time: 2026-07-20T01:00:00Z
endpoint: http://localhost:9
relink-probe: ok: test
-->
EOF
}

# Off arm: 4 discover calls; queries "colX", "colX" (rephrasing+duplicate
# closure), "nope" (genuine absence: never resolved anywhere, NOT a false
# denial), "colY" module-scoped (denied here, resolved plain on the sibling
# arm: a per-tuple FALSE denial a mode-blind count misses); one 2600-byte
# tool response (budget breach).
{
    hdr fixTask off done "all done"
    echo '# Session: fixTask'
    echo '## 1. system'
    echo
    echo '**tool calls:**'
    echo '- `discover` {"query":"colX"}'
    echo '## 2. tool (discover)'
    echo
    echo '```'
    echo '{"hits":[{"install":"installed","name":"colX"}],"query":"colX","shown":1,"state":"found"}'
    echo '```'
    echo '**tool calls:**'
    echo '- `discover` {"query":"colX"}'
    echo '## 3. tool (discover)'
    echo
    echo '```'
    echo '{"query":"colX","state":"duplicate"}'
    echo '```'
    echo '**tool calls:**'
    echo '- `discover` {"query":"nope"}'
    echo '## 4. tool (discover)'
    echo
    echo '```'
    echo '{"hits":[],"query":"nope","shown":0,"state":"not_found"}'
    echo '```'
    echo '**tool calls:**'
    echo '- `discover` {"mode":"search","module":"Zoo","query":"colY"}'
    echo '## 5. tool (discover)'
    echo
    echo '```'
    echo '{"hits":[],"query":"colY","shown":0,"state":"not_found"}'
    echo '```'
    echo '**tool calls:**'
    echo "- \`run_cell\` {\"id\":1}"
    echo '## 6. tool (run_cell)'
    echo
    echo '```'
    printf '{"pad":"%s"}\n' "$(printf 'x%.0s' $(seq 1 2580))"
    echo '```'
} > "$fix/fixTask-s1-off.md"

# On arm: discover, then a write, then another discover (only the first one
# is before-first-write); no closures, empty final under max_turns.
{
    hdr fixTask on max_turns ""
    echo '# Session: fixTask'
    echo '**tool calls:**'
    echo '- `discover` {"query":"colY"}'
    echo '## 2. tool (discover)'
    echo
    echo '```'
    echo '{"hits":[{"install":"installed","name":"colY"}],"query":"colY","shown":1,"state":"found"}'
    echo '```'
    echo '**tool calls:**'
    echo '- `insert_cell` {"source":"x = 1"}'
    echo '## 3. tool (insert_cell)'
    echo
    echo '```'
    echo '{"cell":1,"state":"inserted"}'
    echo '```'
    echo '**tool calls:**'
    echo '- `discover` {"query":"colZ"}'
    echo '## 4. tool (discover)'
    echo
    echo '```'
    echo '{"hits":[{"name":"colZ"}],"query":"colZ","shown":1,"state":"found"}'
    echo '```'
} > "$fix/fixTask-s1-on.md"

out=$(python3 r10-metrics.py "$fix")

expect() {
    echo "$out" | grep -qF "$1" \
        || { echo "SELFTEST FAIL: missing '$1'"; echo "$out"; exit 1; }
}

# disc_bw: discover calls before the first write (insert_cell /
# replace_cell_source / propose_edit); wrote=no when no write ever landed.
# facts_held: discover calls after the first call-ready envelope (exact typed
# top hit) and before the first write — the off arm's colX hit is untyped so
# no fact is ever call-ready (0); the on arm's colY hit is typed but the
# write follows immediately (0).
expect "fixTask off discover=4 disc_bw=4 wrote=no dup=1 not_found=2 rephrase=1 facts_held=0"
expect "fixTask on discover=2 disc_bw=1 wrote=yes dup=0 not_found=0 rephrase=0 facts_held=0"
expect "facts-held-breaches(>2): 0"
expect "empty-finals: 1"
expect "budget-breaches(>2500): 1"
expect "useful=3 noise=3"
expect "unlabeled-stops: 0"
# S13: exactly the scoped colY denial is a false denial (resolved plain on
# the sibling arm); the never-resolved "nope" stays a legal R5.4 absence.
expect "false-denials(per-tuple): 1"
expect "FALSE-DENIAL[fixTask-off]: query='colY' mode='search' module='Zoo' package=''"
if echo "$out" | grep -q "FALSE-DENIAL.*nope"; then
    echo "SELFTEST FAIL: genuine absence 'nope' counted as a false denial"
    exit 1
fi
./r10-tuples-property-test.py
./r10-factsheld-property-test.py
echo "SELFTEST OK"
