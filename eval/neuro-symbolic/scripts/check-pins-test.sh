#!/usr/bin/env bash
# Self-test for check-pins.py: a pinned episode over its byte pin must BREACH
# (exit 1), one within its pins must print WITHIN, and an episode with no pin
# row must surface as a FIRST-PIN candidate with its measured numbers.
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
stopped: done
final: done
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

# pinnedTask off: 1 discover, small file — but pinned at 100 bytes: BREACH.
{
    hdr pinnedTask off
    echo '## 2. tool (discover)'
    echo
    echo '```'
    echo '{"hits":[{"name":"colX"}],"query":"colX","shown":1,"state":"found"}'
    echo '```'
} > "$fix/pinnedTask-s1-off.md"

# pinnedTask on: 0 discover, pinned generously: WITHIN.
{
    hdr pinnedTask on
    echo 'no tools'
} > "$fix/pinnedTask-s1-on.md"

# freshTask off: no pin row: FIRST-PIN candidate.
{
    hdr freshTask off
    echo 'no tools'
} > "$fix/freshTask-s1-off.md"

# factsTask off: a call-ready fact (exact typed hit) then 3 more discover
# calls before any write — facts_held=3 over the <=2 pin: BREACH.
{
    hdr factsTask off
    echo '**tool calls:**'
    echo '- `discover` {"query":"colR"}'
    echo '## 2. tool (discover)'
    echo
    echo '```'
    echo '{"hits":[{"install":"installed","matchKind":"exact","name":"colR","type":"colR :: Int"}],"query":"colR","shown":1,"state":"found"}'
    echo '```'
    for i in 3 4 5; do
        echo '**tool calls:**'
        echo "- \`discover\` {\"query\":\"more$i\"}"
        echo "## $i. tool (discover)"
        echo
        echo '```'
        echo "{\"hits\":[],\"query\":\"more$i\",\"shown\":0,\"state\":\"not_found\"}"
        echo '```'
    done
} > "$fix/factsTask-s1-off.md"

# goalTask off: a clean deliverable write, then 3 discover calls —
# disc_after_goal=3 over the <=2 pin: BREACH (R9-T5).
{
    hdr goalTask off
    echo '**tool calls:**'
    echo '- `insert_cell` {"source":"total = 600"}'
    echo '## 2. tool (insert_cell)'
    echo
    echo '```'
    echo '{"cellId":1,"execution":{"ok":true,"outcome":{"tag":"Succeeded"}},"status":"completed"}'
    echo '```'
    for i in 3 4 5; do
        echo '**tool calls:**'
        echo "- \`discover\` {\"query\":\"late$i\"}"
        echo "## $i. tool (discover)"
        echo
        echo '```'
        echo "{\"hits\":[],\"query\":\"late$i\",\"shown\":0,\"state\":\"not_found\"}"
        echo '```'
    done
} > "$fix/goalTask-s1-off.md"

pins="$fix/pins.tsv"
{
    echo '# task	arm	max_bytes	max_discover	max_disc_bw	max_facts_held	max_disc_after_goal'
    echo 'pinnedTask	off	100	0	-	2	2'
    echo 'pinnedTask	on	50000	0	-	2	2'
    echo 'factsTask	off	50000	-	-	2	2'
    echo 'goalTask	off	50000	-	-	2	2'
} > "$pins"

set +e
out=$(python3 check-pins.py "$fix" "$pins")
code=$?
set -e

expect() {
    echo "$out" | grep -qF "$1" \
        || { echo "SELFTEST FAIL: missing '$1'"; echo "$out"; exit 1; }
}

[ "$code" -eq 1 ] || { echo "SELFTEST FAIL: exit $code, want 1"; exit 1; }
expect "BREACH pinnedTask off bytes"
expect "BREACH pinnedTask off discover"
expect "BREACH factsTask off facts_held=3 pin<=2"
expect "BREACH goalTask off disc_after_goal=3 pin<=2"
expect "WITHIN pinnedTask on"
expect "FIRST-PIN freshTask off bytes="
echo "SELFTEST OK"
