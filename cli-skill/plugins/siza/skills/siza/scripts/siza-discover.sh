#!/usr/bin/env bash
# siza-discover.sh — list running Sabela servers by scanning the local registry.
#
# Prints one JSON object per live server, each augmented with a `live: true|false`
# field determined by hitting /api/ai/health. Non-live entries are omitted.
#
# No args. Honors $SABELA_URL to short-circuit discovery and probe a specific URL.

set -euo pipefail

probe() {
    local base="$1"
    local token="${SABELA_AI_TOKEN:-}"
    local hdr=()
    if [ -n "$token" ]; then
        hdr=(-H "Authorization: Bearer $token")
    fi
    curl -sS --max-time 2 "${hdr[@]}" "$base/api/ai/health" 2>/dev/null
}

if [ -n "${SABELA_URL:-}" ]; then
    body=$(probe "$SABELA_URL" || true)
    if [ -n "$body" ]; then
        printf '%s' "$body" \
            | jq --arg url "$SABELA_URL" '. + {baseUrl: $url, live: true}'
    fi
    exit 0
fi

reg_dir="${XDG_STATE_HOME:-$HOME/.local/state}/sabela/servers"
if [ ! -d "$reg_dir" ]; then
    echo "[]"
    exit 0
fi

results="[]"
for f in "$reg_dir"/*.json; do
    [ -e "$f" ] || continue
    entry=$(cat "$f")
    base=$(printf '%s' "$entry" | jq -r '.baseUrl')
    body=$(probe "$base" || true)
    if [ -n "$body" ]; then
        merged=$(jq -n --argjson a "$entry" --argjson b "$body" '$a + $b + {live: true}')
        results=$(jq -n --argjson acc "$results" --argjson x "$merged" '$acc + [$x]')
    fi
done
printf '%s\n' "$results"
