#!/usr/bin/env bash
# siza-tool.sh — invoke a Sabela AI tool against a running notebook.
#
# Usage:
#   siza-tool.sh <tool_name> [json_input]
#   echo '{"cell_id":3}' | siza-tool.sh execute_cell
#
# Examples:
#   siza-tool.sh list_cells
#   siza-tool.sh read_cell '{"cell_id":1}'
#   siza-tool.sh execute_cell '{"cell_id":3}'
#
# Env:
#   SABELA_URL         Base URL (e.g. http://localhost:3000). Auto-discovered if unset.
#   SABELA_AI_TOKEN    Bearer token when the server has auth enabled.
#   SABELA_SESSION     X-Sabela-Session value. Defaults to a stable per-terminal UUID.
#
# Writes pretty JSON to stdout. Exits non-zero if the tool returned isError=true.

set -euo pipefail

tool_name="${1:-}"
if [ -z "$tool_name" ]; then
    echo "usage: siza-tool.sh <tool_name> [json_input]" >&2
    exit 2
fi
shift || true

if [ $# -ge 1 ]; then
    input_json="$1"
else
    if [ -t 0 ]; then
        input_json="{}"
    else
        input_json="$(cat)"
        [ -z "$input_json" ] && input_json="{}"
    fi
fi

# Resolve base URL.
base="${SABELA_URL:-}"
if [ -z "$base" ]; then
    script_dir="$(cd "$(dirname "$0")" && pwd)"
    first_live=$("$script_dir/siza-discover.sh" | jq -r '.[0].baseUrl // empty')
    if [ -z "$first_live" ]; then
        echo "siza-tool: no live Sabela server found. Set SABELA_URL or start sabela." >&2
        exit 3
    fi
    base="$first_live"
fi

# Warn on non-localhost — trust-bounded posture (same as marimo-pair).
case "$base" in
    http://localhost:*|http://127.0.0.1:*|http://[::1]:*)
        ;;
    *)
        echo "siza-tool: sending data to non-localhost URL ($base) — ensure this is intentional." >&2
        ;;
esac

# Stable session id per terminal.
if [ -z "${SABELA_SESSION:-}" ]; then
    SABELA_SESSION="siza-${HOSTNAME:-host}-${PPID}"
fi

headers=(-H "content-type: application/json" -H "X-Sabela-Session: $SABELA_SESSION")
if [ -n "${SABELA_AI_TOKEN:-}" ]; then
    headers+=(-H "Authorization: Bearer $SABELA_AI_TOKEN")
fi

body=$(jq -nc --arg n "$tool_name" --argjson i "$input_json" '{name:$n, input:$i}')

resp=$(curl -sS --max-time 60 "${headers[@]}" -d "$body" "$base/api/ai/tool")
printf '%s\n' "$resp" | jq .

is_error=$(printf '%s' "$resp" | jq -r '.isError // false')
[ "$is_error" = "true" ] && exit 1 || exit 0
