#!/usr/bin/env bash
set -uo pipefail

# Every gate CI runs, in one command, so a push cannot discover them for you.
# Mirrors .github/workflows/haskell-ci.yml (frontend, fourmolu, hlint, the
# Linux matrix) and .github/workflows/windows.yml (build + test).
#
# It also runs one gate CI cannot: the eval agent-loop specs. CI's Linux job
# builds an sdist of the sabela package alone, so eval/neuro-symbolic is
# unreachable there and this script is its only guard.
#
# Usage:
#   ./scripts/presubmit.sh              # everything
#   ./scripts/presubmit.sh --quick      # skip the compile + test stage
#   ./scripts/presubmit.sh --skip-live  # compile + test, integration specs pending
#
# Runs every gate even after one fails, then reports the full list: a push is
# blocked by all of them, so seeing one at a time wastes a round trip.

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

QUICK=0
SKIP_LIVE=0
for arg in "$@"; do
    case "$arg" in
        --quick) QUICK=1 ;;
        --skip-live) SKIP_LIVE=1 ;;
        -h | --help)
            sed -n '3,12p' "$0" | sed 's/^# \{0,1\}//'
            exit 0
            ;;
        *)
            echo "unknown flag: $arg (try --help)" >&2
            exit 2
            ;;
    esac
done

FAILED=()

# Run one gate, remembering the failure rather than aborting the sweep.
gate() {
    local name="$1"
    shift
    printf '\n\033[1m==> %s\033[0m\n' "$name"
    if "$@"; then
        printf '\033[32m    ok\033[0m\n'
    else
        printf '\033[31m    FAILED\033[0m\n'
        FAILED+=("$name")
    fi
}

# The base branch the comment-style gate diffs against, matching CI's
# `github.base_ref || 'main'`.
BASE_REF="${BASE_REF:-main}"

prettier_check() {
    npx --yes --package=prettier@3.3.3 -- prettier --check \
        'static/src/**/*.{html,js,css}' 'tools/*.mjs'
}

comment_style() {
    git fetch --no-tags --quiet origin "$BASE_REF" 2>/dev/null || true
    BASE="origin/$BASE_REF" ./scripts/check-comments.sh
}

fourmolu_check() {
    local files
    mapfile -t files < <(git ls-files '*.hs' | while IFS= read -r f; do
        [ -f "$f" ] && printf '%s\n' "$f"
    done)
    [ "${#files[@]}" -gt 0 ] || return 0
    fourmolu --mode check "${files[@]}"
}

gate "prettier (frontend sources)" prettier_check
gate "bundled pages up to date" node tools/build-frontend.mjs --check
gate "module size cap" ./scripts/check-module-size.sh
gate "comment style (changed lines)" comment_style
gate "fourmolu" fourmolu_check
gate "hlint" ./scripts/lint.sh

if [ "$QUICK" -eq 0 ]; then
    # shellcheck source=/dev/null
    . "$ROOT/scripts/lib/werror-build.sh"
    gate "build under CI -Werror flags" werror_build
    if [ "$SKIP_LIVE" -eq 1 ]; then
        gate "test suite (integration pending)" werror_test --skip-live
    else
        gate "test suite" werror_test
    fi
    gate "siza-client specs" werror_client_test
    gate "eval agent-loop specs" werror_eval_test
fi

printf '\n'
if [ "${#FAILED[@]}" -eq 0 ]; then
    printf '\033[32m✓ presubmit clean — every CI gate passed locally.\033[0m\n'
    exit 0
fi

printf '\033[31m✗ %d gate(s) failed:\033[0m\n' "${#FAILED[@]}"
for f in "${FAILED[@]}"; do
    printf '  - %s\n' "$f"
done
exit 1
