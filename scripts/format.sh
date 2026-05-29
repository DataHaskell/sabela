#!/usr/bin/env bash
#
# Format every tracked source file in the repo.
#
# Default: format both Haskell (fourmolu) and frontend (prettier).
#   ./scripts/format.sh                  # write changes in place
#   ./scripts/format.sh --check          # report drift, exit non-zero, write nothing
#   ./scripts/format.sh --haskell-only   # skip the frontend pass
#   ./scripts/format.sh --frontend-only  # skip the Haskell pass
#
# The formatter set is intentionally small:
#   - fourmolu on every tracked *.hs (config: ./fourmolu.yaml).
#   - prettier on tracked static/*.{html,js,mjs,css} (config: ./.prettierrc.json).
#
# Both tools are bootstrapped on first run if they aren't already on $PATH —
# fourmolu via cabal, prettier via npx (so no global Node install is needed).
#
# Notes on prettier's HTML output:
#   '.html' files in static/ embed long <script>/<style> blocks. Prettier
#   reformats those embedded sections. If you only want Haskell formatting in
#   a quick checkpoint, pass --haskell-only.

set -euo pipefail

MODE="write"
SCOPE="all"

for arg in "$@"; do
    case "$arg" in
        --check)         MODE="check" ;;
        --haskell-only)  SCOPE="haskell" ;;
        --frontend-only) SCOPE="frontend" ;;
        --help|-h)
            sed -n '2,21p' "$0"
            exit 0
            ;;
        *)
            echo "Unknown flag: $arg" >&2
            echo "Try $0 --help" >&2
            exit 2
            ;;
    esac
done

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT_DIR"

# ---------------------------------------------------------------------------
# Haskell — fourmolu
# ---------------------------------------------------------------------------

run_fourmolu() {
    if ! command -v fourmolu >/dev/null 2>&1; then
        echo "fourmolu not found — installing fourmolu-0.17.0.0 via cabal..."
        cabal v2-install fourmolu-0.17.0.0 \
            --overwrite-policy=always --force-reinstalls
    fi

    # mapfile keeps filenames with spaces intact; --null is overkill for our paths.
    local files
    files=$(git ls-files '*.hs')
    if [[ -z "$files" ]]; then
        echo "[format] no tracked *.hs files; skipping fourmolu"
        return 0
    fi

    if [[ "$MODE" == "check" ]]; then
        echo "[format] fourmolu --mode check"
        # shellcheck disable=SC2086
        fourmolu --mode check $files
    else
        echo "[format] fourmolu --mode inplace"
        # shellcheck disable=SC2086
        fourmolu --mode inplace $files
    fi
}

# ---------------------------------------------------------------------------
# Frontend — prettier
# ---------------------------------------------------------------------------

run_prettier() {
    local files
    files=$(git ls-files \
        'static/*.html' 'static/*.js' 'static/*.mjs' 'static/*.css')
    if [[ -z "$files" ]]; then
        echo "[format] no tracked frontend files; skipping prettier"
        return 0
    fi

    if ! command -v npx >/dev/null 2>&1; then
        echo "[format] npx not found — install Node.js to enable prettier" >&2
        return 1
    fi

    # --yes accepts the prettier download on first run without an interactive
    # prompt; the binary is then cached for subsequent invocations.
    local prettier_args=(--yes --package=prettier@3.3.3 -- prettier)
    if [[ "$MODE" == "check" ]]; then
        echo "[format] npx prettier --check"
        # shellcheck disable=SC2086
        npx "${prettier_args[@]}" --check $files
    else
        echo "[format] npx prettier --write"
        # shellcheck disable=SC2086
        npx "${prettier_args[@]}" --write $files
    fi
}

# ---------------------------------------------------------------------------
# Dispatch
# ---------------------------------------------------------------------------

case "$SCOPE" in
    haskell)  run_fourmolu ;;
    frontend) run_prettier ;;
    all)      run_fourmolu; run_prettier ;;
esac
