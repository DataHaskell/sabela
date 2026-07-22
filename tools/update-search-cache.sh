#!/usr/bin/env bash
# Build/refresh the LOCAL search cache the neuro-symbolic Hackage resolver
# queries: (1) the full Hackage package-name list and (2) a local Hoogle
# database. Queries at run time hit ONLY this local cache — never the public
# Hoogle/Hackage services — so the resolve loop is never throttled or rate
# limited. Rerun this command to refresh the cache when Hackage moves.
#
# Usage: ./tools/update-search-cache.sh [--names-only|--hoogle-only]
#
# Requires: cabal, ghc, tar. Installs `hoogle` into the cabal store if absent.
# Writes: data/hackage-packages.txt, data/search-cache.meta; the Hoogle DB
# lands in hoogle's default data dir ("$(hoogle --version)" prints it).
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
NAMES_OUT="$REPO_ROOT/data/hackage-packages.txt"
META_OUT="$REPO_ROOT/data/search-cache.meta"
INDEX_TAR="${CABAL_INDEX_TAR:-$HOME/.cabal/packages/hackage.haskell.org/01-index.tar}"

DO_NAMES=1
DO_HOOGLE=1
DO_CAPABILITY=0
case "${1:-}" in
    --names-only) DO_HOOGLE=0 ;;
    --hoogle-only) DO_NAMES=0 ;;
    # The SHIP capability-search index (vectors+sidecar+revdeps). Opt-in: it is
    # heavier and needs a local ollama (nomic-embed-text). See tools/build_capability_index.hs.
    --capability-index) DO_NAMES=0; DO_HOOGLE=0; DO_CAPABILITY=1 ;;
    "") ;;
    *) echo "unknown flag: $1" >&2; exit 2 ;;
esac

build_capability_index() {
    echo "==> building SHIP capability-search index (tools/build_capability_index.hs)" >&2
    CABAL_INDEX_TAR="$INDEX_TAR" cabal run -v0 "$REPO_ROOT/tools/build_capability_index.hs" -- \
        --data-dir "$REPO_ROOT/data" --tar "$INDEX_TAR" >&2
}

refresh_index() {
    echo "==> cabal update (refresh local Hackage index)" >&2
    cabal update >&2 || echo "   cabal update failed; using existing index" >&2
}

write_names() {
    [ -f "$INDEX_TAR" ] || { echo "no Hackage index at $INDEX_TAR — run cabal update" >&2; exit 1; }
    echo "==> extracting package names from $INDEX_TAR" >&2
    # Index entries are <pkg>/<version>/<pkg>.cabal; the top path segment is
    # the package name. Pure-local, no network.
    tar tf "$INDEX_TAR" \
        | awk -F/ 'NF>=2 && $1 != "" {print $1}' \
        | sort -u > "$NAMES_OUT"
    echo "   wrote $(wc -l < "$NAMES_OUT" | tr -d ' ') package names -> $NAMES_OUT" >&2
}

ensure_hoogle() {
    if command -v hoogle >/dev/null 2>&1; then
        echo "==> hoogle present: $(command -v hoogle)" >&2
        return
    fi
    echo "==> installing hoogle (cabal install hoogle)" >&2
    cabal install hoogle --overwrite-policy=always >&2
    command -v hoogle >/dev/null 2>&1 || {
        echo "hoogle still not on PATH after install; add cabal bindir to PATH" >&2
        exit 1
    }
}

generate_hoogle() {
    ensure_hoogle
    echo "==> hoogle generate (download + build local DB of all Hackage)" >&2
    hoogle generate >&2
    echo "==> smoke query (local DB): runConduit" >&2
    hoogle search --count=2 --jsonl runConduit >&2 2>/dev/null \
        || hoogle search --count=2 runConduit >&2
    generate_local_hoogle
}

# Index the WHOLE installed package DB — exposed AND hidden/local packages —
# from the local haddock docs, into a second database the resolver unions in
# (SABELA_HOOGLE_LOCAL_DB; see Sabela.AI.HoogleResolve.hoogleDbArgSets).
# Staleness contract: this snapshot is as of generation time; packages
# installed afterwards are unindexed until the next run (the discover session
# evidence still classifies their install state live).
generate_local_hoogle() {
    local_db="$REPO_ROOT/data/hoogle-local.hoo"
    echo "==> hoogle generate --local (whole installed package DB, hidden included)" >&2
    if hoogle generate --local --database="$local_db" >&2; then
        echo "   wrote installed-DB index -> $local_db" >&2
        echo "   set SABELA_HOOGLE_LOCAL_DB=$local_db so queries union it in" >&2
    else
        echo "   local generation failed; installed-only symbols stay unindexed" >&2
    fi
}

[ "$DO_NAMES" = 1 ] && { refresh_index; write_names; }
[ "$DO_HOOGLE" = 1 ] && generate_hoogle
[ "$DO_CAPABILITY" = 1 ] && { build_capability_index; exit 0; }

{
    echo "# machine-produced by tools/update-search-cache.sh — do not hand-edit"
    echo "generated_epoch=$(date +%s)"
    echo "generated_utc=$(date -u +%Y-%m-%dT%H:%M:%SZ)"
    [ -f "$NAMES_OUT" ] && echo "hackage_packages=$(wc -l < "$NAMES_OUT" | tr -d ' ')"
    command -v hoogle >/dev/null 2>&1 && echo "hoogle=$(command -v hoogle)"
    [ -f "$REPO_ROOT/data/hoogle-local.hoo" ] \
        && echo "hoogle_local_db=$REPO_ROOT/data/hoogle-local.hoo"
} > "$META_OUT"
echo "==> search cache updated; meta -> $META_OUT" >&2
