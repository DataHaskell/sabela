#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
NAMES_OUT="$REPO_ROOT/data/hackage-packages.txt"
FACTS_OUT="$REPO_ROOT/data/hackage-facts.tsv"
META_OUT="$REPO_ROOT/data/search-cache.meta"
INDEX_TAR="${CABAL_INDEX_TAR:-$HOME/.cabal/packages/hackage.haskell.org/01-index.tar}"
XDG_DATA="${XDG_DATA_HOME:-$HOME/.local/share}"
HOOGLE_INPUT_TAR="${HOOGLE_INPUT_TAR:-$XDG_DATA/hoogle/input-haskell-hoogle.tar.gz}"
HACKAGE_DOCS_DIR="${SABELA_HACKAGE_DOCS_DIR:-$XDG_DATA/sabela/hackage-docs}"
HACKAGE_DB="${SABELA_HOOGLE_HACKAGE_DB:-$XDG_DATA/sabela/hoogle-hackage.hoo}"

DO_NAMES=1
DO_HOOGLE=1
DO_CAPABILITY=0
DO_LOCAL=0
DO_FACTS=0
DO_HACKAGE=0

LOCAL_UNIVERSES=""
LOCAL_DIRS=0
HACKAGE_DIRS=0
case "${1:-}" in
    --names-only) DO_HOOGLE=0 ;;
    --facts-only) DO_NAMES=0; DO_HOOGLE=0; DO_FACTS=1 ;;
    --hoogle-only) DO_NAMES=0 ;;
    --local-hoogle-only) DO_NAMES=0; DO_HOOGLE=0; DO_LOCAL=1 ;;
    --hackage-hoogle-only) DO_NAMES=0; DO_HOOGLE=0; DO_HACKAGE=1 ;;
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

# A client launched outside the repo cannot see data/, so the names and facts
# also land in the XDG mirror the in-process ladder falls back to.
MIRROR_DIR="$XDG_DATA/sabela"

mirror_data_file() {
    mkdir -p "$MIRROR_DIR"
    cp "$1" "$MIRROR_DIR/$(basename "$1")"
    echo "   mirrored -> $MIRROR_DIR/$(basename "$1")" >&2
}

write_names() {
    [ -f "$INDEX_TAR" ] || { echo "no Hackage index at $INDEX_TAR — run cabal update" >&2; exit 1; }
    echo "==> extracting package names from $INDEX_TAR" >&2
    tar tf "$INDEX_TAR" \
        | awk -F/ 'NF>=2 && $1 != "" {print $1}' \
        | sort -u > "$NAMES_OUT"
    echo "   wrote $(wc -l < "$NAMES_OUT" | tr -d ' ') package names -> $NAMES_OUT" >&2
    mirror_data_file "$NAMES_OUT"
}

write_facts() {
    [ -f "$INDEX_TAR" ] || { echo "no Hackage index at $INDEX_TAR — run cabal update" >&2; exit 1; }
    echo "==> extracting package facts from $INDEX_TAR" >&2
    CABAL_INDEX_TAR="$INDEX_TAR" cabal run -v0 siza-hackage-facts -- \
        --tar "$INDEX_TAR" --out "$FACTS_OUT" >&2
    mirror_data_file "$FACTS_OUT"
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

# Without --download, `hoogle generate` silently rebuilds from whatever inputs
# it cached last, so a refresh reproduces the old snapshot. This path already
# goes to the network for `cabal update`; the corpus is fetched with it.
generate_hoogle() {
    ensure_hoogle
    echo "==> hoogle generate --download (fetch + build local DB of all Hackage)" >&2
    hoogle generate --download >&2
    echo "==> smoke query (local DB): runConduit" >&2
    hoogle search --count=2 --jsonl runConduit >&2 2>/dev/null \
        || hoogle search --count=2 runConduit >&2
    generate_local_hoogle
}

hoogle_dirs_under() {
    find "$1" -name '*.txt' -path '*doc/html*' 2>/dev/null \
        | sed 's#/[^/]*$##' \
        | sort -u
}

repo_haddock_dirs() {
    (cd "$REPO_ROOT" && cabal haddock all --haddock-hoogle >&2) \
        || echo "   cabal haddock all failed; indexing whatever is already built" >&2
    hoogle_dirs_under "$REPO_ROOT/dist-newstyle"
}

store_package_db() {
    store="$(cabal path --store-dir 2>/dev/null || true)"
    [ -n "$store" ] || store="$HOME/.cabal/store"
    ver="$(ghc --numeric-version 2>/dev/null || true)"
    [ -n "$ver" ] || return 0
    for d in "$store/ghc-$ver" "$store/ghc-$ver"-*; do
        if [ -d "$d/package.db" ]; then echo "$d/package.db"; return 0; fi
    done
}

store_haddock_dirs() {
    db="$(store_package_db)"
    [ -n "$db" ] && [ -d "$db" ] || return 0
    ghc-pkg --package-db="$db" field --simple-output '*' haddock-html 2>/dev/null \
        | tr ' ' '\n' \
        | sed '/^$/d' \
        | sort -u \
        | while read -r d; do
            if [ -d "$d" ] && ls "$d"/*.txt >/dev/null 2>&1; then echo "$d"; fi
        done
}

sample_symbol() {
    doc="$(find "$1" -maxdepth 1 -name '*.txt' 2>/dev/null | head -1)"
    [ -n "$doc" ] || return 0
    awk "/^[a-zA-Z_][a-zA-Z0-9_']* :: /{print \$1; exit}" "$doc"
}

generate_local_hoogle() {
    local_db="$REPO_ROOT/data/hoogle-local.hoo"
    echo "==> hoogle generate (in-repo + store packages, one --local per package)" >&2
    args=()
    universes=""
    probe_dir=""
    while read -r d; do
        [ -n "$d" ] || continue
        args+=("--local=$d")
        [ -n "$probe_dir" ] || probe_dir="$d"
        case ",$universes," in *,repo,*) ;; *) universes="${universes:+$universes,}repo" ;; esac
    done <<EOF
$(repo_haddock_dirs)
EOF
    while read -r d; do
        [ -n "$d" ] || continue
        args+=("--local=$d")
        [ -n "$probe_dir" ] || probe_dir="$d"
        case ",$universes," in *,store,*) ;; *) universes="${universes:+$universes,}store" ;; esac
    done <<EOF
$(store_haddock_dirs)
EOF
    echo "   ${#args[@]} package doc dirs (universes: ${universes:-none})" >&2
    ok=0
    if hoogle generate --local ${args[@]+"${args[@]}"} --database="$local_db" >&2 \
        && local_db_answers "$local_db" "$probe_dir"; then
        universes="${universes:+$universes,}global"
        ok=1
    elif [ "${#args[@]}" -gt 0 ] \
        && hoogle generate ${args[@]+"${args[@]}"} --database="$local_db" >&2 \
        && local_db_answers "$local_db" "$probe_dir"; then
        echo "   compiler doc root unusable; indexed the named dirs only" >&2
        ok=1
    fi
    if [ "$ok" = 1 ]; then
        LOCAL_UNIVERSES="$universes"
        LOCAL_DIRS="${#args[@]}"
        echo "   wrote local index -> $local_db" >&2
        echo "   set SABELA_HOOGLE_LOCAL_DB=$local_db so queries union it in" >&2
    else
        rm -f "$local_db"
        echo "   local generation failed; installed-only symbols stay unindexed" >&2
    fi
}

local_db_answers() {
    [ -f "$1" ] || return 1
    [ -n "$2" ] || return 0
    sym="$(sample_symbol "$2")"
    [ -n "$sym" ] || return 0
    hoogle search --database="$1" --count=1 "$sym" 2>/dev/null | grep -q "$sym"
}

# Modification time in epoch seconds, spelled for both BSD and GNU stat.
file_epoch() {
    stat -f %m "$1" 2>/dev/null || stat -c %Y "$1" 2>/dev/null || echo 0
}

# Hoogle's own generate only symbol-indexes Stackage members, so the ~12.7k
# Hackage packages that ship haddock but are not on Stackage answer nothing.
# Their doc files are already in the input tarball; index them directly.
extract_hackage_docs() {
    [ -f "$HOOGLE_INPUT_TAR" ] || {
        echo "   no haddock input at $HOOGLE_INPUT_TAR — run \`hoogle generate\` once to download it" >&2
        return 1
    }
    if [ -d "$HACKAGE_DOCS_DIR" ] && [ "$HACKAGE_DOCS_DIR" -nt "$HOOGLE_INPUT_TAR" ]; then
        echo "   reusing extracted docs in $HACKAGE_DOCS_DIR" >&2
        return 0
    fi
    echo "   extracting $HOOGLE_INPUT_TAR -> $HACKAGE_DOCS_DIR" >&2
    rm -rf "$HACKAGE_DOCS_DIR"
    mkdir -p "$HACKAGE_DOCS_DIR"
    tar xzf "$HOOGLE_INPUT_TAR" -C "$HACKAGE_DOCS_DIR"
    touch "$HACKAGE_DOCS_DIR"
}

# One --local per package against 16k absolute paths overruns ARG_MAX, so the
# generate runs from inside the doc root and names the dirs relatively.
generate_hackage_hoogle() {
    ensure_hoogle
    echo "==> hoogle generate (all Hackage packages that ship haddock)" >&2
    extract_hackage_docs || return 0
    mkdir -p "$(dirname "$HACKAGE_DB")"
    args=()
    probe_dir=""
    while read -r d; do
        [ -n "$d" ] || continue
        args+=("--local=$d")
        [ -n "$probe_dir" ] || probe_dir="$HACKAGE_DOCS_DIR/$d"
    done <<EOF
$(cd "$HACKAGE_DOCS_DIR" && hoogle_dirs_under .)
EOF
    echo "   ${#args[@]} package doc dirs; this takes ~15 minutes and ~1.4 GB" >&2
    if [ "${#args[@]}" -gt 0 ] \
        && (cd "$HACKAGE_DOCS_DIR" && hoogle generate ${args[@]+"${args[@]}"} --database="$HACKAGE_DB" >&2) \
        && local_db_answers "$HACKAGE_DB" "$probe_dir"; then
        HACKAGE_DIRS="${#args[@]}"
        echo "   wrote Hackage index -> $HACKAGE_DB" >&2
        echo "   set SABELA_HOOGLE_HACKAGE_DB=$HACKAGE_DB so queries union it in" >&2
    else
        rm -f "$HACKAGE_DB"
        echo "   Hackage generation failed; non-Stackage packages stay unindexed" >&2
    fi
}

[ "$DO_NAMES" = 1 ] && { refresh_index; write_names; write_facts; }
[ "$DO_HOOGLE" = 1 ] && generate_hoogle
[ "$DO_FACTS" = 1 ] && write_facts
[ "$DO_LOCAL" = 1 ] && { ensure_hoogle; generate_local_hoogle; }
if [ "$DO_HACKAGE" = 1 ]; then generate_hackage_hoogle; fi
[ "$DO_CAPABILITY" = 1 ] && { build_capability_index; exit 0; }

carried_universes() {
    [ -f "$META_OUT" ] || return 0
    grep -E '^hoogle_local_(universes|dirs)=' "$META_OUT" || true
}

# The Hackage index is built by its own opt-in target, so every other run must
# carry its recorded provenance forward rather than dropping it from the meta.
carried_hackage() {
    [ -f "$META_OUT" ] || return 0
    grep -E '^hoogle_hackage_(db|dirs|input_epoch)=' "$META_OUT" || true
}

CARRIED="$(carried_universes)"
CARRIED_HACKAGE="$(carried_hackage)"

{
    echo "# machine-produced by tools/update-search-cache.sh — do not hand-edit"
    echo "generated_epoch=$(date +%s)"
    echo "generated_utc=$(date -u +%Y-%m-%dT%H:%M:%SZ)"
    [ -f "$NAMES_OUT" ] && echo "hackage_packages=$(wc -l < "$NAMES_OUT" | tr -d ' ')"
    [ -f "$FACTS_OUT" ] && echo "hackage_facts=$FACTS_OUT"
    [ -f "$FACTS_OUT" ] && echo "hackage_facts_packages=$(wc -l < "$FACTS_OUT" | tr -d ' ')"
    [ -f "$MIRROR_DIR/hackage-packages.txt" ] \
        && echo "names_mirror=$MIRROR_DIR/hackage-packages.txt"
    [ -f "$MIRROR_DIR/hackage-facts.tsv" ] \
        && echo "facts_mirror=$MIRROR_DIR/hackage-facts.tsv"
    command -v hoogle >/dev/null 2>&1 && echo "hoogle=$(command -v hoogle)"
    [ -f "$HOOGLE_INPUT_TAR" ] \
        && echo "hoogle_input_epoch=$(file_epoch "$HOOGLE_INPUT_TAR")"
    if [ -f "$REPO_ROOT/data/hoogle-local.hoo" ]; then
        echo "hoogle_local_db=$REPO_ROOT/data/hoogle-local.hoo"
        if [ -n "$LOCAL_UNIVERSES" ]; then
            echo "hoogle_local_universes=$LOCAL_UNIVERSES"
            echo "hoogle_local_dirs=$LOCAL_DIRS"
        elif [ -n "$CARRIED" ]; then
            echo "$CARRIED"
        fi
    fi
    if [ "$HACKAGE_DIRS" -gt 0 ] && [ -f "$HACKAGE_DB" ]; then
        echo "hoogle_hackage_db=$HACKAGE_DB"
        echo "hoogle_hackage_dirs=$HACKAGE_DIRS"
        echo "hoogle_hackage_input_epoch=$(file_epoch "$HOOGLE_INPUT_TAR")"
    elif [ -n "$CARRIED_HACKAGE" ]; then
        echo "$CARRIED_HACKAGE"
    fi
} > "$META_OUT"
echo "==> search cache updated; meta -> $META_OUT" >&2
