# Build and test the sabela package under the same -Werror promotions
# .github/workflows/haskell-ci.yml injects into its generated cabal.project,
# so a warning that fails the Linux matrix fails here first.
#
# Sourced by scripts/presubmit.sh; not executable on its own.

# -Werror=missing-home-modules is NOT one of CI's flags. It is here because a
# module missing from the .cabal file compiles locally and then fails the
# Windows job as "Could not find module" — this makes that a local error.
WERROR_GHC_OPTIONS="-Werror=missing-methods -Werror=missing-fields \
-Werror=unused-packages -Werror=incomplete-patterns \
-Werror=missing-home-modules"

# A separate builddir so toggling these flags never invalidates the plain
# `cabal build` tree the developer is iterating in.
WERROR_BUILDDIR="dist-newstyle/presubmit"
WERROR_PROJECT=".presubmit.cabal.project"

# Write a project file that inherits the checked-in one (plus any local
# override) and adds the promotions for the sabela package only.
werror_project() {
    {
        echo "import: cabal.project"
        [ -f cabal.project.local ] && echo "import: cabal.project.local"
        echo "package sabela"
        echo "    ghc-options: $WERROR_GHC_OPTIONS"
    } > "$WERROR_PROJECT"
}

werror_build() {
    werror_project
    cabal build \
        --project-file="$WERROR_PROJECT" \
        --builddir="$WERROR_BUILDDIR" \
        --enable-tests \
        sabela:lib:sabela sabela:sabela-test lib:siza-client exe:siza \
        siza-eval:siza-eval-test
}

# The eval agent-loop specs. CI's Linux matrix builds an sdist of the sabela
# package alone, so it cannot reach eval/neuro-symbolic at all; without this
# gate the package compile-rots unseen, which is exactly what happened.
# Hermetic and ~2s: no model, no server, so it runs even under --skip-live.
werror_eval_test() {
    werror_project
    cabal test \
        --project-file="$WERROR_PROJECT" \
        --builddir="$WERROR_BUILDDIR" \
        siza-eval:siza-eval-test --test-show-details=direct
}

# Pass --skip-live to mark the integration specs pending (see `make test-fast`).
werror_test() {
    werror_project
    if [ "${1:-}" = "--skip-live" ]; then
        SABELA_SKIP_LIVE=1 cabal test \
            --project-file="$WERROR_PROJECT" \
            --builddir="$WERROR_BUILDDIR" \
            sabela:sabela-test --test-show-details=direct
    else
        cabal test \
            --project-file="$WERROR_PROJECT" \
            --builddir="$WERROR_BUILDDIR" \
            sabela:sabela-test --test-show-details=direct
    fi
}
