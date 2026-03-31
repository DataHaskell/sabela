#!/usr/bin/env bash
set -euo pipefail

# Install all Sabela dependencies for local development.
# Usage: ./scripts/install-deps.sh
#
# This installs:
#   - GHCup + GHC + cabal (Haskell toolchain)
#   - elan + Lean 4 + Lake (Lean toolchain)
#   - Python 3 + numpy + pandas + scikit-learn
#   - HLint + fourmolu (dev tools)

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

ok()   { echo -e "${GREEN}[ok]${NC} $1"; }
warn() { echo -e "${YELLOW}[!!]${NC} $1"; }
fail() { echo -e "${RED}[FAIL]${NC} $1"; }

echo "================================================"
echo "  Sabela dependency installer"
echo "================================================"
echo ""

# ── Haskell ──────────────────────────────────────────

echo "── Haskell ──"

if command -v ghc &>/dev/null && command -v cabal &>/dev/null; then
    ok "GHC $(ghc --numeric-version) + cabal $(cabal --numeric-version) already installed"
else
    warn "Installing GHCup (GHC + cabal)..."
    curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
        BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
        BOOTSTRAP_HASKELL_GHC_VERSION=9.12.2 \
        BOOTSTRAP_HASKELL_CABAL_VERSION=latest \
        BOOTSTRAP_HASKELL_INSTALL_HLS=0 \
        sh
    # shellcheck disable=SC1090
    source "$HOME/.ghcup/env" 2>/dev/null || true
    ok "GHC $(ghc --numeric-version) + cabal installed"
fi

if command -v hlint &>/dev/null; then
    ok "HLint already installed"
else
    warn "Installing HLint..."
    cabal install hlint --install-method=copy --overwrite-policy=always 2>/dev/null || warn "HLint install failed (optional)"
fi

if command -v fourmolu &>/dev/null; then
    ok "fourmolu already installed"
else
    warn "Installing fourmolu..."
    cabal install fourmolu --install-method=copy --overwrite-policy=always 2>/dev/null || warn "fourmolu install failed (optional)"
fi

echo ""

# ── Lean 4 ───────────────────────────────────────────

echo "── Lean 4 ──"

if command -v lean &>/dev/null && command -v lake &>/dev/null; then
    ok "Lean $(lean --version | head -1 | awk '{print $4}') + Lake already installed"
else
    warn "Installing elan (Lean 4 toolchain manager)..."
    curl -sSf https://elan.lean-lang.org/install.sh | sh -s -- -y --default-toolchain leanprover/lean4:v4.29.0
    export PATH="$HOME/.elan/bin:$PATH"
    ok "Lean $(lean --version | head -1 | awk '{print $4}') installed"
fi

echo ""

# ── Python ───────────────────────────────────────────

echo "── Python ──"

if command -v python3 &>/dev/null; then
    ok "Python $(python3 --version | awk '{print $2}') already installed"
else
    fail "Python 3 not found. Please install it:"
    echo "  macOS:  brew install python3"
    echo "  Ubuntu: sudo apt install python3 python3-pip"
    echo "  Fedora: sudo dnf install python3 python3-pip"
fi

echo "  Checking Python packages..."
MISSING=()
for pkg in numpy pandas sklearn; do
    if python3 -c "import $pkg" 2>/dev/null; then
        ok "  $pkg installed"
    else
        MISSING+=("$pkg")
        warn "  $pkg missing"
    fi
done

if [ ${#MISSING[@]} -gt 0 ]; then
    warn "Installing missing Python packages..."
    # Map sklearn to scikit-learn for pip
    PIP_PKGS=()
    for pkg in "${MISSING[@]}"; do
        if [ "$pkg" = "sklearn" ]; then
            PIP_PKGS+=("scikit-learn")
        else
            PIP_PKGS+=("$pkg")
        fi
    done
    pip3 install "${PIP_PKGS[@]}" 2>/dev/null || \
        pip3 install --break-system-packages "${PIP_PKGS[@]}" 2>/dev/null || \
        warn "pip install failed. Try: pip3 install ${PIP_PKGS[*]}"
fi

echo ""

# ── Sabela ───────────────────────────────────────────

echo "── Sabela ──"

if [ -f "sabela.cabal" ]; then
    warn "Building Sabela..."
    cabal update 2>/dev/null
    cabal build 2>&1 | tail -3
    ok "Sabela built"
else
    warn "Not in Sabela directory. Run this script from the project root."
fi

echo ""
echo "================================================"
echo "  Setup complete. Run with: cabal run"
echo "================================================"
echo ""
echo "  Add these to your shell profile if not already:"
echo '    source "$HOME/.ghcup/env"'
echo '    export PATH="$HOME/.elan/bin:$PATH"'
echo ""
