#!/usr/bin/env bash
set -euo pipefail

EFS="/mnt/efs"

echo "=== EFS Builder: populating $EFS ==="

# ---------- Python venv ----------
echo "[1/4] Creating Python venv..."
python3 -m venv "$EFS/python/venv"
"$EFS/python/venv/bin/pip" install --no-cache-dir --no-compile \
  numpy pandas scikit-learn matplotlib
find "$EFS/python/venv" -name '__pycache__' -exec rm -rf {} + 2>/dev/null || true
echo "  Python venv ready at $EFS/python/venv"

# ---------- Lean 4 toolchain ----------
echo "[2/4] Installing Lean 4 toolchain..."
export ELAN_HOME="$EFS/lean/.elan"
curl -sSf https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh \
  | sh -s -- -y --default-toolchain leanprover/lean4:v4.29.0
export PATH="$ELAN_HOME/bin:$PATH"
echo "  Lean installed: $(lean --version)"

# ---------- Lean REPL ----------
echo "[3/4] Building Lean REPL..."
cp -a /opt/repl "$EFS/lean/repl"
cd "$EFS/lean/repl"
lake build
echo "  REPL binary at $EFS/lean/repl/.lake/build/bin/repl"

# ---------- Lean + Mathlib ----------
echo "[4/4] Building Lean + Mathlib (this takes a while)..."
mkdir -p "$EFS/lean/lean-base"
cd "$EFS/lean/lean-base"
cat > lakefile.toml <<'TOML'
name = "scratch"
version = "0.1.0"

[[lean_lib]]
name = "Scratch"

[[require]]
name = "mathlib"
scope = "leanprover-community"
TOML
echo 'leanprover/lean4:v4.29.0' > lean-toolchain
lake build
echo "  Mathlib built at $EFS/lean/lean-base"

echo ""
echo "=== EFS Builder complete ==="
echo "Contents:"
du -sh "$EFS/python" "$EFS/lean" 2>/dev/null || true
