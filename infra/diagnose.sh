#!/usr/bin/env bash
set -x

echo "=== Environment ==="
echo "PATH=$PATH"
which cabal ghc 2>&1
cabal --version 2>&1
ghc --version 2>&1

echo "=== Cabal store ==="
ls /root/.cabal/store/ 2>&1

echo "=== Create test project ==="
PROJDIR=/tmp/test-repl
mkdir -p "$PROJDIR"
cat > "$PROJDIR/cabal.project" << 'EOF'
packages: .
EOF
cat > "$PROJDIR/Main.hs" << 'EOF'
main :: IO ()
main = pure ()
EOF
cat > "$PROJDIR/sabela-repl.cabal" << 'EOF'
cabal-version: 3.0
name: sabela-repl
version: 0.1.0.0
executable main
  main-is: Main.hs
  build-depends: base
EOF

echo "=== Test 1: basic cabal repl ==="
cd /opt/sabela
timeout 60 cabal repl exe:main \
  --project-dir="$PROJDIR" \
  -v0 \
  --repl-options="-fobject-code -O2" \
  --ghc-options="+RTS -N -A512m -H1G -RTS" \
  -O2 \
  <<< "1 + 1" 2>&1
echo "Exit: $?"

echo "=== Test 2: simple ghci ==="
timeout 30 ghci <<< "1 + 1" 2>&1
echo "Exit: $?"

echo "=== Done ==="
