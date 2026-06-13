# Compile mode benchmark

Cells run interpreted in GHCi by default. A `-- compile` cell's declarations
are built into a generated module at `-O2` instead — native code. This
notebook times the same strict loop both ways.

The interpreted version, defined at the GHCi prompt:

```haskell
-- cabal: default-extensions: BangPatterns
sumToInterp :: Int -> Int
sumToInterp n = go 0 1
  where
    go !acc i
        | i > n = acc
        | otherwise = go (acc + i) (i + 1)
```

The compiled version — identical code, marked `-- compile`:

```haskell
-- cabal: default-extensions: BangPatterns
-- compile
sumToCompiled :: Int -> Int
sumToCompiled n = go 0 1
  where
    go !acc i
        | i > n = acc
        | otherwise = go (acc + i) (i + 1)
```

A tiny wall-clock timer (interpreted; the work it measures is what matters):

```haskell
import Control.Exception (evaluate)
import GHC.Clock (getMonotonicTime)
```

```haskell
timeIt :: String -> Int -> IO ()
timeIt label x = do
    t0 <- getMonotonicTime
    _ <- evaluate x
    t1 <- getMonotonicTime
    putStrLn (label <> ": " <> show (t1 - t0) <> "s")
```

Now race them over ten million iterations. Expect the interpreted loop to
take seconds and the compiled one to take milliseconds:

```haskell
timeIt "interpreted" (sumToInterp 10000000)
```

```haskell
timeIt "compiled   " (sumToCompiled 10000000)
```

The boundary matters: the loop must live *inside* the compiled cell.
Returning a lazy structure that an interpreted cell consumes element-by-
element keeps the consumption loop interpreted, and most of the slowness
with it.
