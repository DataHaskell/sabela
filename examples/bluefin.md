# A tour of Bluefin

[Bluefin](https://hackage.haskell.org/package/bluefin-0.6.0.0/docs/Bluefin.html)
is an effect system that lets you freely mix a variety of effects: early return,
exceptions, mutable state, streams, and `IO`. What makes it distinctive is that
**effects are accessed through value-level capabilities**: each effect is a
*handle* you receive from a `run*` function and pass explicitly into the
operations that use it, rather than a constraint smeared across a monad.

This notebook is a port of the examples from Tom Ellis's
[`Bluefin` module documentation](https://hackage.haskell.org/package/bluefin-0.6.0.0/docs/Bluefin.html).
Fork it and run the cells to follow along.

<!-- sabela:cell -->

Everything below draws on a handful of modules. `Eff es a` is a computation
producing an `a` whose available effects are tracked in `es`. `runPureEff`
discharges a computation needing no `IO`; `runEff` is its `IO`-aware sibling. The
`Modify` capability is Bluefin's mutable cell (think `IORef`), and `Exception` is
its checked-exception capability.
```haskell
-- cabal: build-depends: bluefin >= 0.6
{-# LANGUAGE TypeOperators #-}
import Bluefin.Eff (Eff, runPureEff, runEff, (:&), (:>))
import Bluefin.Capability.Modify (Modify, evalModify, runModify, get, put, modify)
import Bluefin.Exception (Exception, try, throw)
import Bluefin.IO (IOE, effIO)
import Bluefin.Jump (Jump, withJump, jumpTo)
import Control.Monad (when, forever)
import Data.Foldable (for_)
import Text.Read (readMaybe)
```

## Mutable state

`evalModify` introduces a `Modify` capability initialised to a starting value
and hands it to your block as a handle (`sn` here). Through it you `get`, `put`,
and `modify` the cell. The capability behaves like an `STRef` or `IORef`, but the
whole computation reduces to a pure value.

```haskell
-- If `n < 10` then add 10 to it, otherwise
-- return it unchanged
example1 :: Int -> Int
example1 n = runPureEff $
  evalModify n $ \sn -> do
    n' <- get sn
    when (n' < 10) $
      modify sn (+ 10)
    get sn
```

```haskell
example1 5
```

> <!-- scripths:mime text/plain -->
> 15

```haskell
example1 12
```

> <!-- scripths:mime text/plain -->
> 12

## Multiple effects of the same type

Because a capability is just a value, you can have several of the same kind in
scope at once and keep them straight: `sm` and `sn` are two independent `Modify`
cells. No newtype tagging, no lifting.

```haskell
-- Compare two values and add 10
-- to the smaller
example2 :: (Int, Int) -> (Int, Int)
example2 (m, n) = runPureEff $
  evalModify m $ \sm -> do
    evalModify n $ \sn -> do
      do
        n' <- get sn
        m' <- get sm

        if n' < m'
          then modify sn (+ 10)
          else modify sm (+ 10)

      n' <- get sn
      m' <- get sm

      pure (n', m')
```

```haskell
example2 (5, 10)
```

> <!-- scripths:mime text/plain -->
> (10,15)

```haskell
example2 (30, 3)
```

> <!-- scripths:mime text/plain -->
> (13,30)

## Exceptions

`try` introduces an `Exception` capability and runs your block; the result is an
`Either`. Inside, `throw` short-circuits to the matching `try`. The exception
capability cannot escape its handler's scope, so every `throw` is guaranteed to
be caught by the `try` that introduced it.

```haskell
example3 :: Int -> Either String Int
example3 n = runPureEff $
  try $ \ex -> do
    evalModify 0 $ \total -> do
      for_ [1 .. n] $ \i -> do
        soFar <- get total
        when (soFar > 20) $ do
          throw ex ("Became too big: " ++ show soFar)
        put total (soFar + i)

      get total
```

```haskell
example3 4
```

> <!-- scripths:mime text/plain -->
> Right 10

```haskell
example3 10
```

> <!-- scripths:mime text/plain -->
> Left "Became too big: 21"

## Effect scoping

The phantom scope on a capability's type is what keeps `runPureEff` sound: a
`Modify` cell introduced by `evalModify` cannot leak out of its handler. This
version uses the cell only inside the block, so it is well scoped:

```haskell
-- Result: 1100
correctlyScoped :: Eff es Integer
correctlyScoped = do
  r <- evalModify 0 $ \st -> do
    for_ [1 .. 10] $ \i -> do
      modify st (+ i)
    get st
  pure (r * 20)
```

```haskell
runPureEff correctlyScoped
```

> <!-- scripths:mime text/plain -->
> 1100

Returning the handle `st` from the block and using it afterwards is a *type
error*, not a runtime bug: the scope variable `e` would escape. Bluefin rejects
it at compile time, exactly as `runST` rejects a leaked `STRef`:

```haskell
incorrectlyScoped :: Eff es Integer
incorrectlyScoped = do
  (total, st) <- evalModify 0 $ \st -> do
    for_ [1 .. 10] $ \i -> do
      modify st (+ i)
    r <- get st
    pure (r, st)

  modify st (* 20)   -- using st out here ...
  get st

-- error: Couldn't match type 'e0' with 'e'
--   because type variable 'e' would escape its scope
```

## Type signatures, and reaching `IO`

A function that needs several capabilities just takes them as arguments. The
`e <: es` constraints (an alias for `e :> es`) read as "effect `e` is available
in the set `es`", so the function declares the capabilities it needs without
pinning down the whole effect stack. `effIO` lifts an `IO` action through an
`IOE` handle, and `withJump`/`jumpTo` give an untyped early return.

```haskell
incrementReadLine ::
  (e1 :> es, e2 :> es, e3 :> es) =>
  Modify Int e1 ->
  Exception String e2 ->
  IOE e3 ->
  Eff es ()
incrementReadLine state exception io = do
  withJump $ \break -> forever $ do
    line <- effIO io getLine
    i <- case readMaybe line of
      Nothing ->
        throw exception ("Couldn't read: " ++ line)
      Just i ->
        pure i

    when (i == 0) $
      jumpTo break

    modify state (+ i)
```

`runEff` hands you the `IOE` handle; `try` and `runModify` supply the other two.
Reading lines until a `0`, then returning the running total:

```haskell
runIncrementReadLine :: IO (Either String Int)
runIncrementReadLine = runEff $ \io -> do
  try $ \exception -> do
    ((), r) <- runModify 0 $ \state -> do
      incrementReadLine state exception io
    pure r
```

```haskell
runIncrementReadLine
```

## Putting it together

A larger example combining two `Modify` cells with an early exit via an
`Exception`: count positives and negatives in a list, but bail out the moment a
zero appears.

```haskell
countPositivesNegatives :: [Int] -> String
countPositivesNegatives is = runPureEff $
  evalModify (0 :: Int) $ \positives -> do
    r <- try $ \ex ->
      evalModify (0 :: Int) $ \negatives -> do
        for_ is $ \i -> do
          case compare i 0 of
            GT -> modify positives (+ 1)
            EQ -> throw ex ()
            LT -> modify negatives (+ 1)

        p <- get positives
        n <- get negatives

        pure $
          "Positives: "
            ++ show p
            ++ ", negatives "
            ++ show n

    case r of
      Right r' -> pure r'
      Left () -> do
        p <- get positives
        pure $
          "We saw a zero, but before that there were "
            ++ show p
            ++ " positives"
```

```haskell
countPositivesNegatives [1, -2, 3, -4, 5]
```

> <!-- scripths:mime text/plain -->
> "Positives: 3, negatives 2"

```haskell
countPositivesNegatives [1, 2, 0, 3, -1]
```

> <!-- scripths:mime text/plain -->
> "We saw a zero, but before that there were 2 positives"

Read Bluefin top to bottom and the shape never changes: every effect arrives as
an explicit handle, scoped by the `run*` that introduced it, and ordinary
functions move those handles around. The full reference, with comparisons to
other effect systems, is in the
[`Bluefin` documentation](https://hackage.haskell.org/package/bluefin-0.6.0.0/docs/Bluefin.html).
