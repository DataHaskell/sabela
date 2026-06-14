# A tour of Bluefin

[Bluefin](https://hackage.haskell.org/package/bluefin-0.6.0.0/docs/Bluefin.html)
is an effect system where every effect is a *value* you hold, not a constraint
smeared across a monad. You get a *handle* from a `run*` function and pass it
explicitly into the operations that use it. Because handles are ordinary
arguments, you can have several effects of the same type in scope at once, and
the type checker stops an effect from escaping the block that introduced it (the
same trick `ST` uses to keep an `STRef` from leaking).

This notebook ports the examples from Tom Ellis's
[`Bluefin` documentation](https://hackage.haskell.org/package/bluefin-0.6.0.0/docs/Bluefin.html)
and makes every one of them runnable, so you can edit a value and watch the
result change. Fork it to experiment.

<!-- sabela:cell -->

Everything below uses a handful of modules. `Eff es a` is a computation
producing an `a` whose available effects are tracked in the type-level set `es`.
`runPureEff` discharges a computation that needs no `IO`. `Modify` is Bluefin's
mutable cell (think `IORef`), `Exception` is its checked exception, and `Jump`
is an early return.
```haskell
-- cabal: build-depends: bluefin >= 0.6
{-# LANGUAGE TypeOperators #-}
import Bluefin.Eff (Eff, runPureEff, (:&), (:>))
import Bluefin.Capability.Modify (Modify, evalModify, get, put, modify)
import Bluefin.Exception (Exception, try, throw)
import Bluefin.Jump (Jump, withJump, jumpTo)
import Control.Monad (when)
import Data.Foldable (for_)
import Text.Read (readMaybe)
```

## Mutable state, but pure

`evalModify` hands your block a `Modify` handle initialised to a starting value.
Through it you `get`, `put`, and `modify` the cell. It reads imperatively, yet
the whole thing reduces to a pure value: nothing here touches `IO`.

```haskell
-- If n < 10 then add 10 to it, otherwise leave it unchanged.
example1 :: Int -> Int
example1 n = runPureEff $
  evalModify n $ \sn -> do
    n' <- get sn
    when (n' < 10) $ modify sn (+ 10)
    get sn
```

```haskell
example1 5
```

```haskell
example1 12
```

## Several effects of the same type

Because a handle is just a value, you can keep two `Modify` cells straight at
once. No newtype tagging, no lifting: `sm` and `sn` are independent.

```haskell
-- Compare two values and add 10 to the smaller.
example2 :: (Int, Int) -> (Int, Int)
example2 (m, n) = runPureEff $
  evalModify m $ \sm ->
    evalModify n $ \sn -> do
      n' <- get sn
      m' <- get sm
      if n' < m' then modify sn (+ 10) else modify sm (+ 10)
      (,) <$> get sm <*> get sn
```

```haskell
example2 (5, 10)
```

```haskell
example2 (30, 3)
```

## Exceptions that cannot escape

`try` introduces an `Exception` handle and runs your block, returning an
`Either`. Inside, `throw` short-circuits to the matching `try`. The handle is
scoped, so every `throw` is guaranteed to be caught by the `try` that made it.

```haskell
example3 :: Int -> Either String Int
example3 n = runPureEff $
  try $ \ex ->
    evalModify (0 :: Int) $ \total -> do
      for_ [1 .. n] $ \i -> do
        soFar <- get total
        when (soFar > 20) $ throw ex ("Became too big: " ++ show soFar)
        put total (soFar + i)
      get total
```

```haskell
example3 4
```

```haskell
example3 10
```

## Effect scoping is enforced by the type checker

The phantom scope on a handle keeps `runPureEff` sound. This version only uses
the cell inside its block, so it is well scoped:

```haskell
correctlyScoped :: Int
correctlyScoped = runPureEff $ do
  r <- evalModify 0 $ \st -> do
    for_ [1 .. 10] $ \i -> modify st (+ i)
    get st
  pure (r * 20)
```

```haskell
correctlyScoped
```

Returning the handle and using it after its block is a *compile error*, not a
runtime bug. The scope variable would escape, so Bluefin rejects it, exactly as
`runST` rejects a leaked `STRef`:

```text
incorrectlyScoped :: Eff es Integer
incorrectlyScoped = do
  (total, st) <- evalModify 0 $ \st -> do
    for_ [1 .. 10] $ \i -> modify st (+ i)
    r <- get st
    pure (r, st)
  modify st (* 20)   -- using st out here ...
  get st

-- error: Couldn't match type 'e0' with 'e'
--   because the type variable 'e' would escape its scope
```

## Three effects at once: state, early return, and exceptions

The documentation's headline example reads numbers from stdin, summing them
until it sees a `0` and bailing out on anything unparseable. stdin is awkward in
a notebook, so here it folds over a list of "typed lines" instead. The shape is
identical: `Modify` accumulates the total, `Jump` is the early return when we
hit `0`, and `Exception` reports a line that will not parse.

Each `run*`/handler nests a new scope, and its handle is only valid *inside*
that box. `throw ex` unwinds out to the `try` that owns `ex`; `jumpTo break`
exits just the loop. Nothing leaks outward:

```haskell
displayHtml $ concat
  [ "<svg viewBox='0 0 560 250' xmlns='http://www.w3.org/2000/svg' font-family='ui-sans-serif,system-ui,sans-serif'>"
  , "<rect x='6' y='6' width='548' height='238' rx='14' fill='#faf7f1' stroke='#2e3440' stroke-width='1.5'/>"
  , "<text x='20' y='28' font-size='13' font-weight='700' fill='#2e3440'>runPureEff</text>"
  , "<rect x='26' y='40' width='508' height='196' rx='12' fill='#ffffff' stroke='#bf616a' stroke-width='1.5'/>"
  , "<text x='40' y='62' font-size='13' font-weight='700' fill='#bf616a'>try  \\ex -&gt;</text>"
  , "<rect x='48' y='74' width='470' height='150' rx='10' fill='#faf7f1' stroke='#4c9a8f' stroke-width='1.5'/>"
  , "<text x='62' y='96' font-size='13' font-weight='700' fill='#3f7d74'>evalModify 0  \\total -&gt;</text>"
  , "<rect x='70' y='108' width='430' height='102' rx='8' fill='#ffffff' stroke='#d9a441' stroke-width='1.5'/>"
  , "<text x='84' y='130' font-size='13' font-weight='700' fill='#b8860b'>withJump  \\break -&gt;</text>"
  , "<text x='92' y='158' font-size='13' font-family='monospace' fill='#bf616a'>throw ex msg</text>"
  , "<text x='240' y='158' font-size='12' fill='#6f6a5d'>unwinds out to try</text>"
  , "<text x='92' y='180' font-size='13' font-family='monospace' fill='#b8860b'>jumpTo break</text>"
  , "<text x='240' y='180' font-size='12' fill='#6f6a5d'>exits just the loop</text>"
  , "<text x='92' y='202' font-size='13' font-family='monospace' fill='#3f7d74'>modify total (+ n)</text>"
  , "</svg>"
  ]
```

```haskell
sumUntilZero :: [String] -> Either String Int
sumUntilZero inputs = runPureEff $
  try $ \ex ->
    evalModify (0 :: Int) $ \total -> do
      withJump $ \break ->
        for_ inputs $ \line ->
          case readMaybe line of
            Nothing -> throw ex ("Couldn't read: " ++ line)
            Just 0 -> jumpTo break
            Just n -> modify total (+ n)
      get total
```

Summing until the `0` (everything after it is ignored):

```haskell
sumUntilZero ["1", "2", "3", "0", "99"]
```

No `0`, so every number counts:

```haskell
sumUntilZero ["4", "5", "6"]
```

An unparseable line throws, and `try` turns it into a `Left`:

```haskell
sumUntilZero ["10", "20", "oops", "5"]
```

We can watch it run. The strip below replays the fold step by step: each box is
one input, teal boxes accumulate the running total, and the amber box is where
`jumpTo break` ends it early (everything after the `0` is never visited).

```haskell
data Step = Add Int Int | Stop | Bad String

runTrace :: [String] -> [Step]
runTrace = go 0
  where
    go _ [] = []
    go acc (s : ss) = case readMaybe s :: Maybe Int of
      Nothing -> [Bad s]
      Just 0 -> [Stop]
      Just n -> Add n (acc + n) : go (acc + n) ss

traceSvg :: [String] -> String
traceSvg inputs =
  let steps = zip [0 :: Int ..] (runTrace inputs)
      box (i, st) =
        let x = 16 + i * 110
            (fill, stroke, top, bot) = case st of
              Add n t -> ("#e6f2ef", "#3f7d74", show n, "total " ++ show t)
              Stop -> ("#fbf0d8", "#b8860b", "0", "break")
              Bad s -> ("#f7e0e0", "#bf616a", s, "throws")
         in concat
              [ "<rect x='", show x, "' y='28' width='94' height='58' rx='9' fill='", fill, "' stroke='", stroke, "' stroke-width='1.5'/>"
              , "<text x='", show (x + 47), "' y='56' text-anchor='middle' font-size='17' font-family='monospace' fill='#2e3440'>", top, "</text>"
              , "<text x='", show (x + 47), "' y='76' text-anchor='middle' font-size='11' fill='", stroke, "'>", bot, "</text>"
              ]
      ww = 32 + length steps * 110
   in concat $
        ["<svg viewBox='0 0 ", show ww, " 104' xmlns='http://www.w3.org/2000/svg' font-family='ui-sans-serif,system-ui,sans-serif'>"]
          ++ map box steps
          ++ ["</svg>"]

displayHtml (traceSvg ["1", "2", "3", "0", "99"])
```

The same function over real input is just `runEff $ \io -> ...` with
`effIO io getLine` in place of the list, threading the same three handles.

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
        for_ is $ \i ->
          case compare i 0 of
            GT -> modify positives (+ 1)
            EQ -> throw ex ()
            LT -> modify negatives (+ 1)
        p <- get positives
        n <- get negatives
        pure ("Positives: " ++ show p ++ ", negatives: " ++ show n)
    case r of
      Right msg -> pure msg
      Left () -> do
        p <- get positives
        pure ("We saw a zero, but before it there were " ++ show p ++ " positives")
```

```haskell
countPositivesNegatives [1, -2, 3, -4, 5]
```

```haskell
countPositivesNegatives [1, 2, 0, 3, -1]
```

Read Bluefin top to bottom and the shape never changes: every effect arrives as
an explicit handle, scoped by the `run*` that introduced it, and ordinary
functions move those handles around. The full reference, with comparisons to
other effect systems, is in the
[`Bluefin` documentation](https://hackage.haskell.org/package/bluefin-0.6.0.0/docs/Bluefin.html).
