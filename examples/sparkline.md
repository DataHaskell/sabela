# Sabela: reactive notebooks for Haskell

Let's build a tiny **ASCII sparkline dashboard** that updates automatically when you change
inputs.

**Try this:** run all cells, then change `seed`, `width`, or `amplitude` and watch
the “chart” and summary stats update.

---

## 1) Controls (edit these)

```haskell
-- Change these, and everything below should update automatically.

seed :: Int
seed = 42

width :: Int
width = 64

amplitude :: Int
amplitude = 8

trend :: Int
trend = 1  -- try 0, 1, 2
```

## 2) Tiny deterministic “signal generator”

We'll generate a pseudo-random but deterministic series from `seed`. Then we add
a small trend so the picture changes in a pleasing way.

```haskell
-- A tiny deterministic RNG (linear congruential generator)
next :: Int -> Int
next x = (1103515245 * x + 12345) `mod` 2147483647

stream :: Int -> [Int]
stream s = let s' = next s in s' : stream s'

-- Convert RNG output into a centered "wiggle" in [-amplitude..amplitude]
wiggle :: Int -> Int -> Int
wiggle amp x =
  let r = x `mod` (2 * amp + 1)
  in r - amp

-- Our final signal: wiggle + a gentle trend
signal :: Int -> Int -> Int -> [Int]
signal s n amp =
  take n $
    zipWith (+) (map (wiggle amp) (stream s))
               (map (\i -> (i * trend) `div` 8) [0..])
```

---

## 3) A nice ASCII sparkline

This maps values onto a set of block characters.

```haskell
blocks :: [Char]
blocks = "▁▂▃▄▅▆▇█"

clamp :: Int -> Int -> Int -> Int
clamp lo hi x = max lo (min hi x)

spark :: [Int] -> String
spark xs =
  let lo = minimum xs
      hi = maximum xs
      -- map x in [lo..hi] -> index in [0..7]
      toIdx x =
        if hi == lo then 0
        else (7 * (x - lo)) `div` (hi - lo)
  in map (\x -> blocks !! clamp 0 7 (toIdx x)) xs
```

---

## 4) A tiny “dashboard”

We render the sparkline plus a few stats. This is where the reactivity feels
great: tweak controls above and this output updates.

```haskell
meanInt :: [Int] -> Int
meanInt xs = sum xs `div` max 1 (length xs)

rangeStr :: [Int] -> String
rangeStr xs = show (minimum xs) ++ " .. " ++ show (maximum xs)

bar :: Int -> Int -> String
bar w x =
  let k = clamp 0 w x
  in replicate k '█' ++ replicate (w - k) ' '

dashboard :: [Int] -> String
dashboard xs =
  let lo = minimum xs
      hi = maximum xs
      m  = meanInt xs
      -- show mean location as a bar
      pos =
        if hi == lo then 0
        else (32 * (m - lo)) `div` (hi - lo)
  in unlines
      [ "Sabela Spark Dashboard"
      , "──────────────────────"
      , spark xs
      , ""
      , "range : " ++ rangeStr xs
      , "mean  : " ++ show m
      , "mean@ : |" ++ bar 32 pos ++ "|"
      ]

let xs = signal seed width amplitude

putStrLn (dashboard xs)
```

---

Now go back to **Controls** and change just one value:

* `seed = 99` (new shape)
* `width = 120` (longer chart)
* `amplitude = 2` (flatter)
* `trend = 0` (no drift)

Sabela should automatically re-run the dependent cells and update the dashboard.
