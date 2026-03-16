# Interactive Widgets

Widgets are HTML controls — sliders, dropdowns, buttons, checkboxes, text inputs — that live inside a cell's output and trigger re-execution when the user interacts with them. No JavaScript on your part: Sabela generates the control, bridges the value back to Haskell, and re-runs the cell automatically.

## The Behavior type

Every widget is a `Behavior a` — a value that knows how to *render* itself and how to *sample* its current value:

```haskell
-- data Behavior a = Behavior { bSample :: IO a, bRender :: IO () }
-- instance Functor     Behavior
-- instance Applicative Behavior
```

The single verb `display` renders the control **and** returns the current value:

```haskell
-- display :: Behavior a -> IO a
```

## slider

```haskell
-- slider :: (Show a, Read a) => String -> a -> a -> a -> Behavior a
--           name               default  lo    hi
```

A range slider that re-runs the cell as the user drags (debounced to avoid flooding).

```haskell
c <- display (slider "celsius" (20 :: Int) (-40) 120)

let f = c * 9 `div` 5 + 32
    k = c + 273

displayHtml $ unlines
  [ "<p style='font-size:1.4em;margin:4px 0'><b>" ++ show c ++ " &#8451;</b></p>"
  , "<p style='color:#888;margin:4px 0'>" ++ show f ++ " &#8457; &nbsp; " ++ show k ++ " K</p>"
  ]
```

## dropdown

```haskell
-- dropdown :: String -> [String] -> String -> Behavior String
--             name     options     default
```

A select control that re-runs on each change.

```haskell
shape <- display (dropdown "shape" ["Circle", "Square", "Triangle"] "Circle")

let svg = case shape of
      "Circle"   -> "<circle cx='60' cy='60' r='50' fill='#3498db'/>"
      "Square"   -> "<rect x='10' y='10' width='100' height='100' rx='4' fill='#e74c3c'/>"
      _          -> "<polygon points='60,10 110,110 10,110' fill='#2ecc71'/>"

displayHtml $ "<svg width='120' height='120' xmlns='http://www.w3.org/2000/svg'>" ++ svg ++ "</svg>"
```

## button

```haskell
-- button :: String -> String -> Behavior (Maybe ())
--           label    name
-- Nothing = not clicked, Just () = clicked since last run
```

A button that re-runs on each click. `Nothing` on the first run (before clicking); `Just ()` after.

```haskell
clicked <- display (button "Roll dice" "go")

let result = case clicked of
      Nothing -> "Click the button to roll."
      Just () -> "You rolled: " ++ show (42 :: Int)

displayHtml $ "<p>" ++ result ++ "</p>"
```

## checkbox

```haskell
-- checkbox :: String -> Bool -> Behavior Bool
--             name     default
```

```haskell
verbose <- display (checkbox "verbose" False)
n <- display (slider "n" (1000 :: Int) 1 10000)

if verbose then displayMarkdown ("Computing sum from 1 to " ++ show n) else return ()

displayHtml $ "<p>Result: <b>" ++ show (sum [1..n]) ++ "</b></p>"
```

## textInput

```haskell
-- textInput :: String -> String -> Behavior String
--              name     default
```

```haskell
name <- display (textInput "name" "World")

displayHtml $ "<h2>Hello, " ++ name ++ "!</h2>"
```

## Combining with fmap and liftA2

`Behavior` is `Functor` and `Applicative`, so standard Prelude functions work directly.

**`fmap`** — derive a value from one widget without binding:

```haskell
f' <- display (fmap (\c -> c * 9 `div` 5 + 32) (slider "celsius" (20 :: Int) (-40) 120))

displayHtml $ "<p>" ++ show f' ++ " &#8457;</p>"
```

**`liftA2`** — combine two widgets:

```haskell
area <- display (liftA2 (*) (slider "width" (10 :: Int) 1 100) (slider "height" (10 :: Int) 1 100))

displayHtml $ "<p>Area: <b>" ++ show area ++ "</b></p>"
```

**`pure`** — a constant behavior that renders nothing:

```haskell
x <- display (pure (42 :: Int))

displayHtml $ "<p>Always: " ++ show x ++ "</p>"
```

## DataFrame example

```haskell
-- cabal: build-depends: dataframe, text
:set -XOverloadedStrings
import qualified DataFrame as D
import qualified Data.Text as T
import DataFrame ((|>))

v <- display (slider "rows" (10 :: Int) 1 20)

D.empty |> D.insert "x" [1..100]
        |> D.insert "y" [101..200]
        |> D.take v
        |> D.toMarkdownTable
        |> T.unpack
        |> displayMarkdown
```

## sample and render separately

For the rare case where you need to read a value without rendering, or render without reading:

```haskell
-- sample :: Behavior a -> IO a   -- read current value, no output
-- render :: Behavior a -> IO ()  -- render control, discard value
```

## Reactivity and re-execution

- **Slider** — re-runs as the user drags, debounced at 150 ms.
- **Dropdown / Button / Checkbox / TextInput** — re-runs on each discrete change.
- Widget state persists for the lifetime of the session. Pressing **Reset** clears all values back to their defaults.
- Only the cell that owns the widget re-executes. Other cells are unaffected unless they depend on definitions from this cell.

## Low-level API (still available)

The original imperative helpers remain for backward compatibility:

```haskell
-- widgetGet    :: String -> IO (Maybe String)
-- widgetRead   :: (Show a, Read a) => String -> a -> IO a
-- displaySlider :: String -> a -> a -> a -> IO ()
-- displaySelect :: String -> [String] -> String -> IO ()
-- displayButton :: String -> String -> IO ()
```
