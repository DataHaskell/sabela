# Interactive Widgets

Widgets are HTML controls — sliders, dropdowns, buttons — that live inside a cell's output and trigger re-execution when the user interacts with them. No JavaScript on your part: Sabela generates the control, bridges the value back to Haskell via a server-side store, and re-runs the cell automatically.

## How it works

Every widget has a **name** (a `String` key). When the user moves a slider or picks a dropdown option, the new value is posted to the server under that name. The cell then re-executes. Inside the cell, `widgetGet name` returns the current value as `Maybe String` — `Nothing` on the first run (before the user has touched the control), and `Just v` on every subsequent run.

The typical pattern is:



```haskell
-- cabal: build-depends: dataframe, text
:set -XOverloadedStrings
import qualified DataFrame as D
import qualified Data.Text as T

import DataFrame ((|>))

sliderName = "myWidget"
mValue <- widgetGet sliderName

defaultValue = 10
lo = 1
hi = 20

let v = maybe defaultValue read mValue

-- render the control with the current value so it stays in sync
displaySlider sliderName lo hi v

-- render output that depends on v
displayHtml $ "<p>You chose: " ++ show v ++ "</p>"


D.empty |> D.insert "x" [1..100]
        |> D.insert "y" [101..200]
        |> D.take v
        |> D.toMarkdownTable
        |> T.unpack
        |> displayMarkdown
```

> <!-- sabela:mime text/plain -->
> ---MIME:text/html---
> <input type='range' min='1' max='20' value='5' oninput="parent.postMessage({type:'widget',cellId:14,name:'myWidget',value:this.value},'*')">
> ---MIME:text/html---
> <p>You chose: 5</p>
> ---MIME:text/markdown---
> | x<br>Integer | y<br>Integer |
> | -------------|------------- |
> | 1            | 101          |
> | 2            | 102          |
> | 3            | 103          |
> | 4            | 104          |
> | 5            | 105          |


Passing `v` back to `displaySlider` keeps the slider handle at the right position after each re-render.

## widgetGet




```haskell
-- widgetGet :: String -> IO (Maybe String)
```




Returns `Nothing` until the user has interacted with the named widget. Always provide a sensible default with `maybe`:




```haskell
mX <- widgetGet "x"
let x = maybe 0 read mX :: Int
```




Widget values are stored per-cell, per-name. Two cells can use the same name without interfering.

## displaySlider




```haskell
-- displaySlider :: String -> Int -> Int -> Int -> IO ()
--                   name     min   max   current
```




Renders a range slider. The cell re-runs on every change while the user drags (debounced to avoid flooding the server).




```haskell
mTemp <- widgetGet "celsius"
let c = maybe 20 read mTemp :: Int
    f = c * 9 `div` 5 + 32
    k = c + 273

displaySlider "celsius" (-40) 120 c

displayHtml $ unlines
  [ "<p style='font-size:1.4em;margin:4px 0'><b>" ++ show c ++ " &#8451;</b></p>"
  , "<p style='color:#888;margin:4px 0'>" ++ show f ++ " &#8457; &nbsp; " ++ show k ++ " K</p>"
  ]
```

> <!-- sabela:mime text/plain -->
> ---MIME:text/html---
> <input type='range' min='-40' max='120' value='-18' oninput="parent.postMessage({type:'widget',cellId:22,name:'celsius',value:this.value},'*')">
> ---MIME:text/html---
> <p style='font-size:1.4em;margin:4px 0'><b>-18 &#8451;</b></p>
> <p style='color:#888;margin:4px 0'>-1 &#8457; &nbsp; 255 K</p>




## displaySelect




```haskell
-- displaySelect :: String -> [String] -> String -> IO ()
--                  name     options     current
```




Renders a dropdown. The cell re-runs when the selection changes.




```haskell
mShape <- widgetGet "shape"
let shape = maybe "Circle" id mShape
    svg = case shape of
      "Circle"   -> "<circle cx='60' cy='60' r='50' fill='#3498db'/>"
      "Square"   -> "<rect x='10' y='10' width='100' height='100' rx='4' fill='#e74c3c'/>"
      _          -> "<polygon points='60,10 110,110 10,110' fill='#2ecc71'/>"

displaySelect "shape" ["Circle", "Square", "Triangle"] shape

displayHtml $ "<svg width='120' height='120' xmlns='http://www.w3.org/2000/svg'>" ++ svg ++ "</svg>"
```

> <!-- sabela:mime text/plain -->
> ---MIME:text/html---
> <select onchange="parent.postMessage({type:'widget',cellId:26,name:'shape',value:this.value},'*')"><option selected>Circle</option><option>Square</option><option>Triangle</option></select>
> ---MIME:text/html---
> <svg width='120' height='120' xmlns='http://www.w3.org/2000/svg'><circle cx='60' cy='60' r='50' fill='#3498db'/></svg>




## displayButton




```haskell
-- displayButton :: String -> String -> IO ()
--                  label    name
```




Renders a button. The cell re-runs each time the button is clicked. The value stored under `name` is always `"clicked"` — you can use `widgetGet` to detect whether the button has been clicked at least once.




```haskell
mClicked <- widgetGet "go"

displayButton "Roll dice" "go"

let result = case mClicked of
      Nothing -> "Click the button to roll."
      Just _  -> "You rolled: " ++ show (42 :: Int)  -- replace with real RNG

displayHtml $ "<p>" ++ result ++ "</p>"
```

> <!-- sabela:mime text/plain -->
> ---MIME:text/html---
> <button onclick="parent.postMessage({type:'widget',cellId:30,name:'go',value:'clicked'},'*')">Roll dice</button>
> ---MIME:text/html---
> <p>You rolled: 42</p>




Buttons are most useful alongside a slider: the slider sets a parameter, the button triggers the (potentially slow) computation explicitly.

## Combining widgets

Multiple widgets can appear in the same cell. Each has its own name and value.




```haskell
mLimit <- widgetGet "limit"
mClicked' <- widgetGet "go"

let n = maybe 50 read mLimit :: Int
    sieve []     = []
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
    ps = sieve [2..n]

displaySlider "limit" 2 500 n

displayButton "Compute primes" "go"

displayHtml $ unlines
  [ "<p><b>" ++ show (length ps) ++ " primes &le; " ++ show n ++ "</b></p>"
  , "<p style='color:#888;word-break:break-all'>" ++ unwords (map show ps) ++ "</p>"
  ]
```

> <!-- sabela:mime text/plain -->
> ---MIME:text/html---
> <input type='range' min='2' max='500' value='77' oninput="parent.postMessage({type:'widget',cellId:32,name:'limit',value:this.value},'*')">
> ---MIME:text/html---
> <button onclick="parent.postMessage({type:'widget',cellId:32,name:'go',value:'clicked'},'*')">Compute primes</button>
> ---MIME:text/html---
> <p><b>21 primes &le; 77</b></p>
> <p style='color:#888;word-break:break-all'>2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73</p>




## Custom HTML widgets

`displaySlider`, `displaySelect`, and `displayButton` are convenience wrappers around plain `displayHtml`. Any HTML element can act as a widget by posting a message to the parent frame:




```haskell
-- parent.postMessage({ type: 'widget', cellId: <id>, name: '<name>', value: <value> }, '*')
```




The cell ID is available in Haskell via `readIORef _sabelaCellIdRef`. Here is an example of a colour picker built from a plain `<input type="color">`:




```haskell
cid <- readIORef _sabelaCellIdRef

mColour <- widgetGet "colour"
let colour = maybe "#3498db" id mColour

displayHtml $ unlines
  [ "<input type='color' value='" ++ colour ++ "'"
  , "  oninput=\"parent.postMessage({type:'widget',cellId:" ++ cid ++ ",name:'colour',value:this.value},'*')\">"
  , "<p>Chosen: <span style='color:" ++ colour ++ "'><b>" ++ colour ++ "</b></span></p>"
  ]
```

> <!-- sabela:mime text/html -->
> <input type='color' value='#db3376'
>   oninput="parent.postMessage({type:'widget',cellId:36,name:'colour',value:this.value},'*')">
> <p>Chosen: <span style='color:#db3376'><b>#db3376</b></span></p>




The same technique works for `<input type="range">` (what `displaySlider` uses internally), checkboxes, text inputs, or any other HTML form control.

## Reactivity and re-execution

- **Slider** — re-runs the cell as the user drags, debounced at 150 ms. If the user drags quickly only the final resting value triggers a run.
- **Select / Button** — re-runs on each discrete change or click.
- Widget state persists for the lifetime of the session. Reloading the notebook or pressing **Reset** clears all values back to `Nothing`.
- Only the cell that owns the widget re-executes. Other cells are unaffected unless they depend on definitions from this cell.
