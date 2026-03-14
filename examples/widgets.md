# Interactive Widgets

Sabela widgets are HTML controls (sliders, dropdowns, buttons) that post their value back to Haskell whenever they change, re-executing the cell with the new state. No JavaScript required on your part — the display helpers generate the controls and the bridge is built in.

There are three widget functions available in every cell:

- `displaySlider name lo hi val` — a range slider
- `displaySelect name options currentVal` — a dropdown
- `displayButton label name` — a button that re-runs the cell when clicked

`widgetGet name` retrieves the current server-side value for a named widget (returns `Maybe String`).

## Slider — Temperature Converter

Drag the slider to convert Celsius to Fahrenheit and Kelvin in real time.

```haskell
mCelsius <- widgetGet "celsius"

let c = maybe 20 read mCelsius :: Int
    f = c * 9 `div` 5 + 32
    k = c + 273
    html = unlines
      [ "<div style='font-family:sans-serif;padding:12px;line-height:2'>"
      , "  <p style='font-size:2em;margin:0'><b>" ++ show c ++ " &#8451;</b></p>"
      , "  <p style='margin:0;color:#555'>" ++ show f ++ " &#8457;</p>"
      , "  <p style='margin:0;color:#555'>" ++ show k ++ " K</p>"
      , "</div>"
      ]

displaySlider "celsius" (-40) 120 c

displayHtml html
```

## Dropdown — Shape Viewer

Pick a shape from the dropdown to see it rendered as SVG.

```haskell
mShape <- widgetGet "shape"

let shape = maybe "Circle" id mShape
    svg = case shape of
      "Circle"   -> "<circle cx='70' cy='70' r='55' fill='#3498db'/>"
      "Square"   -> "<rect x='15' y='15' width='110' height='110' rx='6' fill='#e74c3c'/>"
      "Triangle" -> "<polygon points='70,12 128,128 12,128' fill='#2ecc71'/>"
      _          -> "<polygon points='70,5 82,45 125,45 90,68 103,108 70,83 37,108 50,68 15,45 58,45' fill='#f39c12'/>"
    html = "<svg width='140' height='140' xmlns='http://www.w3.org/2000/svg'>" ++ svg ++ "</svg>"

displaySelect "shape" ["Circle", "Square", "Triangle", "Star"] shape

displayHtml html
```

## Button + Slider — Prime Sieve

The slider sets the upper bound; the button re-runs the cell on demand. Both controls trigger re-execution — the button is useful when you want explicit control rather than running on every slider tick.

```haskell
mLimit <- widgetGet "limit"

let n = maybe 50 read mLimit :: Int
    sieve []     = []
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
    ps = sieve [2..n]
    html = unlines
      [ "<div style='font-family:monospace;padding:8px'>"
      , "  <p><b>" ++ show (length ps) ++ " primes &le; " ++ show n ++ "</b></p>"
      , "  <p style='color:#555;word-break:break-all'>" ++ unwords (map show ps) ++ "</p>"
      , "</div>"
      ]

displaySlider "limit" 2 500 n

displayButton "Compute primes" "go"

displayHtml html
```
