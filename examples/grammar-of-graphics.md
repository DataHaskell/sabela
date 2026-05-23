# Plotting with the grammar of graphics

`granite` recently grew a second, richer API. The functions in
`Granite.Svg` (`bars`, `pie`, `lineGraph`, ...) are still there and still
handy for one-liners, but the new `Granite.Spec` interface describes a
chart the way *ggplot2* and *Vega* do — as **data + layers + scales +
coordinates + facets**. That little bit of extra structure buys a lot:
you can stack several geometries on the same axes, fit a regression
line, split a plot into small multiples, or bend the coordinate system
into polar space, all without leaving Haskell.

A `Chart` is built from a few pieces:

```
Chart = data + layers + scales + coord + facet + theme + size
```

- **data** — a `DataFrame` of named columns (`ColNum` for numbers,
  `ColCat` for categories)
- **layers** — one or more `Layer`s, each pairing a `Geom`
  (`GeomPoint`, `GeomLine`, `GeomBar`, …) with an aesthetic mapping
  that says which column drives `aesX`, `aesY`, `aesFill`, and so on
- **stat** — an optional transform applied before drawing
  (`StatBin`, `StatDensity`, `StatSmooth`, `StatBoxplot`)
- **scales / coord / facet** — how data maps to the screen

The pipeline turns that spec into a backend-agnostic scene, then
`renderChartSvg` emits the SVG we hand to `displaySvg`. (Its sibling,
`renderChartTerminal`, draws the same chart in braille characters if you
ever want it in a plain terminal.)

```haskell
-- cabal: build-depends: text, granite
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Granite.Color (Color (..))
import Granite.Data.Frame
import Granite.Render.Pipeline (renderChartSvg)
import Granite.Spec

-- A tiny helper so each cell below ends with one tidy call.
draw chart = displaySvg (T.unpack (renderChartSvg chart))
```

## Scatter with a best-fit line

The headline feature is **layering**. Here two layers share one data
frame: raw points on the bottom, and on top a line whose
`StatSmooth SmoothLm` runs an ordinary-least-squares regression and
draws the fit. Swap in `SmoothLoess` or `SmoothMovingAvg` for a
different smoother.

```haskell
:set -XOverloadedStrings

scatterFit =
    let xs    = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9] :: [Double]
        noise = [0.3, -0.5, 0.7, -0.2, 0.4, -0.6, 0.1, 0.3, -0.4, 0.5]
        ys    = [2 * x + 1 + n | (x, n) <- zip xs noise]
        df    = fromColumns [("x", ColNum xs), ("y", ColNum ys)]
        m     = emptyMapping
                    { aesX = Just (ColumnRef "x")
                    , aesY = Just (ColumnRef "y")
                    }
        points = (defLayer GeomPoint) {layerMapping = m}
        fit =
            (defLayer GeomLine)
                { layerMapping = m
                , layerStat = StatSmooth SmoothLm
                , layerAesDef =
                    emptyAesDefaults
                        { defColor = Just (NamedColor BrightRed)
                        , defLineWidth = Just 2
                        }
                }
     in emptyChart
            { chartData = df
            , chartLayers = [points, fit]
            , chartTitle = Just "Measured vs. fitted"
            , chartSize = SizeChars 60 18
            }

draw scatterFit
```

## Grouped bars

Long-format data — one row per *quarter × product* — plus
`layerPosition = PosDodge` places the three products side-by-side
within each quarter. Switch `PosDodge` to `PosStack` and the same data
stacks instead.

```haskell
groupedBars =
    let quarters = concat [replicate 3 q | q <- ["Q1", "Q2", "Q3", "Q4"]]
        products = take 12 (cycle ["Widgets", "Gadgets", "Gizmos"])
        sales    = [12, 8, 4, 15, 10, 6, 18, 12, 8, 22, 14, 10] :: [Double]
        df =
            fromColumns
                [ ("quarter", ColCat quarters)
                , ("product", ColCat products)
                , ("sales", ColNum sales)
                ]
        layer =
            (defLayer GeomBar)
                { layerMapping =
                    emptyMapping
                        { aesX = Just (ColumnRef "quarter")
                        , aesY = Just (ColumnRef "sales")
                        , aesGroup = Just (ColumnRef "product")
                        , aesFill = Just (ColumnRef "product")
                        }
                , layerStat = StatIdentity
                , layerPosition = PosDodge 0.25
                }
     in emptyChart
            { chartData = df
            , chartLayers = [layer]
            , chartTitle = Just "Unit sales by quarter"
            , chartSize = SizeChars 64 18
            }

draw groupedBars
```

## Small multiples (faceting)

`FacetWrap` splits the data into a grid of panels by a categorical
column — the quickest way to compare a shape across groups without
cramming every series onto one set of axes.

```haskell
faceted =
    let df =
            fromColumns
                [ ("x", ColNum [0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3])
                , ("y", ColNum [1, 4, 9, 16, 0, 2, 4, 6, 5, 4, 3, 2])
                , ("series", ColCat (replicate 4 "A" <> replicate 4 "B" <> replicate 4 "C"))
                ]
        layer =
            (defLayer GeomLine)
                { layerMapping =
                    emptyMapping
                        { aesX = Just (ColumnRef "x")
                        , aesY = Just (ColumnRef "y")
                        }
                }
     in emptyChart
            { chartData = df
            , chartLayers = [layer]
            , chartFacet = FacetWrap (ColumnRef "series") (Just 3) Nothing ScalesFixed
            , chartTitle = Just "One panel per series"
            , chartSize = SizeChars 72 18
            }

draw faceted
```

## A line with a confidence band

Two layers again, but order matters: the translucent `GeomRibbon`
(reading `aesYmin`/`aesYmax`) is drawn first so the solid `GeomLine`
lands on top of it. `defAlpha` makes the band see-through.

```haskell
forecast =
    let xs  = [0, 0.5 .. 6.0] :: [Double]
        ys  = map sin xs
        los = map (\x -> sin x - 0.3) xs
        his = map (\x -> sin x + 0.3) xs
        df =
            fromColumns
                [ ("x", ColNum xs)
                , ("y", ColNum ys)
                , ("lo", ColNum los)
                , ("hi", ColNum his)
                ]
        ribbon =
            (defLayer GeomRibbon)
                { layerMapping =
                    emptyMapping
                        { aesX = Just (ColumnRef "x")
                        , aesYmin = Just (ColumnRef "lo")
                        , aesYmax = Just (ColumnRef "hi")
                        }
                , layerAesDef =
                    emptyAesDefaults
                        { defColor = Just (NamedColor BrightCyan)
                        , defAlpha = Just 0.3
                        }
                }
        line =
            (defLayer GeomLine)
                { layerMapping =
                    emptyMapping
                        { aesX = Just (ColumnRef "x")
                        , aesY = Just (ColumnRef "y")
                        }
                , layerAesDef =
                    emptyAesDefaults
                        { defColor = Just (NamedColor BrightBlue)
                        , defLineWidth = Just 2
                        }
                }
     in emptyChart
            { chartData = df
            , chartLayers = [ribbon, line]
            , chartTitle = Just "Estimate +/- 0.3 band"
            , chartSize = SizeChars 60 16
            }

draw forecast
```

## Annotated heatmap

A `GeomTile` shades each cell along a cold→hot gradient when `aesFill`
maps to a numeric column; a second `GeomText` layer prints the value on
top. This is the recipe for a labelled correlation matrix.

```haskell
heatmap =
    let coords =
            [ (fromIntegral x, fromIntegral y, sin (fromIntegral x / 2) + cos (fromIntegral y / 2))
            | x <- [0 .. 4 :: Int]
            , y <- [0 .. 4 :: Int]
            ]
        labels = [T.pack (show (round (z * 10) :: Int)) | (_, _, z) <- coords]
        df =
            fromColumns
                [ ("x", ColNum [x | (x, _, _) <- coords])
                , ("y", ColNum [y | (_, y, _) <- coords])
                , ("z", ColNum [z | (_, _, z) <- coords])
                , ("label", ColCat labels)
                ]
        tile =
            (defLayer GeomTile)
                { layerMapping =
                    emptyMapping
                        { aesX = Just (ColumnRef "x")
                        , aesY = Just (ColumnRef "y")
                        , aesFill = Just (ColumnRef "z")
                        }
                , layerStat = StatIdentity
                }
        label =
            (defLayer GeomText)
                { layerMapping =
                    emptyMapping
                        { aesX = Just (ColumnRef "x")
                        , aesY = Just (ColumnRef "y")
                        , aesLabel = Just (ColumnRef "label")
                        }
                , layerStat = StatIdentity
                }
     in emptyChart
            { chartData = df
            , chartLayers = [tile, label]
            , chartTitle = Just "Annotated heatmap"
            , chartSize = SizeChars 48 18
            }

draw heatmap
```

## Bending the axes: a polar rose

`CoordPolar` projects one aesthetic onto an angle and the other onto a
radius. Feed it `r = |sin(3θ)|` and you get a three-petalled rose — the
same `GeomLine` layer you'd use on Cartesian axes, just reinterpreted.

```haskell
rose =
    let n = 64 :: Int
        pts =
            [ (theta, abs (sin (3 * theta)))
            | i <- [0 .. n]
            , let theta = (fromIntegral i / fromIntegral n) * 2 * pi
            ]
        df =
            fromColumns
                [ ("theta", ColNum [t | (t, _) <- pts])
                , ("r", ColNum [r | (_, r) <- pts])
                ]
        layer =
            (defLayer GeomLine)
                { layerMapping =
                    emptyMapping
                        { aesX = Just (ColumnRef "theta")
                        , aesY = Just (ColumnRef "r")
                        }
                }
     in emptyChart
            { chartData = df
            , chartLayers = [layer]
            , chartCoord = CoordPolar ThetaX 0 PolarCCW
            , chartTitle = Just "r = |sin(3 theta)|"
            , chartSize = SizeChars 44 22
            }

draw rose
```

## Log-scale axes

Exponential growth flattens into a straight line once you set
`scaleY = SLog Base10`. The default scale options produce "nice"
integer-power ticks (1, 10, 100, …).

```haskell
logScatter =
    let xs = [1 .. 6] :: [Double]
        ys = [3, 30, 80, 200, 700, 2100] :: [Double]
        df = fromColumns [("x", ColNum xs), ("y", ColNum ys)]
        layer =
            (defLayer GeomPoint)
                { layerMapping =
                    emptyMapping
                        { aesX = Just (ColumnRef "x")
                        , aesY = Just (ColumnRef "y")
                        }
                }
     in emptyChart
            { chartData = df
            , chartLayers = [layer]
            , chartScales = defScales {scaleY = SLog Base10 defScaleOpts}
            , chartTitle = Just "Growth on a log axis"
            , chartSize = SizeChars 56 16
            }

draw logScatter
```

## Distribution = histogram + density

Stat transforms shine here. `StatBin` buckets the raw sample into
counts; `StatDensity` runs a Gaussian kernel density estimate. Layer
them and you get the classic "distplot".

```haskell
distribution =
    let sample =
            [ 1.0, 1.2, 1.3, 1.5, 1.7, 2.0, 2.1, 2.3
            , 2.5, 2.8, 3.0, 3.1, 3.3, 3.7, 3.9, 4.0
            ]
        df = fromColumns [("x", ColNum sample)]
        hist =
            (defLayer GeomHistogram)
                { layerMapping =
                    emptyMapping
                        { aesX = Just (ColumnRef "x")
                        , aesY = Just (ColumnRef "count")
                        }
                , layerStat = StatBin (BinByCount 8)
                }
        kde =
            (defLayer GeomDensity)
                { layerMapping =
                    emptyMapping
                        { aesX = Just (ColumnRef "x")
                        , aesY = Just (ColumnRef "density")
                        }
                , layerStat = StatDensity
                }
     in emptyChart
            { chartData = df
            , chartLayers = [hist, kde]
            , chartTitle = Just "Sample distribution"
            , chartSize = SizeChars 60 16
            }

draw distribution
```

## Where to go next

Everything here is just records, so the natural next move is to factor
out the parts you reuse — a `themed` chart, a `withTitle` helper, a
function that turns a `DataFrame` column pair into a layer. Because a
`Chart` is plain data you can build it up programmatically, map over a
list of datasets to produce a gallery, or pattern-match a widget value
to switch geoms live. The full catalogue of geoms, stats, and
coordinate systems lives in `granite`'s own `docs/tutorial.md`.
