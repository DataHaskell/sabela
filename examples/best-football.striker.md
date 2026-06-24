```haskell
-- cabal: build-depends: dataframe ==2.1.0.3, dataframe-hasktorch ==0.2.0.1, hasktorch, text, random, granite
-- cabal: default-extensions: BangPatterns, NumericUnderscores, OverloadedStrings, ScopedTypeVariables, TemplateHaskell, TypeApplications
import qualified DataFrame as D

raw <- D.readCsv "./examples/data/top5_attackers.csv"
df = D.parseDefaults D.defaultParseOptions raw
D.dimensions df
```

Let's first have a look at the available data:

```haskell
import DataFrame.Operators

df |> D.take 5
   |> D.toMarkdown'
   |> displayMarkdown
```

# Who is the best striker? A z-score story

The viral version of this chart is a few lines of `pandas` + `numpy`: take every
attacker in the top-5 leagues (here the 2018-19 season), look at goals + assists
per 90 minutes, and measure how many standard deviations each player sits above
the average. The freaks of the distribution — the ones living out at 4σ, 5σ —
are the elite. Spoiler: Messi is alone out there.

We tell the same story in Haskell: `dataframe` for the data, **hasktorch**
tensors for the mean / standard deviation / z-scores, and
[`granite`](https://github.com/mchav/granite)'s grammar of graphics for the plot.

## Qualify the sample

Per-90 rates are meaningless for someone who played four minutes — one lucky
goal would read as an absurd rate. So we keep only players with a real sample:
at least ten full matches (`minutes_90s >= 10`, i.e. 900+ minutes), then pull
goals-plus-assists-per-90 out as a plain list of `Double`s.

```haskell
import qualified DataFrame.Functions as F
import Data.Text (Text)

qualified = df |> D.filter (F.col @Double "minutes_90s") (>= 10)

ga90 = D.columnAsList (F.col @Double "goals_assists_per90") qualified :: [Double]

(D.dimensions qualified, Prelude.take 5 ga90)
```

## The stats, in hasktorch

This is the part the image does with `numpy`. We move the column into a hasktorch
tensor and let Torch compute the mean and (unbiased) standard deviation. A
z-score is then just `(value - μ) / σ` — and because tensors broadcast, the whole
column standardises in a single elementwise expression, no loop required.

```haskell
import Torch

vals :: Tensor
vals = asTensor (map realToFrac ga90 :: [Float])

mu = asValue @Float (mean vals)
sd = asValue @Float (std vals)

zTensor = Torch.div (Torch.sub vals (mean vals)) (std vals)
zScores = asValue @[Float] zTensor :: [Float]

putStrLn ("average G+A/90 = " <> show mu <> "  |  std dev = " <> show sd)
```

## The outliers

We attach the z-scores back onto the frame and sort descending. These are the
players furthest out on the right tail — the strikers everyone argues about.

```haskell
withZ = qualified |> D.insert "z" (map realToFrac zScores :: [Double])

leaders = withZ |> D.sortBy [D.Desc (F.col @Double "z")] |> D.take 8

leaders |> D.select ["player", "team", "goals_assists_per90", "z"]
        |> D.toMarkdown'
        |> displayMarkdown
```

## The picture

Now the grammar of graphics. We bin the whole field into a histogram — the
right-skewed hump every attacker lives inside — and pick out a handful of
marquee names (whoever is in the data), laddered up the right edge at their
goals-plus-assists-per-90. The crowd clusters near the average; Messi stands
alone on the right at 5σ, exactly like the original. First the labels:

```haskell
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)

marquee = ["Messi", "Ronaldo", "Mbappé", "Neymar", "Lewandowski", "Salah"] :: [Text]
isMarquee p = Prelude.any (`T.isInfixOf` p) marquee

stars =
    withZ
        |> D.filter (F.col @(Maybe Text) "player") (maybe False isMarquee)
        |> D.sortBy [D.Desc (F.col @Double "z")]

topN = length (D.columnAsList (F.col @Double "z") stars :: [Double])

starName = map (fromMaybe "?") (D.columnAsList (F.col @(Maybe Text) "player") stars) :: [Text]
starGa90 = D.columnAsList (F.col @Double "goals_assists_per90") stars :: [Double]
starZ = D.columnAsList (F.col @Double "z") stars :: [Double]

starLabel =
    zipWith
        (\n z -> n <> " · " <> T.pack (show (fromIntegral (round (z * 10)) / 10 :: Double)) <> "σ")
        starName
        starZ

starLabel
```

The chart layers, from bottom to top: the histogram of the whole field, a marker
for each star, and the names. We compute the bins by hand and draw them as
columns, then overlay the marquee names as points and text.

```haskell
import Granite.Data.Frame
import Granite.Render.Pipeline
import Granite.Spec

nbins = 36 :: Int
lo = 0 :: Double
hi = Prelude.maximum ga90
binW = (hi - lo) / fromIntegral nbins
binOf x = Prelude.min (nbins - 1) (Prelude.floor ((x - lo) / binW))
counts = [length (filter ((== b) . binOf) ga90) | b <- [0 .. nbins - 1]]
centers = [lo + (fromIntegral b + 0.5) * binW | b <- [0 .. nbins - 1]] :: [Double]
maxC = fromIntegral (Prelude.maximum counts) :: Double

distFrame =
    fromColumns
        [ ("ga90", ColNum centers)
        , ("count", ColNum (map fromIntegral counts :: [Double]))
        ]

ladder = [maxC * (0.62 - 0.085 * fromIntegral i) | i <- [0 .. topN - 1]] :: [Double]

starsFrame =
    fromColumns
        [ ("ga90", ColNum starGa90)
        , ("y", ColNum ladder)
        , ("label", ColCat starLabel)
        ]

distLayer =
    (defLayer GeomCol)
        { layerMapping = emptyMapping {aesX = Just (ColumnRef "ga90"), aesY = Just (ColumnRef "count")}
        , layerAesDef = emptyAesDefaults {defFill = Just (Hex "#d9d9e3"), defColor = Just (Hex "#d9d9e3")}
        }

starPoints =
    (defLayer GeomPoint)
        { layerData = Just starsFrame
        , layerMapping = emptyMapping {aesX = Just (ColumnRef "ga90"), aesY = Just (ColumnRef "y")}
        , layerAesDef = emptyAesDefaults {defColor = Just (Hex "#ff2d92"), defSize = Just 8}
        }

starText =
    (defLayer GeomText)
        { layerData = Just starsFrame
        , layerMapping =
            emptyMapping
                { aesX = Just (ColumnRef "ga90")
                , aesY = Just (ColumnRef "y")
                , aesLabel = Just (ColumnRef "label")
                }
        , layerAesDef = emptyAesDefaults {defColor = Just (Hex "#1a1a1a"), defSize = Just 12}
        }

chart =
    emptyChart
        { chartData = distFrame
        , chartLayers = [distLayer, starPoints, starText]
        , chartTitle = Just "Goals + assists per 90 - every qualified attacker, top-5 leagues (2018-19)"
        , chartSize = SizePixels 900 560
        }

displayHtml (T.unpack (renderChartSvg chart))
```
