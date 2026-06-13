# California Housing: From Exploration to Linear Regression

This notebook walks the full arc on the classic California housing dataset: we
load it, take stock of its structure and distributions, engineer a handful of
features that track house prices better than the raw columns, confirm those
features correlate with the target, and finally train a linear-regression model
with **Hasktorch** to predict the median house value of a district.

The exploration follows chapter 2 of *Hands-on Machine Learning with
Scikit-Learn, Keras & TensorFlow* (the prose is heavily derived from that book
and included here purely for didactic purposes). The modeling section is a port
of the [`dataframe` CaliforniaHousing example](https://github.com/mchav/dataframe/blob/main/examples/CaliforniaHousing.hs).

## Setup

We pull in `dataframe` for the data work, its plotting and Hasktorch bridges for
visualisation and tensors, and `Torch` itself for the model. Everything the rest
of the notebook needs is imported once here.

```haskell
-- cabal: build-depends: dataframe ==2.1.0.3, dataframe-hasktorch ==0.2.0.1, hasktorch, text, random, granite
-- cabal: default-extensions: BangPatterns, NumericUnderscores, OverloadedStrings, ScopedTypeVariables, TemplateHaskell, TypeApplications

import Control.Monad (when)
import Data.Function (on)
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text (Text)

import qualified DataFrame as D
import qualified DataFrame.Functions as F
import DataFrame.Operators
import DataFrame.Hasktorch (toTensor)
import qualified DataFrame.Display.Web.Plot as Plt

import System.Random (mkStdGen)
import Torch
```

## Loading the data

The dataset lives alongside the notebook. We read it into a dataframe with
`readCsv`.

```haskell
df <- D.readCsv "./examples/data/housing.csv"
```

## A first look at the data

Let's take a look at the top five rows using the dataframe's `take` function.

```haskell
df |> D.take 5
   |> D.toMarkdown'
   |> displayMarkdown
```

> <!-- scripths:mime text/markdown -->
> | longitude<br>Double | latitude<br>Double | housing_median_age<br>Double | total_rooms<br>Double | total_bedrooms<br>Maybe Double | population<br>Double | households<br>Double | median_income<br>Double | median_house_value<br>Double | ocean_proximity<br>Text |
> | --------------------|--------------------|------------------------------|-----------------------|--------------------------------|----------------------|----------------------|-------------------------|------------------------------|------------------------ |
> | -122.23             | 37.88              | 41.0                         | 880.0                 | Just 129.0                     | 322.0                | 126.0                | 8.3252                  | 452600.0                     | NEAR BAY                |
> | -122.22             | 37.86              | 21.0                         | 7099.0                | Just 1106.0                    | 2401.0               | 1138.0               | 8.3014                  | 358500.0                     | NEAR BAY                |
> | -122.24             | 37.85              | 52.0                         | 1467.0                | Just 190.0                     | 496.0                | 177.0                | 7.2574                  | 352100.0                     | NEAR BAY                |
> | -122.25             | 37.85              | 52.0                         | 1274.0                | Just 235.0                     | 558.0                | 219.0                | 5.6431000000000004      | 341300.0                     | NEAR BAY                |
> | -122.25             | 37.85              | 52.0                         | 1627.0                | Just 280.0                     | 565.0                | 259.0                | 3.8462                  | 342200.0                     | NEAR BAY                |

Each row represents one district. There are 10 attributes: `longitude`,
`latitude`, `housing_median_age`, `total_rooms`, `total_bedrooms`, `population`,
`households`, `median_income`, `median_house_value`, and `ocean_proximity`.

We can already tell from the types that `total_bedrooms` will have null values,
since its type is `Maybe Double`.

## Column summaries

How many nulls does `total_bedrooms` actually have? We can get a per-column
summary — including null counts — using `describeColumns`.

```haskell
df |> D.describeColumns
   |> D.toMarkdown'
   |> displayMarkdown
```

> <!-- scripths:mime text/markdown -->
> | Column Name<br>Text | # Non-null Values<br>Int | # Null Values<br>Int | Type<br>Text |
> | --------------------|--------------------------|----------------------|------------- |
> | total_bedrooms      | 20433                    | 207                  | Maybe Double |
> | ocean_proximity     | 20640                    | 0                    | Text         |
> | median_house_value  | 20640                    | 0                    | Double       |
> | median_income       | 20640                    | 0                    | Double       |
> | households          | 20640                    | 0                    | Double       |
> | population          | 20640                    | 0                    | Double       |
> | total_rooms         | 20640                    | 0                    | Double       |
> | housing_median_age  | 20640                    | 0                    | Double       |
> | latitude            | 20640                    | 0                    | Double       |
> | longitude           | 20640                    | 0                    | Double       |

Let's look at the other fields. The `summarize` function shows a summary of the
numerical attributes.

```haskell
df |> D.summarize
   |> D.toMarkdown'
   |> displayMarkdown
```

> <!-- scripths:mime text/markdown -->
> | Statistic<br>Text | longitude<br>Double | latitude<br>Double | housing_median_age<br>Double | total_rooms<br>Double | total_bedrooms<br>Double | population<br>Double | households<br>Double | median_income<br>Double | median_house_value<br>Double |
> | ------------------|---------------------|--------------------|------------------------------|-----------------------|--------------------------|----------------------|----------------------|-------------------------|----------------------------- |
> | Count             | 20640.0             | 20640.0            | 20640.0                      | 20640.0               | 20433.0                  | 20640.0              | 20640.0              | 20640.0                 | 20640.0                      |
> | Mean              | -119.57             | 35.63              | 28.64                        | 2635.76               | 537.87                   | 1425.48              | 499.54               | 3.87                    | 206855.82                    |
> | Minimum           | -124.35             | 32.54              | 1.0                          | 2.0                   | 1.0                      | 3.0                  | 1.0                  | 0.5                     | 14999.0                      |
> | 25%               | -121.8              | 33.93              | 18.0                         | 1447.75               | 296.0                    | 787.0                | 280.0                | 2.56                    | 119600.0                     |
> | Median            | -118.49             | 34.26              | 29.0                         | 2127.0                | 435.0                    | 1166.0               | 409.0                | 3.53                    | 179700.0                     |
> | 75%               | -118.01             | 37.71              | 37.0                         | 3148.0                | 647.0                    | 1725.0               | 605.0                | 4.74                    | 264725.0                     |
> | Max               | -114.31             | 41.95              | 52.0                         | 39320.0               | 6445.0                   | 35682.0              | 6082.0               | 15.0                    | 500001.0                     |
> | StdDev            | 2.0                 | 2.14               | 12.59                        | 2181.62               | 421.39                   | 1132.46              | 382.33               | 1.9                     | 115395.62                    |
> | IQR               | 3.79                | 3.78               | 19.0                         | 1700.25               | 351.0                    | 938.0                | 325.0                | 2.18                    | 145125.0                     |
> | Skewness          | -0.3                | 0.47               | 6.0e-2                       | 4.15                  | 3.46                     | 4.94                 | 3.41                 | 1.65                    | 0.98                         |

We can also get summaries of categorical columns. `ocean_proximity` is text, so
we count how often each value occurs with `frequencies`.

```haskell
df |> D.frequencies (D.col @Text "ocean_proximity")
   |> D.toMarkdown'
   |> displayMarkdown
```

> <!-- scripths:mime text/markdown -->
> | Statistic<br>Text | <1H OCEAN<br>Any | INLAND<br>Any | ISLAND<br>Any | NEAR BAY<br>Any | NEAR OCEAN<br>Any |
> | ------------------|------------------|---------------|---------------|-----------------|------------------ |
> | Count             | 9136             | 6551          | 5             | 2290            | 2658              |
> | Percentage (%)    | 44.26%           | 31.74%        | 0.02%         | 11.09%          | 12.88%            |

The count, mean, minimum, and max rows are self-explanatory. The StdDev row
shows the standard deviation, which measures how dispersed the values are. The
25%, 50%, and 75% rows show the corresponding percentiles: a percentile
indicates the value below which a given percentage of observations falls.

For example, 25% of the districts have a `housing_median_age` lower than 18,
while 50% are lower than 29 and 75% are lower than 37. These are often called
the 25th percentile (or 1st quartile), the median, and the 75th percentile (or
3rd quartile).

## Distributions

Another quick way to get a feel for the data is to plot a histogram for each
numerical attribute.

```haskell
-- `selectBy` lets us filter columns by name, index range, property
-- (isNumeric, isOptional, ...), or name range. Here we keep only the numeric
-- columns. (allHistograms already skips non-numeric columns itself; we
-- filter explicitly to show the API.)
Plt.HtmlPlot p <- Plt.allHistograms (df |> D.selectBy [D.byProperty D.isNumeric])

displayHtml (T.unpack p)
```

## Typed column references

Before preparing the data for a model, it's worth trying out various attribute
combinations. The total number of rooms in a district is not very useful if you
don't know how many households there are — what you really want is the number of
rooms *per household*. Similarly the total number of bedrooms is more telling
relative to the number of rooms, and the population per household is an
interesting combination too.

To reference existing attributes in type-safe expressions, we generate global
references to each column with `declareColumns`. This reads the loaded dataframe
and creates one typed handle per column (`total_rooms`, `households`,
`median_house_value`, and so on), so the compiler catches typos and type
mismatches in the expressions that follow.

```haskell
$(D.declareColumns df)
```

## Spatial structure

Let's bucket districts by price and plot them by location. We use `F.name` to
turn a typed column reference back into its text name, which keeps us from
mistyping column names in the plotting call.

```haskell
priceBucket p
    | p > 500000 = (">500'000" :: Text)
    | p > 450000 = ">450'000"
    | p > 400000 = ">400'000"
    | p > 300000 = ">300'000"
    | p > 200000 = ">200'000"
    | otherwise  = "<=200'000"

withPriceBuckets = df
                 |> D.derive "price_bucket" (F.lift priceBucket median_house_value)
                 |> D.sortBy [D.Desc median_house_value]

Plt.HtmlPlot scatter <- Plt.scatter ((Plt.mkScatter (F.name longitude) (F.name latitude)){ Plt.color = Just "price_bucket" }) withPriceBuckets

displayHtml (T.unpack scatter)
```

### Rebuilding it with granite

The scatter above is drawn with Chart.js. Below we build the same
longitude/latitude view with [granite](https://github.com/mchav/granite)'s
grammar of graphics, one step at a time: prepare the data, then layer on the
cities and the price emphasis.

```haskell
-- Spatial structure with granite. Step 1: pull longitude, latitude, the price
-- bucket and the raw median value into a granite frame (one row per district).
import Granite.Data.Frame (Column(ColNum, ColCat), fromColumns, frameLength, columnNames)
import Granite.Render.Pipeline (renderChartSvg)
import Granite.Spec

housingFrame = fromColumns [("lng", ColNum (D.columnAsList longitude withPriceBuckets)), ("lat", ColNum (D.columnAsList latitude withPriceBuckets)), ("bucket", ColCat (D.columnAsList (F.col @Text "price_bucket") withPriceBuckets)), ("value", ColNum (D.columnAsList median_house_value withPriceBuckets))]

(frameLength housingFrame, columnNames housingFrame)
```

That is a granite frame of all 20,640 districts with their coordinates and
price bucket. Next, the major population centres we want to label:

```haskell
-- Step 2: the major California population centres, as a small granite frame.
cityName = ["Los Angeles", "San Francisco", "San Diego", "San Jose", "Sacramento", "Fresno", "Long Beach", "Bakersfield"] :: [Text]
cityLng = [-118.24, -122.42, -117.16, -121.89, -121.49, -119.77, -118.19, -119.02] :: [Double]
cityLat = [34.05, 37.77, 32.72, 37.34, 38.58, 36.75, 33.77, 35.37] :: [Double]
citiesFrame = fromColumns [("lng", ColNum cityLng), ("lat", ColNum cityLat), ("name", ColCat cityName)]

zip3 cityName cityLng cityLat
```

> <!-- scripths:mime text/plain -->
> [("Los Angeles",-118.24,34.05),("San Francisco",-122.42,37.77),("San Diego",-117.16,32.72),("San Jose",-121.89,37.34),("Sacramento",-121.49,38.58),("Fresno",-119.77,36.75),("Long Beach",-118.19,33.77),("Bakersfield",-119.02,35.37)]

With both frames ready, the base scatter draws one point per district,
coloured by its price bucket:

```haskell
-- Step 3: the base scatter -- one point per district, coloured by price bucket.
housingLayer = (defLayer GeomPoint) { layerMapping = emptyMapping { aesX = Just (ColumnRef "lng"), aesY = Just (ColumnRef "lat"), aesColor = Just (ColumnRef "bucket") }, layerAesDef = emptyAesDefaults { defSize = Just 2 } }

baseChart = emptyChart { chartData = housingFrame, chartLayers = [housingLayer], chartTitle = Just "Median house value by location", chartSize = SizePixels 760 620 }

displayHtml (T.unpack (renderChartSvg baseChart))
```

Cheap districts are by far the most numerous, so they crowd the plot. Mapping
point opacity to the raw price with `aesAlpha` fades the cheap districts and
keeps the expensive ones solid; overlaying the city names then shows which
cities sit in the pricey clusters:

```haskell
-- Step 4: a single layer -- coloured by price bucket and faded by price with
fadedLayer = (defLayer GeomPoint) { layerMapping = emptyMapping { aesX = Just (ColumnRef "lng"), aesY = Just (ColumnRef "lat"), aesColor = Just (ColumnRef "bucket"), aesAlpha = Just (ColumnRef "value") }, layerAesDef = emptyAesDefaults { defSize = Just 2 } }

cityLabels = (defLayer GeomText) { layerData = Just citiesFrame, layerMapping = emptyMapping { aesX = Just (ColumnRef "lng"), aesY = Just (ColumnRef "lat"), aesLabel = Just (ColumnRef "name") }, layerAesDef = emptyAesDefaults { defColor = Just (Hex "#000000") } }

spatialChart = emptyChart { chartData = housingFrame, chartLayers = [fadedLayer, cityLabels], chartTitle = Just "Median house value by location (faded by price), with major cities", chartSize = SizePixels 760 620 }

displayHtml (T.unpack (renderChartSvg spatialChart))
```

From the plot above we can see that there are specific population centres where
house prices are high. A really good model should be able to account for this
non-linearity. The expensive population centres are coastal, which suggests that
`ocean_proximity` will be useful for predicting house prices.

## Engineering features

Now let's build those derived attributes. We encode `ocean_proximity` as an
ordinal value — 5 means closest to the ocean, 1 means farthest inland — impute
the missing `total_bedrooms` with the column mean, and add the per-household
ratios we reasoned about above.

```haskell
ordinalOceanProximity :: Text -> Double
ordinalOceanProximity op = case op of
    "ISLAND"     -> 5
    "NEAR OCEAN" -> 4
    "NEAR BAY"   -> 3
    "<1H OCEAN"  -> 2
    "INLAND"     -> 1
    _ -> error ("Unknown ocean proximity value: " ++ show op)


meanTotalBedrooms = D.meanMaybe total_bedrooms df
imputedTotalBedrooms = F.fromMaybe meanTotalBedrooms total_bedrooms
augmented = df |> D.deriveMany
                     [ "rooms_per_household" .= total_rooms / households
                     , "total_bedrooms" .= imputedTotalBedrooms
                     , "bedrooms_per_household" .= imputedTotalBedrooms / total_rooms
                     , "population_per_household".= population / households
                     , "ocean_proximity" .= F.lift ordinalOceanProximity ocean_proximity
                     ]
```

## How well do the features correlate with price?

Let's see how each attribute — original and engineered — correlates with our
target, `median_house_value`.

```haskell
correlationWithHouseValue columnName = (columnName, fromMaybe 0 (D.correlation columnName "median_house_value" augmented))

correlations = map correlationWithHouseValue (D.columnNames augmented)

mapM_ print (L.sortBy (flip compare `on` snd) correlations)
```

> <!-- scripths:mime text/plain -->
> ("median_house_value",1.0)
> ("median_income",0.6880752079585185)
> ("ocean_proximity",0.38324444779865596)
> ("rooms_per_household",0.1519482897414869)
> ("total_rooms",0.13415311380656322)
> ("housing_median_age",0.10562341249320993)
> ("households",6.584265057005642e-2)
> ("total_bedrooms",4.945354544321848e-2)
> ("population_per_household",-2.373741295613557e-2)
> ("population",-2.4649678888894886e-2)
> ("longitude",-4.5966615119112986e-2)
> ("latitude",-0.14416027687448565)
> ("bedrooms_per_household",-0.22004911702819213)

```haskell
-- A correlation heatmap of the augmented features: GeomTile coloured by the
-- pairwise correlation through a diverging scaleFill (blue = -1, red = +1),
-- with each cell's value labelled. Axis names are abbreviated to fit.
import Granite.Data.Frame (Column(ColNum, ColCat), fromColumns)
import Granite.Render.Pipeline (renderChartSvg)
import Granite.Spec (Geom(GeomTile, GeomText), Stat(StatIdentity), Scale(SColorContinuous), Scales(scaleFill), ColorSpec(Hex), defScales, defLayer, emptyMapping, emptyChart, layerMapping, layerStat, aesX, aesY, aesFill, aesLabel, ColumnRef(ColumnRef), chartData, chartLayers, chartScales, chartTitle, chartSize, Size(SizePixels))

corrAbbrev n = case n of { "median_house_value" -> "value"; "median_income" -> "income"; "ocean_proximity" -> "ocean"; "rooms_per_household" -> "rooms/hh"; "total_rooms" -> "rooms"; "housing_median_age" -> "age"; "households" -> "h.holds"; "total_bedrooms" -> "bedrms"; "population_per_household" -> "pop/hh"; "population" -> "pop"; "longitude" -> "lng"; "latitude" -> "lat"; "bedrooms_per_household" -> "bed/hh"; other -> other }

corrColumns = D.columnNames augmented
corrCells = [(corrAbbrev a, corrAbbrev b, fromMaybe 0 (D.correlation a b augmented)) | a <- corrColumns, b <- corrColumns]
corrFrame = fromColumns [("col", ColCat [a | (a, _, _) <- corrCells]), ("row", ColCat [b | (_, b, _) <- corrCells]), ("corr", ColNum [c | (_, _, c) <- corrCells]), ("label", ColCat [T.pack (show (fromIntegral (round (c * 100)) / 100 :: Double)) | (_, _, c) <- corrCells])]

corrTile = (defLayer GeomTile) { layerMapping = emptyMapping { aesX = Just (ColumnRef "col"), aesY = Just (ColumnRef "row"), aesFill = Just (ColumnRef "corr") }, layerStat = StatIdentity }
corrText = (defLayer GeomText) { layerMapping = emptyMapping { aesX = Just (ColumnRef "col"), aesY = Just (ColumnRef "row"), aesLabel = Just (ColumnRef "label") }, layerStat = StatIdentity }
corrChart = emptyChart { chartData = corrFrame, chartLayers = [corrTile, corrText], chartScales = defScales { scaleFill = Just (SColorContinuous [Hex "#2166ac", Hex "#f7f7f7", Hex "#b2182b"]) }, chartTitle = Just "Feature correlation matrix", chartSize = SizePixels 820 760 }

displayHtml (T.unpack (renderChartSvg corrChart))
```

> <!-- scripths:mime text/html -->
> <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 820 760" width="820" height="760" font-family="system-ui, -apple-system, sans-serif">
> <rect width="100%" height="100%" fill="white"/>
> <line x1="55" y1="732.20" x2="800" y2="732.20" stroke="#ecf0f1" stroke-width="1"/>
> <line x1="55" y1="34" x2="55" y2="732.20" stroke="#ecf0f1" stroke-width="1"/>
> <text x="110.03" y="747.20" text-anchor="middle" fill="#7f8c8d" font-size="11">lng</text>
> <text x="162.94" y="747.20" text-anchor="middle" fill="#7f8c8d" font-size="11">lat</text>
> <text x="215.85" y="747.20" text-anchor="middle" fill="#7f8c8d" font-size="11">age</text>
> <text x="268.76" y="747.20" text-anchor="middle" fill="#7f8c8d" font-size="11">rooms</text>
> <text x="321.68" y="747.20" text-anchor="middle" fill="#7f8c8d" font-size="11">bedrms</text>
> <text x="374.59" y="747.20" text-anchor="middle" fill="#7f8c8d" font-size="11">pop</text>
> <text x="427.50" y="747.20" text-anchor="middle" fill="#7f8c8d" font-size="11">h.holds</text>
> <text x="480.41" y="747.20" text-anchor="middle" fill="#7f8c8d" font-size="11">income</text>
> <text x="533.32" y="747.20" text-anchor="middle" fill="#7f8c8d" font-size="11">value</text>
> <text x="586.24" y="747.20" text-anchor="middle" fill="#7f8c8d" font-size="11">ocean</text>
> <text x="639.15" y="747.20" text-anchor="middle" fill="#7f8c8d" font-size="11">rooms/hh</text>
> <text x="692.06" y="747.20" text-anchor="middle" fill="#7f8c8d" font-size="11">bed/hh</text>
> <text x="744.97" y="747.20" text-anchor="middle" fill="#7f8c8d" font-size="11">pop/hh</text>
> <text x="49" y="684.30" text-anchor="end" fill="#7f8c8d" font-size="11">lng</text>
> <text x="49" y="634.71" text-anchor="end" fill="#7f8c8d" font-size="11">lat</text>
> <text x="49" y="585.12" text-anchor="end" fill="#7f8c8d" font-size="11">age</text>
> <text x="49" y="535.53" text-anchor="end" fill="#7f8c8d" font-size="11">rooms</text>
> <text x="49" y="485.94" text-anchor="end" fill="#7f8c8d" font-size="11">bedrms</text>
> <text x="49" y="436.35" text-anchor="end" fill="#7f8c8d" font-size="11">pop</text>
> <text x="49" y="386.77" text-anchor="end" fill="#7f8c8d" font-size="11">h.holds</text>
> <text x="49" y="337.18" text-anchor="end" fill="#7f8c8d" font-size="11">income</text>
> <text x="49" y="287.59" text-anchor="end" fill="#7f8c8d" font-size="11">value</text>
> <text x="49" y="238.00" text-anchor="end" fill="#7f8c8d" font-size="11">ocean</text>
> <text x="49" y="188.41" text-anchor="end" fill="#7f8c8d" font-size="11">rooms/hh</text>
> <text x="49" y="138.83" text-anchor="end" fill="#7f8c8d" font-size="11">bed/hh</text>
> <text x="49" y="89.24" text-anchor="end" fill="#7f8c8d" font-size="11">pop/hh</text>
> <rect x="83.57" y="655.83" width="52.91" height="49.59" fill="#b2182b"/>
> <rect x="83.57" y="606.25" width="52.91" height="49.59" fill="#2166ac"/>
> <rect x="83.57" y="556.66" width="52.91" height="49.59" fill="#d7e1ec"/>
> <rect x="83.57" y="507.07" width="52.91" height="49.59" fill="#f7f5f6"/>
> <rect x="83.57" y="457.48" width="52.91" height="49.59" fill="#f5f0f0"/>
> <rect x="83.57" y="407.89" width="52.91" height="49.59" fill="#f3e9ea"/>
> <rect x="83.57" y="358.31" width="52.91" height="49.59" fill="#f6f3f3"/>
> <rect x="83.57" y="308.72" width="52.91" height="49.59" fill="#ebeff3"/>
> <rect x="83.57" y="259.13" width="52.91" height="49.59" fill="#e4eaf0"/>
> <rect x="83.57" y="209.54" width="52.91" height="49.59" fill="#dae3ed"/>
> <rect x="83.57" y="159.95" width="52.91" height="49.59" fill="#e8edf2"/>
> <rect x="83.57" y="110.37" width="52.91" height="49.59" fill="#f4eeef"/>
> <rect x="83.57" y="60.78" width="52.91" height="49.59" fill="#eff2f4"/>
> <rect x="136.48" y="655.83" width="52.91" height="49.59" fill="#2166ac"/>
> <rect x="136.48" y="606.25" width="52.91" height="49.59" fill="#b2182b"/>
> <rect x="136.48" y="556.66" width="52.91" height="49.59" fill="#f1f3f5"/>
> <rect x="136.48" y="507.07" width="52.91" height="49.59" fill="#e7ecf1"/>
> <rect x="136.48" y="457.48" width="52.91" height="49.59" fill="#e0e7ef"/>
> <rect x="136.48" y="407.89" width="52.91" height="49.59" fill="#d6e1ec"/>
> <rect x="136.48" y="358.31" width="52.91" height="49.59" fill="#dfe7ef"/>
> <rect x="136.48" y="308.72" width="52.91" height="49.59" fill="#dde5ee"/>
> <rect x="136.48" y="259.13" width="52.91" height="49.59" fill="#cfdce9"/>
> <rect x="136.48" y="209.54" width="52.91" height="49.59" fill="#cad9e7"/>
> <rect x="136.48" y="159.95" width="52.91" height="49.59" fill="#f2e7e8"/>
> <rect x="136.48" y="110.37" width="52.91" height="49.59" fill="#dae3ed"/>
> <rect x="136.48" y="60.78" width="52.91" height="49.59" fill="#eff2f4"/>
> <rect x="189.40" y="655.83" width="52.91" height="49.59" fill="#d7e1ec"/>
> <rect x="189.40" y="606.25" width="52.91" height="49.59" fill="#f1f3f5"/>
> <rect x="189.40" y="556.66" width="52.91" height="49.59" fill="#b2182b"/>
> <rect x="189.40" y="507.07" width="52.91" height="49.59" fill="#9ebbd8"/>
> <rect x="189.40" y="457.48" width="52.91" height="49.59" fill="#a8c1db"/>
> <rect x="189.40" y="407.89" width="52.91" height="49.59" fill="#adc5dd"/>
> <rect x="189.40" y="358.31" width="52.91" height="49.59" fill="#abc4dc"/>
> <rect x="189.40" y="308.72" width="52.91" height="49.59" fill="#d4dfeb"/>
> <rect x="189.40" y="259.13" width="52.91" height="49.59" fill="#f2e7e9"/>
> <rect x="189.40" y="209.54" width="52.91" height="49.59" fill="#ebcfd2"/>
> <rect x="189.40" y="159.95" width="52.91" height="49.59" fill="#cddae8"/>
> <rect x="189.40" y="110.37" width="52.91" height="49.59" fill="#f0e1e3"/>
> <rect x="189.40" y="60.78" width="52.91" height="49.59" fill="#f2f3f5"/>
> <rect x="242.31" y="655.83" width="52.91" height="49.59" fill="#f7f5f6"/>
> <rect x="242.31" y="606.25" width="52.91" height="49.59" fill="#e7ecf1"/>
> <rect x="242.31" y="556.66" width="52.91" height="49.59" fill="#9ebbd8"/>
> <rect x="242.31" y="507.07" width="52.91" height="49.59" fill="#b2182b"/>
> <rect x="242.31" y="457.48" width="52.91" height="49.59" fill="#b7293a"/>
> <rect x="242.31" y="407.89" width="52.91" height="49.59" fill="#bc3949"/>
> <rect x="242.31" y="358.31" width="52.91" height="49.59" fill="#b82b3c"/>
> <rect x="242.31" y="308.72" width="52.91" height="49.59" fill="#ecd2d5"/>
> <rect x="242.31" y="259.13" width="52.91" height="49.59" fill="#f0e1e3"/>
> <rect x="242.31" y="209.54" width="52.91" height="49.59" fill="#e9edf2"/>
> <rect x="242.31" y="159.95" width="52.91" height="49.59" fill="#f0e1e3"/>
> <rect x="242.31" y="110.37" width="52.91" height="49.59" fill="#c7d6e6"/>
> <rect x="242.31" y="60.78" width="52.91" height="49.59" fill="#e9eef2"/>
> <rect x="295.22" y="655.83" width="52.91" height="49.59" fill="#f5f0f0"/>
> <rect x="295.22" y="606.25" width="52.91" height="49.59" fill="#e0e7ef"/>
> <rect x="295.22" y="556.66" width="52.91" height="49.59" fill="#a8c1db"/>
> <rect x="295.22" y="507.07" width="52.91" height="49.59" fill="#b7293a"/>
> <rect x="295.22" y="457.48" width="52.91" height="49.59" fill="#b2182b"/>
> <rect x="295.22" y="407.89" width="52.91" height="49.59" fill="#bb3546"/>
> <rect x="295.22" y="358.31" width="52.91" height="49.59" fill="#b41e30"/>
> <rect x="295.22" y="308.72" width="52.91" height="49.59" fill="#edf0f3"/>
> <rect x="295.22" y="259.13" width="52.91" height="49.59" fill="#f6f4f5"/>
> <rect x="295.22" y="209.54" width="52.91" height="49.59" fill="#eef1f4"/>
> <rect x="295.22" y="159.95" width="52.91" height="49.59" fill="#eff2f4"/>
> <rect x="295.22" y="110.37" width="52.91" height="49.59" fill="#f5f0f0"/>
> <rect x="295.22" y="60.78" width="52.91" height="49.59" fill="#e8edf2"/>
> <rect x="348.13" y="655.83" width="52.91" height="49.59" fill="#f3e9ea"/>
> <rect x="348.13" y="606.25" width="52.91" height="49.59" fill="#d6e1ec"/>
> <rect x="348.13" y="556.66" width="52.91" height="49.59" fill="#adc5dd"/>
> <rect x="348.13" y="507.07" width="52.91" height="49.59" fill="#bc3949"/>
> <rect x="348.13" y="457.48" width="52.91" height="49.59" fill="#bb3546"/>
> <rect x="348.13" y="407.89" width="52.91" height="49.59" fill="#b2182b"/>
> <rect x="348.13" y="358.31" width="52.91" height="49.59" fill="#b92d3f"/>
> <rect x="348.13" y="308.72" width="52.91" height="49.59" fill="#f0f2f4"/>
> <rect x="348.13" y="259.13" width="52.91" height="49.59" fill="#e9eef2"/>
> <rect x="348.13" y="209.54" width="52.91" height="49.59" fill="#e9edf2"/>
> <rect x="348.13" y="159.95" width="52.91" height="49.59" fill="#dfe6ee"/>
> <rect x="348.13" y="110.37" width="52.91" height="49.59" fill="#f0f2f4"/>
> <rect x="348.13" y="60.78" width="52.91" height="49.59" fill="#f5f0f0"/>
> <rect x="401.04" y="655.83" width="52.91" height="49.59" fill="#f6f3f3"/>
> <rect x="401.04" y="606.25" width="52.91" height="49.59" fill="#dfe7ef"/>
> <rect x="401.04" y="556.66" width="52.91" height="49.59" fill="#abc4dc"/>
> <rect x="401.04" y="507.07" width="52.91" height="49.59" fill="#b82b3c"/>
> <rect x="401.04" y="457.48" width="52.91" height="49.59" fill="#b41e30"/>
> <rect x="401.04" y="407.89" width="52.91" height="49.59" fill="#b92d3f"/>
> <rect x="401.04" y="358.31" width="52.91" height="49.59" fill="#b2182b"/>
> <rect x="401.04" y="308.72" width="52.91" height="49.59" fill="#f2f3f5"/>
> <rect x="401.04" y="259.13" width="52.91" height="49.59" fill="#f5f0f1"/>
> <rect x="401.04" y="209.54" width="52.91" height="49.59" fill="#f2f4f5"/>
> <rect x="401.04" y="159.95" width="52.91" height="49.59" fill="#dde5ee"/>
> <rect x="401.04" y="110.37" width="52.91" height="49.59" fill="#f5f5f6"/>
> <rect x="401.04" y="60.78" width="52.91" height="49.59" fill="#e9edf2"/>
> <rect x="453.96" y="655.83" width="52.91" height="49.59" fill="#ebeff3"/>
> <rect x="453.96" y="606.25" width="52.91" height="49.59" fill="#dde5ee"/>
> <rect x="453.96" y="556.66" width="52.91" height="49.59" fill="#d4dfeb"/>
> <rect x="453.96" y="507.07" width="52.91" height="49.59" fill="#ecd2d5"/>
> <rect x="453.96" y="457.48" width="52.91" height="49.59" fill="#edf0f3"/>
> <rect x="453.96" y="407.89" width="52.91" height="49.59" fill="#f0f2f4"/>
> <rect x="453.96" y="358.31" width="52.91" height="49.59" fill="#f2f3f5"/>
> <rect x="453.96" y="308.72" width="52.91" height="49.59" fill="#b2182b"/>
> <rect x="453.96" y="259.13" width="52.91" height="49.59" fill="#c8606d"/>
> <rect x="453.96" y="209.54" width="52.91" height="49.59" fill="#efdddf"/>
> <rect x="453.96" y="159.95" width="52.91" height="49.59" fill="#e2b4ba"/>
> <rect x="453.96" y="110.37" width="52.91" height="49.59" fill="#7da4cc"/>
> <rect x="453.96" y="60.78" width="52.91" height="49.59" fill="#f3f4f6"/>
> <rect x="506.87" y="655.83" width="52.91" height="49.59" fill="#e4eaf0"/>
> <rect x="506.87" y="606.25" width="52.91" height="49.59" fill="#cfdce9"/>
> <rect x="506.87" y="556.66" width="52.91" height="49.59" fill="#f2e7e9"/>
> <rect x="506.87" y="507.07" width="52.91" height="49.59" fill="#f0e1e3"/>
> <rect x="506.87" y="457.48" width="52.91" height="49.59" fill="#f6f4f5"/>
> <rect x="506.87" y="407.89" width="52.91" height="49.59" fill="#e9eef2"/>
> <rect x="506.87" y="358.31" width="52.91" height="49.59" fill="#f5f0f1"/>
> <rect x="506.87" y="308.72" width="52.91" height="49.59" fill="#c8606d"/>
> <rect x="506.87" y="259.13" width="52.91" height="49.59" fill="#b2182b"/>
> <rect x="506.87" y="209.54" width="52.91" height="49.59" fill="#dea7ae"/>
> <rect x="506.87" y="159.95" width="52.91" height="49.59" fill="#efdddf"/>
> <rect x="506.87" y="110.37" width="52.91" height="49.59" fill="#bed0e3"/>
> <rect x="506.87" y="60.78" width="52.91" height="49.59" fill="#e9eef2"/>
> <rect x="559.78" y="655.83" width="52.91" height="49.59" fill="#dae3ed"/>
> <rect x="559.78" y="606.25" width="52.91" height="49.59" fill="#cad9e7"/>
> <rect x="559.78" y="556.66" width="52.91" height="49.59" fill="#ebcfd2"/>
> <rect x="559.78" y="507.07" width="52.91" height="49.59" fill="#e9edf2"/>
> <rect x="559.78" y="457.48" width="52.91" height="49.59" fill="#eef1f4"/>
> <rect x="559.78" y="407.89" width="52.91" height="49.59" fill="#e9edf2"/>
> <rect x="559.78" y="358.31" width="52.91" height="49.59" fill="#f2f4f5"/>
> <rect x="559.78" y="308.72" width="52.91" height="49.59" fill="#efdddf"/>
> <rect x="559.78" y="259.13" width="52.91" height="49.59" fill="#dea7ae"/>
> <rect x="559.78" y="209.54" width="52.91" height="49.59" fill="#b2182b"/>
> <rect x="559.78" y="159.95" width="52.91" height="49.59" fill="#d7e1ec"/>
> <rect x="559.78" y="110.37" width="52.91" height="49.59" fill="#f5f1f1"/>
> <rect x="559.78" y="60.78" width="52.91" height="49.59" fill="#ebeff3"/>
> <rect x="612.69" y="655.83" width="52.91" height="49.59" fill="#e8edf2"/>
> <rect x="612.69" y="606.25" width="52.91" height="49.59" fill="#f2e7e8"/>
> <rect x="612.69" y="556.66" width="52.91" height="49.59" fill="#cddae8"/>
> <rect x="612.69" y="507.07" width="52.91" height="49.59" fill="#f0e1e3"/>
> <rect x="612.69" y="457.48" width="52.91" height="49.59" fill="#eff2f4"/>
> <rect x="612.69" y="407.89" width="52.91" height="49.59" fill="#dfe6ee"/>
> <rect x="612.69" y="358.31" width="52.91" height="49.59" fill="#dde5ee"/>
> <rect x="612.69" y="308.72" width="52.91" height="49.59" fill="#e2b4ba"/>
> <rect x="612.69" y="259.13" width="52.91" height="49.59" fill="#efdddf"/>
> <rect x="612.69" y="209.54" width="52.91" height="49.59" fill="#d7e1ec"/>
> <rect x="612.69" y="159.95" width="52.91" height="49.59" fill="#b2182b"/>
> <rect x="612.69" y="110.37" width="52.91" height="49.59" fill="#a1bdd9"/>
> <rect x="612.69" y="60.78" width="52.91" height="49.59" fill="#eef1f4"/>
> <rect x="665.60" y="655.83" width="52.91" height="49.59" fill="#f4eeef"/>
> <rect x="665.60" y="606.25" width="52.91" height="49.59" fill="#dae3ed"/>
> <rect x="665.60" y="556.66" width="52.91" height="49.59" fill="#f0e1e3"/>
> <rect x="665.60" y="507.07" width="52.91" height="49.59" fill="#c7d6e6"/>
> <rect x="665.60" y="457.48" width="52.91" height="49.59" fill="#f5f0f0"/>
> <rect x="665.60" y="407.89" width="52.91" height="49.59" fill="#f0f2f4"/>
> <rect x="665.60" y="358.31" width="52.91" height="49.59" fill="#f5f5f6"/>
> <rect x="665.60" y="308.72" width="52.91" height="49.59" fill="#7da4cc"/>
> <rect x="665.60" y="259.13" width="52.91" height="49.59" fill="#bed0e3"/>
> <rect x="665.60" y="209.54" width="52.91" height="49.59" fill="#f5f1f1"/>
> <rect x="665.60" y="159.95" width="52.91" height="49.59" fill="#a1bdd9"/>
> <rect x="665.60" y="110.37" width="52.91" height="49.59" fill="#b2182b"/>
> <rect x="665.60" y="60.78" width="52.91" height="49.59" fill="#eff2f4"/>
> <rect x="718.52" y="655.83" width="52.91" height="49.59" fill="#eff2f4"/>
> <rect x="718.52" y="606.25" width="52.91" height="49.59" fill="#eff2f4"/>
> <rect x="718.52" y="556.66" width="52.91" height="49.59" fill="#f2f3f5"/>
> <rect x="718.52" y="507.07" width="52.91" height="49.59" fill="#e9eef2"/>
> <rect x="718.52" y="457.48" width="52.91" height="49.59" fill="#e8edf2"/>
> <rect x="718.52" y="407.89" width="52.91" height="49.59" fill="#f5f0f0"/>
> <rect x="718.52" y="358.31" width="52.91" height="49.59" fill="#e9edf2"/>
> <rect x="718.52" y="308.72" width="52.91" height="49.59" fill="#f3f4f6"/>
> <rect x="718.52" y="259.13" width="52.91" height="49.59" fill="#e9eef2"/>
> <rect x="718.52" y="209.54" width="52.91" height="49.59" fill="#ebeff3"/>
> <rect x="718.52" y="159.95" width="52.91" height="49.59" fill="#eef1f4"/>
> <rect x="718.52" y="110.37" width="52.91" height="49.59" fill="#eff2f4"/>
> <rect x="718.52" y="60.78" width="52.91" height="49.59" fill="#b2182b"/>
> <text x="110.03" y="680.63" text-anchor="middle" fill="#9b59b6" font-size="11">1.0</text>
> <text x="110.03" y="631.04" text-anchor="middle" fill="#9b59b6" font-size="11">-0.92</text>
> <text x="110.03" y="581.45" text-anchor="middle" fill="#9b59b6" font-size="11">-0.11</text>
> <text x="110.03" y="531.86" text-anchor="middle" fill="#9b59b6" font-size="11">4.0e-2</text>
> <text x="110.03" y="482.28" text-anchor="middle" fill="#9b59b6" font-size="11">7.0e-2</text>
> <text x="110.03" y="432.69" text-anchor="middle" fill="#9b59b6" font-size="11">0.1</text>
> <text x="110.03" y="383.10" text-anchor="middle" fill="#9b59b6" font-size="11">6.0e-2</text>
> <text x="110.03" y="333.51" text-anchor="middle" fill="#9b59b6" font-size="11">-2.0e-2</text>
> <text x="110.03" y="283.92" text-anchor="middle" fill="#9b59b6" font-size="11">-5.0e-2</text>
> <text x="110.03" y="234.34" text-anchor="middle" fill="#9b59b6" font-size="11">-9.0e-2</text>
> <text x="110.03" y="184.75" text-anchor="middle" fill="#9b59b6" font-size="11">-3.0e-2</text>
> <text x="110.03" y="135.16" text-anchor="middle" fill="#9b59b6" font-size="11">8.0e-2</text>
> <text x="110.03" y="85.57" text-anchor="middle" fill="#9b59b6" font-size="11">0.0</text>
> <text x="162.94" y="680.63" text-anchor="middle" fill="#9b59b6" font-size="11">-0.92</text>
> <text x="162.94" y="631.04" text-anchor="middle" fill="#9b59b6" font-size="11">1.0</text>
> <text x="162.94" y="581.45" text-anchor="middle" fill="#9b59b6" font-size="11">1.0e-2</text>
> <text x="162.94" y="531.86" text-anchor="middle" fill="#9b59b6" font-size="11">-4.0e-2</text>
> <text x="162.94" y="482.28" text-anchor="middle" fill="#9b59b6" font-size="11">-7.0e-2</text>
> <text x="162.94" y="432.69" text-anchor="middle" fill="#9b59b6" font-size="11">-0.11</text>
> <text x="162.94" y="383.10" text-anchor="middle" fill="#9b59b6" font-size="11">-7.0e-2</text>
> <text x="162.94" y="333.51" text-anchor="middle" fill="#9b59b6" font-size="11">-8.0e-2</text>
> <text x="162.94" y="283.92" text-anchor="middle" fill="#9b59b6" font-size="11">-0.14</text>
> <text x="162.94" y="234.34" text-anchor="middle" fill="#9b59b6" font-size="11">-0.16</text>
> <text x="162.94" y="184.75" text-anchor="middle" fill="#9b59b6" font-size="11">0.11</text>
> <text x="162.94" y="135.16" text-anchor="middle" fill="#9b59b6" font-size="11">-9.0e-2</text>
> <text x="162.94" y="85.57" text-anchor="middle" fill="#9b59b6" font-size="11">0.0</text>
> <text x="215.85" y="680.63" text-anchor="middle" fill="#9b59b6" font-size="11">-0.11</text>
> <text x="215.85" y="631.04" text-anchor="middle" fill="#9b59b6" font-size="11">1.0e-2</text>
> <text x="215.85" y="581.45" text-anchor="middle" fill="#9b59b6" font-size="11">1.0</text>
> <text x="215.85" y="531.86" text-anchor="middle" fill="#9b59b6" font-size="11">-0.36</text>
> <text x="215.85" y="482.28" text-anchor="middle" fill="#9b59b6" font-size="11">-0.32</text>
> <text x="215.85" y="432.69" text-anchor="middle" fill="#9b59b6" font-size="11">-0.3</text>
> <text x="215.85" y="383.10" text-anchor="middle" fill="#9b59b6" font-size="11">-0.3</text>
> <text x="215.85" y="333.51" text-anchor="middle" fill="#9b59b6" font-size="11">-0.12</text>
> <text x="215.85" y="283.92" text-anchor="middle" fill="#9b59b6" font-size="11">0.11</text>
> <text x="215.85" y="234.34" text-anchor="middle" fill="#9b59b6" font-size="11">0.21</text>
> <text x="215.85" y="184.75" text-anchor="middle" fill="#9b59b6" font-size="11">-0.15</text>
> <text x="215.85" y="135.16" text-anchor="middle" fill="#9b59b6" font-size="11">0.13</text>
> <text x="215.85" y="85.57" text-anchor="middle" fill="#9b59b6" font-size="11">1.0e-2</text>
> <text x="268.76" y="680.63" text-anchor="middle" fill="#9b59b6" font-size="11">4.0e-2</text>
> <text x="268.76" y="631.04" text-anchor="middle" fill="#9b59b6" font-size="11">-4.0e-2</text>
> <text x="268.76" y="581.45" text-anchor="middle" fill="#9b59b6" font-size="11">-0.36</text>
> <text x="268.76" y="531.86" text-anchor="middle" fill="#9b59b6" font-size="11">1.0</text>
> <text x="268.76" y="482.28" text-anchor="middle" fill="#9b59b6" font-size="11">0.93</text>
> <text x="268.76" y="432.69" text-anchor="middle" fill="#9b59b6" font-size="11">0.86</text>
> <text x="268.76" y="383.10" text-anchor="middle" fill="#9b59b6" font-size="11">0.92</text>
> <text x="268.76" y="333.51" text-anchor="middle" fill="#9b59b6" font-size="11">0.2</text>
> <text x="268.76" y="283.92" text-anchor="middle" fill="#9b59b6" font-size="11">0.13</text>
> <text x="268.76" y="234.34" text-anchor="middle" fill="#9b59b6" font-size="11">-3.0e-2</text>
> <text x="268.76" y="184.75" text-anchor="middle" fill="#9b59b6" font-size="11">0.13</text>
> <text x="268.76" y="135.16" text-anchor="middle" fill="#9b59b6" font-size="11">-0.18</text>
> <text x="268.76" y="85.57" text-anchor="middle" fill="#9b59b6" font-size="11">-2.0e-2</text>
> <text x="321.68" y="680.63" text-anchor="middle" fill="#9b59b6" font-size="11">7.0e-2</text>
> <text x="321.68" y="631.04" text-anchor="middle" fill="#9b59b6" font-size="11">-7.0e-2</text>
> <text x="321.68" y="581.45" text-anchor="middle" fill="#9b59b6" font-size="11">-0.32</text>
> <text x="321.68" y="531.86" text-anchor="middle" fill="#9b59b6" font-size="11">0.93</text>
> <text x="321.68" y="482.28" text-anchor="middle" fill="#9b59b6" font-size="11">1.0</text>
> <text x="321.68" y="432.69" text-anchor="middle" fill="#9b59b6" font-size="11">0.87</text>
> <text x="321.68" y="383.10" text-anchor="middle" fill="#9b59b6" font-size="11">0.97</text>
> <text x="321.68" y="333.51" text-anchor="middle" fill="#9b59b6" font-size="11">-1.0e-2</text>
> <text x="321.68" y="283.92" text-anchor="middle" fill="#9b59b6" font-size="11">5.0e-2</text>
> <text x="321.68" y="234.34" text-anchor="middle" fill="#9b59b6" font-size="11">0.0</text>
> <text x="321.68" y="184.75" text-anchor="middle" fill="#9b59b6" font-size="11">0.0</text>
> <text x="321.68" y="135.16" text-anchor="middle" fill="#9b59b6" font-size="11">7.0e-2</text>
> <text x="321.68" y="85.57" text-anchor="middle" fill="#9b59b6" font-size="11">-3.0e-2</text>
> <text x="374.59" y="680.63" text-anchor="middle" fill="#9b59b6" font-size="11">0.1</text>
> <text x="374.59" y="631.04" text-anchor="middle" fill="#9b59b6" font-size="11">-0.11</text>
> <text x="374.59" y="581.45" text-anchor="middle" fill="#9b59b6" font-size="11">-0.3</text>
> <text x="374.59" y="531.86" text-anchor="middle" fill="#9b59b6" font-size="11">0.86</text>
> <text x="374.59" y="482.28" text-anchor="middle" fill="#9b59b6" font-size="11">0.87</text>
> <text x="374.59" y="432.69" text-anchor="middle" fill="#9b59b6" font-size="11">1.0</text>
> <text x="374.59" y="383.10" text-anchor="middle" fill="#9b59b6" font-size="11">0.91</text>
> <text x="374.59" y="333.51" text-anchor="middle" fill="#9b59b6" font-size="11">0.0</text>
> <text x="374.59" y="283.92" text-anchor="middle" fill="#9b59b6" font-size="11">-2.0e-2</text>
> <text x="374.59" y="234.34" text-anchor="middle" fill="#9b59b6" font-size="11">-3.0e-2</text>
> <text x="374.59" y="184.75" text-anchor="middle" fill="#9b59b6" font-size="11">-7.0e-2</text>
> <text x="374.59" y="135.16" text-anchor="middle" fill="#9b59b6" font-size="11">1.0e-2</text>
> <text x="374.59" y="85.57" text-anchor="middle" fill="#9b59b6" font-size="11">7.0e-2</text>
> <text x="427.50" y="680.63" text-anchor="middle" fill="#9b59b6" font-size="11">6.0e-2</text>
> <text x="427.50" y="631.04" text-anchor="middle" fill="#9b59b6" font-size="11">-7.0e-2</text>
> <text x="427.50" y="581.45" text-anchor="middle" fill="#9b59b6" font-size="11">-0.3</text>
> <text x="427.50" y="531.86" text-anchor="middle" fill="#9b59b6" font-size="11">0.92</text>
> <text x="427.50" y="482.28" text-anchor="middle" fill="#9b59b6" font-size="11">0.97</text>
> <text x="427.50" y="432.69" text-anchor="middle" fill="#9b59b6" font-size="11">0.91</text>
> <text x="427.50" y="383.10" text-anchor="middle" fill="#9b59b6" font-size="11">1.0</text>
> <text x="427.50" y="333.51" text-anchor="middle" fill="#9b59b6" font-size="11">1.0e-2</text>
> <text x="427.50" y="283.92" text-anchor="middle" fill="#9b59b6" font-size="11">7.0e-2</text>
> <text x="427.50" y="234.34" text-anchor="middle" fill="#9b59b6" font-size="11">2.0e-2</text>
> <text x="427.50" y="184.75" text-anchor="middle" fill="#9b59b6" font-size="11">-8.0e-2</text>
> <text x="427.50" y="135.16" text-anchor="middle" fill="#9b59b6" font-size="11">3.0e-2</text>
> <text x="427.50" y="85.57" text-anchor="middle" fill="#9b59b6" font-size="11">-3.0e-2</text>
> <text x="480.41" y="680.63" text-anchor="middle" fill="#9b59b6" font-size="11">-2.0e-2</text>
> <text x="480.41" y="631.04" text-anchor="middle" fill="#9b59b6" font-size="11">-8.0e-2</text>
> <text x="480.41" y="581.45" text-anchor="middle" fill="#9b59b6" font-size="11">-0.12</text>
> <text x="480.41" y="531.86" text-anchor="middle" fill="#9b59b6" font-size="11">0.2</text>
> <text x="480.41" y="482.28" text-anchor="middle" fill="#9b59b6" font-size="11">-1.0e-2</text>
> <text x="480.41" y="432.69" text-anchor="middle" fill="#9b59b6" font-size="11">0.0</text>
> <text x="480.41" y="383.10" text-anchor="middle" fill="#9b59b6" font-size="11">1.0e-2</text>
> <text x="480.41" y="333.51" text-anchor="middle" fill="#9b59b6" font-size="11">1.0</text>
> <text x="480.41" y="283.92" text-anchor="middle" fill="#9b59b6" font-size="11">0.69</text>
> <text x="480.41" y="234.34" text-anchor="middle" fill="#9b59b6" font-size="11">0.15</text>
> <text x="480.41" y="184.75" text-anchor="middle" fill="#9b59b6" font-size="11">0.33</text>
> <text x="480.41" y="135.16" text-anchor="middle" fill="#9b59b6" font-size="11">-0.51</text>
> <text x="480.41" y="85.57" text-anchor="middle" fill="#9b59b6" font-size="11">2.0e-2</text>
> <text x="533.32" y="680.63" text-anchor="middle" fill="#9b59b6" font-size="11">-5.0e-2</text>
> <text x="533.32" y="631.04" text-anchor="middle" fill="#9b59b6" font-size="11">-0.14</text>
> <text x="533.32" y="581.45" text-anchor="middle" fill="#9b59b6" font-size="11">0.11</text>
> <text x="533.32" y="531.86" text-anchor="middle" fill="#9b59b6" font-size="11">0.13</text>
> <text x="533.32" y="482.28" text-anchor="middle" fill="#9b59b6" font-size="11">5.0e-2</text>
> <text x="533.32" y="432.69" text-anchor="middle" fill="#9b59b6" font-size="11">-2.0e-2</text>
> <text x="533.32" y="383.10" text-anchor="middle" fill="#9b59b6" font-size="11">7.0e-2</text>
> <text x="533.32" y="333.51" text-anchor="middle" fill="#9b59b6" font-size="11">0.69</text>
> <text x="533.32" y="283.92" text-anchor="middle" fill="#9b59b6" font-size="11">1.0</text>
> <text x="533.32" y="234.34" text-anchor="middle" fill="#9b59b6" font-size="11">0.38</text>
> <text x="533.32" y="184.75" text-anchor="middle" fill="#9b59b6" font-size="11">0.15</text>
> <text x="533.32" y="135.16" text-anchor="middle" fill="#9b59b6" font-size="11">-0.22</text>
> <text x="533.32" y="85.57" text-anchor="middle" fill="#9b59b6" font-size="11">-2.0e-2</text>
> <text x="586.24" y="680.63" text-anchor="middle" fill="#9b59b6" font-size="11">-9.0e-2</text>
> <text x="586.24" y="631.04" text-anchor="middle" fill="#9b59b6" font-size="11">-0.16</text>
> <text x="586.24" y="581.45" text-anchor="middle" fill="#9b59b6" font-size="11">0.21</text>
> <text x="586.24" y="531.86" text-anchor="middle" fill="#9b59b6" font-size="11">-3.0e-2</text>
> <text x="586.24" y="482.28" text-anchor="middle" fill="#9b59b6" font-size="11">0.0</text>
> <text x="586.24" y="432.69" text-anchor="middle" fill="#9b59b6" font-size="11">-3.0e-2</text>
> <text x="586.24" y="383.10" text-anchor="middle" fill="#9b59b6" font-size="11">2.0e-2</text>
> <text x="586.24" y="333.51" text-anchor="middle" fill="#9b59b6" font-size="11">0.15</text>
> <text x="586.24" y="283.92" text-anchor="middle" fill="#9b59b6" font-size="11">0.38</text>
> <text x="586.24" y="234.34" text-anchor="middle" fill="#9b59b6" font-size="11">1.0</text>
> <text x="586.24" y="184.75" text-anchor="middle" fill="#9b59b6" font-size="11">-0.11</text>
> <text x="586.24" y="135.16" text-anchor="middle" fill="#9b59b6" font-size="11">7.0e-2</text>
> <text x="586.24" y="85.57" text-anchor="middle" fill="#9b59b6" font-size="11">-2.0e-2</text>
> <text x="639.15" y="680.63" text-anchor="middle" fill="#9b59b6" font-size="11">-3.0e-2</text>
> <text x="639.15" y="631.04" text-anchor="middle" fill="#9b59b6" font-size="11">0.11</text>
> <text x="639.15" y="581.45" text-anchor="middle" fill="#9b59b6" font-size="11">-0.15</text>
> <text x="639.15" y="531.86" text-anchor="middle" fill="#9b59b6" font-size="11">0.13</text>
> <text x="639.15" y="482.28" text-anchor="middle" fill="#9b59b6" font-size="11">0.0</text>
> <text x="639.15" y="432.69" text-anchor="middle" fill="#9b59b6" font-size="11">-7.0e-2</text>
> <text x="639.15" y="383.10" text-anchor="middle" fill="#9b59b6" font-size="11">-8.0e-2</text>
> <text x="639.15" y="333.51" text-anchor="middle" fill="#9b59b6" font-size="11">0.33</text>
> <text x="639.15" y="283.92" text-anchor="middle" fill="#9b59b6" font-size="11">0.15</text>
> <text x="639.15" y="234.34" text-anchor="middle" fill="#9b59b6" font-size="11">-0.11</text>
> <text x="639.15" y="184.75" text-anchor="middle" fill="#9b59b6" font-size="11">1.0</text>
> <text x="639.15" y="135.16" text-anchor="middle" fill="#9b59b6" font-size="11">-0.35</text>
> <text x="639.15" y="85.57" text-anchor="middle" fill="#9b59b6" font-size="11">0.0</text>
> <text x="692.06" y="680.63" text-anchor="middle" fill="#9b59b6" font-size="11">8.0e-2</text>
> <text x="692.06" y="631.04" text-anchor="middle" fill="#9b59b6" font-size="11">-9.0e-2</text>
> <text x="692.06" y="581.45" text-anchor="middle" fill="#9b59b6" font-size="11">0.13</text>
> <text x="692.06" y="531.86" text-anchor="middle" fill="#9b59b6" font-size="11">-0.18</text>
> <text x="692.06" y="482.28" text-anchor="middle" fill="#9b59b6" font-size="11">7.0e-2</text>
> <text x="692.06" y="432.69" text-anchor="middle" fill="#9b59b6" font-size="11">1.0e-2</text>
> <text x="692.06" y="383.10" text-anchor="middle" fill="#9b59b6" font-size="11">3.0e-2</text>
> <text x="692.06" y="333.51" text-anchor="middle" fill="#9b59b6" font-size="11">-0.51</text>
> <text x="692.06" y="283.92" text-anchor="middle" fill="#9b59b6" font-size="11">-0.22</text>
> <text x="692.06" y="234.34" text-anchor="middle" fill="#9b59b6" font-size="11">7.0e-2</text>
> <text x="692.06" y="184.75" text-anchor="middle" fill="#9b59b6" font-size="11">-0.35</text>
> <text x="692.06" y="135.16" text-anchor="middle" fill="#9b59b6" font-size="11">1.0</text>
> <text x="692.06" y="85.57" text-anchor="middle" fill="#9b59b6" font-size="11">0.0</text>
> <text x="744.97" y="680.63" text-anchor="middle" fill="#9b59b6" font-size="11">0.0</text>
> <text x="744.97" y="631.04" text-anchor="middle" fill="#9b59b6" font-size="11">0.0</text>
> <text x="744.97" y="581.45" text-anchor="middle" fill="#9b59b6" font-size="11">1.0e-2</text>
> <text x="744.97" y="531.86" text-anchor="middle" fill="#9b59b6" font-size="11">-2.0e-2</text>
> <text x="744.97" y="482.28" text-anchor="middle" fill="#9b59b6" font-size="11">-3.0e-2</text>
> <text x="744.97" y="432.69" text-anchor="middle" fill="#9b59b6" font-size="11">7.0e-2</text>
> <text x="744.97" y="383.10" text-anchor="middle" fill="#9b59b6" font-size="11">-3.0e-2</text>
> <text x="744.97" y="333.51" text-anchor="middle" fill="#9b59b6" font-size="11">2.0e-2</text>
> <text x="744.97" y="283.92" text-anchor="middle" fill="#9b59b6" font-size="11">-2.0e-2</text>
> <text x="744.97" y="234.34" text-anchor="middle" fill="#9b59b6" font-size="11">-2.0e-2</text>
> <text x="744.97" y="184.75" text-anchor="middle" fill="#9b59b6" font-size="11">0.0</text>
> <text x="744.97" y="135.16" text-anchor="middle" fill="#9b59b6" font-size="11">0.0</text>
> <text x="744.97" y="85.57" text-anchor="middle" fill="#9b59b6" font-size="11">1.0</text>
> <text x="427.50" y="26" text-anchor="middle" fill="#7f8c8d" font-size="14">Feature correlation matrix</text>
> </svg>

Not bad! The engineered ratios are much more informative than the raw totals:
the number of rooms per household tells you more than the total number of rooms
in a district — obviously, the larger the houses, the more expensive they are.
And once we translate ocean proximity to an ordinal value (5 = close to the
ocean, 1 = far), we can see how predictive that feature is too. These are
exactly the features we'll feed the model.

## Modeling: linear regression with Hasktorch

Exploration done. We'll now train a linear-regression model on the `augmented`
features to predict `median_house_value`, using Hasktorch for the tensors and
gradient descent.

### Train/test split

We hold out 20% of the districts for testing, with a fixed seed for
reproducibility.

```haskell
let (train, test) = D.randomSplit (mkStdGen 42) 0.8 augmented

(D.dimensions train, D.dimensions test)
```

> <!-- scripths:mime text/plain -->
> ((16408,13),(4232,13))

### Building the tensors

Min-max normalisation rescales every numeric column to the `[0, 1]` range, which
keeps the different feature scales from dominating the gradient updates. We use
`D.fold` to apply the rescaling across every `Double` column.

```haskell
normalizeFeatures :: D.DataFrame -> D.DataFrame
normalizeFeatures df =
    df
        |> D.fold
            ( \name d ->
                let
                    -- Convenience reference to the column.
                    col = F.col @Double name
                 in
                    D.derive name ((col - F.minimum col) / (F.maximum col - F.minimum col)) d
            )
            (D.columnNames (df |> D.selectBy [D.byProperty (D.hasElemType @Double)]))
```

We drop the target column (`median_house_value`) from the features, normalise
them, and convert both features and labels to Hasktorch tensors with
`toTensor`.

```haskell
let trainFeatures =
        toTensor (train |> D.exclude [F.name median_house_value] |> normalizeFeatures)
let testFeatures =
        toTensor (test |> D.exclude [F.name median_house_value] |> normalizeFeatures)
let trainLabels = toTensor (D.select [F.name median_house_value] train)
```

### The model

The model is one linear layer; `squeezeAll` drops the trailing size-1 dimension
so the output is a flat vector of predictions.

```haskell
-- compile: LinearRegression

import Torch

model :: Linear -> Tensor -> Tensor
model state input = squeezeAll $ linear state input
```

### Training

We initialise a linear layer with one output, then run 1,000 gradient-descent
steps minimising mean-squared error, logging the loss every 100 iterations.
(The original program runs 100,000 steps; 1,000 is plenty to watch the loss
fall and keeps the notebook responsive — bump `foldLoop init 1_000` higher for
a better fit.)

```haskell
import Control.Monad (when)

putStrLn "Training linear regression model..."

init <-
    Torch.sample $
        LinearSpec{in_features = snd (D.dimensions train) - 1, out_features = 1}

trained <- foldLoop init 1_000 $ \state i -> do
    let labels' = model state trainFeatures
        loss = mseLoss trainLabels labels'
    when (i `mod` 100 == 0) $
        putStrLn $ "Iteration: " ++ show i ++ " | Loss: " ++ show loss
    (state', _) <- runStep state GD loss 0.1
    pure state'
```

### Predictions

Finally we run the trained model on the held-out test set and place the
predictions next to the true median house values.

```haskell
let predictionColumn = "predicted_house_value"

let predictions =
        D.insert predictionColumn (asValue @[Float] (model trained testFeatures)) test

predictions |> D.select [F.name median_house_value, predictionColumn]
            |> D.take 10
            |> D.toMarkdown'
            |> displayMarkdown
```
