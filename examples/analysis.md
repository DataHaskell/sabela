# California housing dataset

We'll be re-doing the example from chapter 2 of Hands-on machine learning with Scikit-Learn, Keras & TensorFlow. The text is heavily derived from that book and included here purely for didactive purposes.


## Loading the data

The data is already in our root directory. We'll use the readCsv function to read the data into a dataframe.

```haskell
-- cabal: build-depends: dataframe, text
-- cabal: default-extensions: TemplateHaskell, TypeApplications, OverloadedStrings

import qualified DataFrame as D
import Data.Text (Text)

df <- D.readCsv "./examples/data/housing.csv"
```

## Take a Quick Look at the Data Structure

Let’s take a look at the top five rows using the DataFrame’s take function.

```haskell
import qualified Data.Text as T
import DataFrame ((|>))

df |> D.take 5
   |> D.toMarkdownTable
   |> T.unpack
   |> displayMarkdown
```
Each row represents one district. There are 10 attributes: longitude, latitude, housing_median_age, total_rooms, total_bedrooms, population, households, median_income, median_house_value, and ocean_proximity.

We can already tell from the types that total_bedrooms will have null values since it's type is Maybe Double. How many nulls does it have? We can get a summary of all rows using the describeColumns function.

```haskell
df |> D.describeColumns
   |> D.toMarkdownTable
   |> T.unpack
   |> displayMarkdown
```

Let’s look at the other fields. The summarize function shows a summary of the numerical attributes

```haskell
df |> D.summarize
   |> D.toMarkdownTable
   |> T.unpack
   |> displayMarkdown
```

We can also get summaries of categorical columns.

```haskell
df |> D.frequencies "ocean_proximity"
   |> D.toMarkdownTable
   |> T.unpack
   |> displayMarkdown
```

The count, mean, mininum, and max rows are self-explanatory. The StdDev row shows the standard deviation, which measures how dispersed the values are. The 25%, 50%, and 75% rows show the corresponding percentiles: a percentile indicates the value below which a given percentage of observations in a group of observations falls.

For example, 25% of the districts have a housing_median_age lower than 18, while 50% are lower than 29 and 75% are lower than 37. These are often called the 25th percentile (or 1st quartile), the median, and the 75th percentile (or 3rd quartile).

Another quick way to get a feel of the type of data you are dealing with is to plot a histogram for each numerical attribute.


```haskell

import qualified DataFrame.Display.Web.Plot as Plt
import DataFrame ((|>))

-- We can explicitly filter out numeric columns. `selectBy` lets us filter columns by
-- any of the below:
--    * column name
--    * column index range
--    * column property (isNumeric, isOptional) etc.
--    * column name range (similar to dplyr)

-- plotAllHistograms already skips non-numeric columns using similar logic.
-- We include this API here purely for demonstration.
Plt.HtmlPlot p <- Plt.plotAllHistograms (df |> D.selectBy [D.byProperty D.isNumeric])

displayHtml (T.unpack p)
```

One last thing you may want to do before actually preparing the data for Machine Learning algorithms is to try out various attribute combinations. For example, the total number of rooms in a district is not very useful if you don’t know how many households there are. What you really want is the number of rooms per household. Similarly, the total number of bedrooms by itself is not very useful: you probably want to compare it to the number of rooms. And the population per household also seems like an interesting attribute combination to look at.

We can reference existing attributes in type-safe expressions by automatically creating references to them. We use the declareColumns function from DataFrame.Functions. This creates global references to each of the columns.

```haskell
import qualified DataFrame.Functions as F

$(F.declareColumns df)
```

```haskell
priceBucket :: Double -> Text
priceBucket p
    | p > 500000 = ">500'000"
    | p > 450000 = ">450'000"
    | p > 400000 = ">400'000"
    | p > 300000 = ">300'000"
    | p > 200000 = ">200'000"
    | otherwise  = "<=200'000"

withPriceBuckets = df
                 |> D.derive "price_bucket" (F.lift priceBucket median_house_value)
                 |> D.sortBy [D.Desc (F.name median_house_value)]

-- We use the `name` function to get the text that our column refers to.
-- Saves us from having typos is the code.
Plt.HtmlPlot scatter <- Plt.plotScatterBy (F.name longitude) (F.name latitude) "price_bucket" withPriceBuckets

displayHtml (T.unpack scatter)
```

From the plot above we can see that there are specific population centers where house prices are high. This tells us that a really good model should be able to account for this non-linearity. The expensive population centers are coastal suggesting that the ocean_proximity will be useful for predictiting house prices.

Now let's declare these attributes.

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
augmented = df |> D.derive "rooms_per_household" (total_rooms / households)
               |> D.derive "total_bedrooms" (F.fromMaybe meanTotalBedrooms total_bedrooms)
               |> D.derive "bedrooms_per_household" (F.col "total_bedrooms" / total_rooms)
               |> D.derive "population_per_household" (population / households)
               |> D.derive "ocean_proximity" (F.lift ordinalOceanProximity ocean_proximity)

```

Now let's see how well these variables correlate with our target.

```haskell
import qualified Data.List as L
import Data.Function
import Data.Maybe (fromMaybe)

correlationWithHouseValue columnName = (columnName, fromMaybe 0 (D.correlation columnName "median_house_value" augmented))

correlations = map correlationWithHouseValue (D.columnNames augmented)

mapM_ print (L.sortBy (flip compare `on` snd) correlations)
```

Hey, not bad! The new bedrooms_per_room attribute is much more correlated with the median house value than the total number of rooms or bedrooms. Apparently houses with a lower bedroom/room ratio tend to be more expensive. The number of rooms per household is also more informative than the total number of rooms in a district—obviously the larger the houses, the more expensive they are.

Also, once we translate ocean proximity to an ordinal value 5 means close the ocean and 1 means far from the ocean we see how predictive the features is.
