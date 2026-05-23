# California Housing — Linear Regression with Hasktorch

This notebook is a port of the [`dataframe` CaliforniaHousing example](https://github.com/mchav/dataframe/blob/main/examples/CaliforniaHousing.hs).
We load the California housing dataset, engineer a few features with
`dataframe`, then train a linear-regression model with **Hasktorch** to predict
the median house value of a district.

## Setup



```haskell
-- cabal: build-depends: dataframe ==2.1.0.0, dataframe-hasktorch ==0.2.0.0, hasktorch, text, random
-- cabal: default-extensions: BangPatterns, NumericUnderscores, OverloadedStrings, ScopedTypeVariables, TemplateHaskell, TypeApplications

import Control.Monad (when)
import qualified Data.Text as T
import Data.Text (Text)

import qualified DataFrame as D
import qualified DataFrame.Functions as F
import DataFrame.TH
import DataFrame.Operators
import DataFrame.Hasktorch (toTensor)

import System.Random (mkStdGen)
import Torch
```



## Typed column references

`declareColumnsFromCsvWithOpts` reads the CSV header (and a sample of rows to
infer types) at compile time and generates a typed reference for every column —
`total_rooms`, `households`, `median_house_value`, and so on. We sample 300 rows
so the inferred types are accurate.



```haskell
$( declareColumnsFromCsvWithOpts (D.defaultReadOptions{D.typeSpec = D.InferFromSample 300}) "./examples/data/housing.csv" )
```



## Helper functions

`ocean_proximity` is a categorical text column; we encode it as a number so the
model can use it.



```haskell
oceanProximity :: T.Text -> Double
oceanProximity op = case op of
    "ISLAND" -> 0
    "NEAR OCEAN" -> 1
    "NEAR BAY" -> 2
    "<1H OCEAN" -> 3
    "INLAND" -> 4
    _ -> error ("Unknown ocean proximity value: " ++ T.unpack op)
```



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



The model itself is one linear layer; `squeezeAll` drops the trailing size-1
dimension so the output is a flat vector of predictions.



```haskell
model :: Linear -> Tensor -> Tensor
model state input = squeezeAll $ linear state input
```



## Loading the data



```haskell
df <- D.readCsv "./examples/data/housing.csv"

df |> D.take 5
   |> D.toMarkdown'
   |> displayMarkdown
```

> <!-- sabela:mime text/markdown -->
> | longitude<br>Double | latitude<br>Double | housing_median_age<br>Double | total_rooms<br>Double | total_bedrooms<br>Maybe Double | population<br>Double | households<br>Double | median_income<br>Double | median_house_value<br>Double | ocean_proximity<br>Text |
> | --------------------|--------------------|------------------------------|-----------------------|--------------------------------|----------------------|----------------------|-------------------------|------------------------------|------------------------ |
> | -122.23             | 37.88              | 41.0                         | 880.0                 | Just 129.0                     | 322.0                | 126.0                | 8.3252                  | 452600.0                     | NEAR BAY                |
> | -122.22             | 37.86              | 21.0                         | 7099.0                | Just 1106.0                    | 2401.0               | 1138.0               | 8.3014                  | 358500.0                     | NEAR BAY                |
> | -122.24             | 37.85              | 52.0                         | 1467.0                | Just 190.0                     | 496.0                | 177.0                | 7.2574                  | 352100.0                     | NEAR BAY                |
> | -122.25             | 37.85              | 52.0                         | 1274.0                | Just 235.0                     | 558.0                | 219.0                | 5.6431000000000004      | 341300.0                     | NEAR BAY                |
> | -122.25             | 37.85              | 52.0                         | 1627.0                | Just 280.0                     | 565.0                | 259.0                | 3.8462                  | 342200.0                     | NEAR BAY                |



## Feature ingestion and engineering

`total_bedrooms` has missing values, so we impute them with the column mean.
We also add a `rooms_per_household` feature and replace the categorical
`ocean_proximity` with its numeric encoding.



```haskell
let meanTotalBedrooms = df |> D.meanMaybe total_bedrooms

let cleaned =
        df
            |> D.impute total_bedrooms meanTotalBedrooms
            |> D.deriveMany
                [ "ocean_proximity" .= F.lift oceanProximity ocean_proximity
                , "rooms_per_household" .= total_rooms / households
                ]

cleaned |> D.take 5
        |> D.toMarkdown'
        |> displayMarkdown
```

> <!-- sabela:mime text/markdown -->
> | longitude<br>Double | latitude<br>Double | housing_median_age<br>Double | total_rooms<br>Double | total_bedrooms<br>Double | population<br>Double | households<br>Double | median_income<br>Double | median_house_value<br>Double | ocean_proximity<br>Double | rooms_per_household<br>Double |
> | --------------------|--------------------|------------------------------|-----------------------|--------------------------|----------------------|----------------------|-------------------------|------------------------------|---------------------------|------------------------------ |
> | -122.23             | 37.88              | 41.0                         | 880.0                 | 129.0                    | 322.0                | 126.0                | 8.3252                  | 452600.0                     | 2.0                       | 6.984126984126984             |
> | -122.22             | 37.86              | 21.0                         | 7099.0                | 1106.0                   | 2401.0               | 1138.0               | 8.3014                  | 358500.0                     | 2.0                       | 6.238137082601054             |
> | -122.24             | 37.85              | 52.0                         | 1467.0                | 190.0                    | 496.0                | 177.0                | 7.2574                  | 352100.0                     | 2.0                       | 8.288135593220339             |
> | -122.25             | 37.85              | 52.0                         | 1274.0                | 235.0                    | 558.0                | 219.0                | 5.6431000000000004      | 341300.0                     | 2.0                       | 5.8173515981735155            |
> | -122.25             | 37.85              | 52.0                         | 1627.0                | 280.0                    | 565.0                | 259.0                | 3.8462                  | 342200.0                     | 2.0                       | 6.281853281853282             |



## Train/test split

We hold out 20% of the districts for testing, with a fixed seed for
reproducibility.



```haskell
let (train, test) = D.randomSplit (mkStdGen 42) 0.8 cleaned

(D.dimensions train, D.dimensions test)
```

> <!-- sabela:mime text/plain -->
> ((16408,11),(4232,11))



## Building the tensors

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



## Training the model

We initialise a linear layer with one output, then run 1,000 gradient-descent
steps minimising mean-squared error, logging the loss every 100 iterations.
(The original program runs 100,000 steps; 1,000 is plenty to watch the loss
fall and keeps the notebook responsive — bump `foldLoop init 1_000` higher for
a better fit.)



```haskell
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

> <!-- sabela:mime text/plain -->
> Training linear regression model...
> Iteration: 100 | Loss: Tensor Float []  8.5483e9   
> Iteration: 200 | Loss: Tensor Float []  6.8199e9   
> Iteration: 300 | Loss: Tensor Float []  6.1524e9   
> Iteration: 400 | Loss: Tensor Float []  5.8438e9   
> Iteration: 500 | Loss: Tensor Float []  5.6812e9   
> Iteration: 600 | Loss: Tensor Float []  5.5868e9   
> Iteration: 700 | Loss: Tensor Float []  5.5275e9   
> Iteration: 800 | Loss: Tensor Float []  5.4874e9   
> Iteration: 900 | Loss: Tensor Float []  5.4582e9   
> Iteration: 1000 | Loss: Tensor Float []  5.4355e9



## Predictions

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

> <!-- sabela:mime text/markdown -->
> | median_house_value<br>Double | predicted_house_value<br>Float |
> | -----------------------------|------------------------------- |
> | 452600.0                     | 426711.56                      |
> | 352100.0                     | 402954.13                      |
> | 341300.0                     | 341709.56                      |
> | 213500.0                     | 249442.75                      |
> | 147500.0                     | 160755.23                      |
> | 99700.0                      | 212119.5                       |
> | 93800.0                      | 216513.4                       |
> | 97200.0                      | 180216.56                      |
> | 188800.0                     | 276951.88                      |
> | 187500.0                     | 145220.22                      |


