# examples/typed


```haskell
-- cabal: build-depends: dataframe, text

import qualified DataFrame as D
import qualified DataFrame.Functions as F
import qualified DataFrame.Typed as DT
```


```haskell
import DataFrame.Operators

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


```haskell
df |> D.describeColumns
   |> D.toMarkdown'
   |> displayMarkdown
```

> <!-- sabela:mime text/markdown -->
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


```haskell
:set -XDataKinds
:set -XTemplateHaskell
:set -XTypeApplications

$(DT.deriveSchema "Housing" df)

tdf = either (error . show) id (DT.freezeWithError @Housing df)
```


```haskell
tdf |> DT.take 5
    |> DT.thaw
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


```haskell
:t DT.filter
```

> <!-- sabela:mime text/plain -->
> DT.filter
>   :: DataFrame.Internal.Column.Columnable a =>
>      DT.TExpr cols a
>      -> (a -> Bool) -> DT.TypedDataFrame cols -> DT.TypedDataFrame cols


```haskell
tdf |> DT.take 5
    |> DT.derive @"rooms_per_household" (DT.col @"total_rooms" / DT.col @"households")
    |> DT.filter (DT.col @"rooms_per_household") (>= 5)
    |> DT.thaw
    |> D.toMarkdown'
    |> displayMarkdown
```

> <!-- sabela:mime text/markdown -->
> | longitude<br>Double | latitude<br>Double | housing_median_age<br>Double | total_rooms<br>Double | total_bedrooms<br>Maybe Double | population<br>Double | households<br>Double | median_income<br>Double | median_house_value<br>Double | ocean_proximity<br>Text | rooms_per_household<br>Double |
> | --------------------|--------------------|------------------------------|-----------------------|--------------------------------|----------------------|----------------------|-------------------------|------------------------------|-------------------------|------------------------------ |
> | -122.23             | 37.88              | 41.0                         | 880.0                 | Just 129.0                     | 322.0                | 126.0                | 8.3252                  | 452600.0                     | NEAR BAY                | 6.984126984126984             |
> | -122.22             | 37.86              | 21.0                         | 7099.0                | Just 1106.0                    | 2401.0               | 1138.0               | 8.3014                  | 358500.0                     | NEAR BAY                | 6.238137082601054             |
> | -122.24             | 37.85              | 52.0                         | 1467.0                | Just 190.0                     | 496.0                | 177.0                | 7.2574                  | 352100.0                     | NEAR BAY                | 8.288135593220339             |
> | -122.25             | 37.85              | 52.0                         | 1274.0                | Just 235.0                     | 558.0                | 219.0                | 5.6431000000000004      | 341300.0                     | NEAR BAY                | 5.8173515981735155            |
> | -122.25             | 37.85              | 52.0                         | 1627.0                | Just 280.0                     | 565.0                | 259.0                | 3.8462                  | 342200.0                     | NEAR BAY                | 6.281853281853282             |

