# Iris Classification with a Decision Tree

This notebook is a port of the [`dataframe` Iris example](https://github.com/mchav/dataframe/blob/main/examples/Iris.ipynb).
We use the internal decision tre module instead of Hasktorch.

## Setup

We depend on `dataframe` (the dataframe library), `dataframe-learn` (the decision
tree), `text` (for the label column type) and `random` (for a reproducible
train/test split).


```haskell
-- cabal: build-depends: dataframe, dataframe-learn, text, random
-- cabal: default-extensions: OverloadedStrings, TypeApplications, ScopedTypeVariables

import qualified DataFrame as D
import qualified DataFrame.Functions as F
import DataFrame.Operators
import qualified DataFrame.DecisionTree as DT
import qualified Data.Text as T
import System.Random (mkStdGen)
```


## Loading the data

The classic Iris dataset ships as a small Parquet file. Each row is one flower
with four `Double` measurements and a `variety` label.


```haskell
df <- D.readParquet "./examples/data/iris.parquet"

df |> D.take 5
   |> D.toMarkdown'
   |> displayMarkdown
```

> <!-- sabela:mime text/markdown -->
> | sepal.length<br>Double | sepal.width<br>Double | petal.length<br>Double | petal.width<br>Double | variety<br>Text |
> | -----------------------|-----------------------|------------------------|-----------------------|---------------- |
> | 5.1                    | 3.5                   | 1.4                    | 0.2                   | Setosa          |
> | 4.9                    | 3.0                   | 1.4                    | 0.2                   | Setosa          |
> | 4.7                    | 3.2                   | 1.3                    | 0.2                   | Setosa          |
> | 4.6                    | 3.1                   | 1.5                    | 0.2                   | Setosa          |
> | 5.0                    | 3.6                   | 1.4                    | 0.2                   | Setosa          |


## Looking at the data

Before modelling, it helps to know the shape and types of every column.


```haskell
df |> D.describeColumns
   |> D.toMarkdown'
   |> displayMarkdown
```

> <!-- sabela:mime text/markdown -->
> | Column Name<br>Text | # Non-null Values<br>Int | # Null Values<br>Int | Type<br>Text |
> | --------------------|--------------------------|----------------------|------------- |
> | variety             | 150                      | 0                    | Text         |
> | petal.width         | 150                      | 0                    | Double       |
> | petal.length        | 150                      | 0                    | Double       |
> | sepal.width         | 150                      | 0                    | Double       |
> | sepal.length        | 150                      | 0                    | Double       |


A summary of the numeric columns shows the ranges each measurement spans.


```haskell
df |> D.summarize
   |> D.toMarkdown'
   |> displayMarkdown
```

> <!-- sabela:mime text/markdown -->
> | Statistic<br>Text | sepal.length<br>Double | sepal.width<br>Double | petal.length<br>Double | petal.width<br>Double |
> | ------------------|------------------------|-----------------------|------------------------|---------------------- |
> | Count             | 150.0                  | 150.0                 | 150.0                  | 150.0                 |
> | Mean              | 5.84                   | 3.06                  | 3.76                   | 1.2                   |
> | Minimum           | 4.3                    | 2.0                   | 1.0                    | 0.1                   |
> | 25%               | 5.1                    | 2.8                   | 1.6                    | 0.3                   |
> | Median            | 5.8                    | 3.0                   | 4.35                   | 1.3                   |
> | 75%               | 6.4                    | 3.3                   | 5.1                    | 1.8                   |
> | Max               | 7.9                    | 4.4                   | 6.9                    | 2.5                   |
> | StdDev            | 0.83                   | 0.44                  | 1.77                   | 0.76                  |
> | IQR               | 1.3                    | 0.5                   | 3.5                    | 1.5                   |
> | Skewness          | 0.31                   | 0.31                  | -0.27                  | -0.1                  |


## Is the dataset balanced?

A severely imbalanced dataset would make accuracy a misleading metric. The Iris
dataset is famously balanced — 50 of each species — which we can confirm with
`frequencies`.


```haskell
df |> D.frequencies (F.col @T.Text "variety")
   |> D.toMarkdown'
   |> displayMarkdown
```

> <!-- sabela:mime text/markdown -->
> | Statistic<br>Text | Setosa<br>Any | Versicolor<br>Any | Virginica<br>Any |
> | ------------------|---------------|-------------------|----------------- |
> | Count             | 50            | 50                | 50               |
> | Percentage (%)    | 33.33%        | 33.33%            | 33.33%           |


## Splitting into training and test sets

`randomSplit` is the equivalent of scikit-learn's `train_test_split`. We hold
out 30% of the data for testing and fix the random seed (42) so the split is
reproducible.


```haskell
let (trainDf, testDf) = D.randomSplit (mkStdGen 42) 0.7 df

(D.dimensions trainDf, D.dimensions testDf)
```

> <!-- sabela:mime text/plain -->
> ((104,5),(46,5))


## Fitting the decision tree

This is the whole "training" step. `fitDecisionTree` takes a configuration, the
target column (`variety`, a `Text` label), and the training frame. It returns an
`Expr Text`: a self-contained expression that predicts the species from the
other columns. Everything else in the frame is treated as a candidate feature.


```haskell
let model = DT.fitDecisionTree DT.defaultTreeConfig (F.col @T.Text "variety") trainDf
```


The fitted model is just data — a tree of `if`/`then`/`else` rules over the
measurement columns. Unlike the neural network's weight matrices, you can read
it directly and see exactly how the tree decides.


```haskell
putStrLn $ D.prettyPrint model
```

> <!-- sabela:mime text/plain -->
> if petal.width .> 0.4
>   then if petal.width .< 1.8
>     then if petal.length .> 4.9
>       then "Virginica"
>       else if sepal.length .< 5.0
>         then "Virginica"
>         else "Versicolor"
>     else "Virginica"
>   else "Setosa"


## Making predictions

Because the model is an expression, we apply it to the test frame the same way
we'd add any derived column: with `D.derive`. Here we add a `predicted` column
next to the true `variety`.


```haskell
let scored = testDf |> D.derive "predicted" model

scored |> D.select ["variety", "predicted"]
       |> D.take 10
       |> D.toMarkdown'
       |> displayMarkdown
```

> <!-- sabela:mime text/markdown -->
> | variety<br>Text | predicted<br>Text |
> | ----------------|------------------ |
> | Setosa          | Setosa            |
> | Setosa          | Setosa            |
> | Setosa          | Setosa            |
> | Setosa          | Setosa            |
> | Setosa          | Setosa            |
> | Setosa          | Setosa            |
> | Setosa          | Versicolor        |
> | Setosa          | Setosa            |
> | Setosa          | Setosa            |
> | Setosa          | Setosa            |


## Measuring accuracy

We mark each test row as correct (1.0) or wrong (0.0) by comparing the true and
predicted labels with `F.eq`, then take the mean — that mean is the accuracy.


```haskell
let withCorrect =
        scored
            |> D.derive
                "is_correct"
                ( F.ifThenElse
                    (F.eq (F.col @T.Text "variety") (F.col @T.Text "predicted"))
                    (F.lit (1.0 :: Double))
                    (F.lit (0.0 :: Double))
                )

putStrLn ("Test accuracy: " ++ show (D.mean (F.col @Double "is_correct") withCorrect))
```

> <!-- sabela:mime text/plain -->
> Test accuracy: 0.8695652173913043


## Confusion matrix

The original example built a confusion matrix by hand. With a `DataFrame` we get
it by grouping on the (actual, predicted) pair and counting. The diagonal
(`variety == predicted`) holds the correct predictions; off-diagonal entries are
the mistakes.


```haskell
scored |> D.groupBy ["variety", "predicted"]
       |> D.aggregate ["count" .= F.count (F.col @T.Text "variety")]
       |> D.sortBy [D.Asc "variety", D.Asc "predicted"]
       |> D.toMarkdown'
       |> displayMarkdown
```

> <!-- sabela:mime text/markdown -->
> | variety<br>Text | predicted<br>Text | count<br>Int |
> | ----------------|-------------------|------------- |
> | Versicolor      | Versicolor        | 14           |
> | Virginica       | Virginica         | 14           |
> | Setosa          | Versicolor        | 2            |
> | Setosa          | Setosa            | 12           |
> | Versicolor      | Virginica         | 4            |


## Conclusion

Decision trees ,on a clean, well-separated
dataset like Iris, are pretty easy and efficient to run.
