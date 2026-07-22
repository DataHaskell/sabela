{- | The siza-eval task catalogue, split out of "Eval.Task" (module-size cap):
the interactive @siza-eval@ pool of small graded tasks plus the id lookup.
-}
module Eval.TaskSet (
    tasks,
    findTask,
) where

import Data.List (find)
import Data.Text (Text)

import Eval.Task (Grader (..), Task (..))

tasks :: [Task]
tasks =
    [ Task
        "double"
        "Define a function `double :: Int -> Int` that returns twice its argument."
        (ByValue "double 21 == 42")
    , Task
        "applyTwice"
        "Define `applyTwice :: (a -> a) -> a -> a` that applies a function to a value twice."
        (ByValue "applyTwice (+3) (10 :: Int) == 16")
    , Task
        "safeDiv"
        "Define `safeDiv :: Int -> Int -> Maybe Int` returning Nothing when the divisor is 0, otherwise Just the quotient."
        (ByValue "safeDiv 10 0 == Nothing && safeDiv 10 2 == Just 5")
    , Task
        "color"
        "Define a type `Color` with constructors Red, Green and Blue, deriving Show, Eq, Enum and Bounded. Then define `allColors :: [Color]` listing every colour using minBound/maxBound."
        (ByValue "allColors == [Red, Green, Blue]")
    , Task
        "mapMaybe"
        "Define `mapMaybe' :: (a -> Maybe b) -> [a] -> [b]` that applies the function to each element and keeps only the Just results."
        ( ByValue
            "mapMaybe' (\\x -> if even x then Just (x * x) else Nothing) [1..6 :: Int] == [4, 16, 36]"
        )
    , Task
        "treeFunctor"
        "Define `data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show, Eq)` and a Functor instance for Tree that maps over the stored values."
        (ByValue "fmap (+1) (Node Leaf (1 :: Int) Leaf) == Node Leaf 2 Leaf")
    , Task
        "quarterlyBars"
        "Plot these quarterly sales figures as a bar chart and show the chart in the notebook: Q1 12, Q2 18, Q3 9, Q4 15. Use the granite plotting library."
        ByRender
    , Task
        "revenueTotal"
        "A CSV file `revenue.csv` with columns `month` and `revenue` is in the working directory. Using the dataframe library, load it into a DataFrame and define `revenueTotal :: Double` as the total revenue across all months."
        (ByValue "abs (revenueTotal - 600) < 0.001")
    , Task
        "revenueChart"
        "A CSV file `revenue.csv` with columns `month` and `revenue` is in the working directory. Using the dataframe library, load it into a DataFrame, then plot revenue by month as a bar chart with the granite library and show the chart in the notebook."
        ByRender
    , Task
        "fixBroken"
        "This function is meant to sum a list of Ints but does not compile:\n\n    total xs = foldr (+) xs\n\nFix it and define `total :: [Int] -> Int` so it returns the sum of the list."
        (ByValue "total [1, 2, 3, 4] == 10 && total [] == 0")
    ]

findTask :: Text -> Maybe Task
findTask tid = find ((== tid) . taskId) tasks
