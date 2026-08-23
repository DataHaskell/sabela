{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Examples (
    Example (..),
    exampleIndex,
    searchExamples,
) where

import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T

data Example = Example
    { exTags :: [Text]
    , exTitle :: Text
    , exCode :: Text
    }
    deriving (Eq, Show)

exampleIndex :: [Example]
exampleIndex =
    [ Example
        ["csv", "read", "load", "dataframe", "parse", "data"]
        "Read a CSV into a DataFrame"
        ( T.unlines
            [ "-- cabal: build-depends: dataframe"
            , "{-# LANGUAGE OverloadedStrings #-}"
            , "import qualified DataFrame as D"
            , ""
            , "df <- D.readCsv \"data.csv\""
            , "df"
            ]
        )
    , Example
        [ "column"
        , "columns"
        , "value"
        , "sum"
        , "total"
        , "aggregate"
        , "mean"
        , "average"
        , "typed"
        , "schema"
        , "safe"
        , "select"
        , "access"
        , "dataframe"
        , "header"
        , "names"
        ]
        "Total a DataFrame column with compile-checked names (typed)"
        ( T.unlines
            [ "-- cabal: build-depends: dataframe, dataframe-th, template-haskell"
            , "{-# LANGUAGE TemplateHaskell #-}"
            , "import qualified DataFrame as D"
            , ""
            , "-- Reads data.csv at compile time and binds each column header as a typed"
            , "-- value; refer to a column by its header (here `value`). A wrong name is"
            , "-- a compile error, with GHC suggesting the closest real column."
            , "df <- D.readCsv \"data.csv\""
            , "$(D.declareColumns df)"
            , "total = sum (D.columnAsList value df)"
            , "total"
            ]
        )
    , Example
        [ "bar"
        , "bars"
        , "chart"
        , "plot"
        , "graph"
        , "draw"
        , "picture"
        , "svg"
        , "visualise"
        , "visualize"
        ]
        "Draw a bar chart of labelled values"
        ( T.unlines
            [ "import Sabela.Notebook"
            , ""
            , "-- barChart takes the pairs you already have; displayPicture shows it."
            , "displayPicture (barChart [(\"Q1\", 12), (\"Q2\", 18), (\"Q3\", 9)])"
            ]
        )
    , Example
        [ "scatter"
        , "points"
        , "line chart"
        , "line graph"
        , "histogram"
        , "distribution"
        ]
        "Draw points, a line, or a distribution"
        ( T.unlines
            [ "import Sabela.Notebook"
            , ""
            , "displayPicture (scatterChart [(1, 2), (2, 4), (3, 3)])"
            , "displayPicture (lineChart [(1, 2), (2, 4), (3, 3)])"
            , "displayPicture (histogram 10 [1, 1, 2, 3, 5, 8, 13])"
            ]
        )
    , Example
        ["widget", "button", "counter", "click", "press", "interactive"]
        "A widget with a button that counts presses"
        ( T.unlines
            [ "import Sabela.Notebook.Widget"
            , "import Sabela.Notebook.Widget.Kit (counter)"
            , ""
            , "-- The kit has whole widgets; htmlWidget mounts one in this cell."
            , "presses <- mkWidget (htmlWidget \"presses\" (renderWidget (counter \"presses\" 0)))"
            , "presses"
            ]
        )
    , Example
        [ "widget"
        , "custom"
        , "slider"
        , "checkbox"
        , "dropdown"
        , "form"
        , "interactive"
        , "frp"
        , "event"
        ]
        "Write a widget of your own"
        ( T.unlines
            [ "import Sabela.Notebook.Widget"
            , "import Sabela.Notebook.Event (accumB, mapE)"
            , ""
            , "-- Each control hands back the Event of its own uses; fold that into state."
            , "tally :: Ui Int"
            , "tally = do"
            , "  up <- pushButton \"+\""
            , "  n  <- sample (accumB (0 :: Int) (mapE (const (+ 1)) up))"
            , "  say (\"count: \" ++ show n)"
            , "  pure n"
            , ""
            , "count <- mkWidget (htmlWidget \"tally\" (renderWidget tally))"
            , "count"
            ]
        )
    ]

searchExamples :: Text -> [Example]
searchExamples query =
    map snd $
        sortOn (Down . fst) $
            filter ((> 0) . fst) [(score (exTags e), e) | e <- exampleIndex]
  where
    q = T.toLower query
    score = length . filter (`T.isInfixOf` q)
