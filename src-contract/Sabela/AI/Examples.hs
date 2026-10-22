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
        "Bar chart with the chart language"
        ( T.unlines
            [ "-- cabal: build-depends: dataframe, text"
            , "-- cabal: default-extensions: DataKinds, TemplateHaskell, TypeApplications, OverloadedStrings"
            , "import qualified DataFrame as D"
            , "import qualified DataFrame.Typed as DT"
            , "import qualified DataFrame.Display.Web.Chart.Typed as C"
            , "import DataFrame.Typed.Freeze (freezeOrThrow)"
            , "import DataFrame ((|>))"
            , ""
            , "-- A frame, its schema derived by the compiler, frozen to it:"
            , "sales0 = D.fromNamedColumns [(\"quarter\", D.fromList [\"Q1\", \"Q2\", \"Q3\" :: Text]), (\"units\", D.fromList [12, 18, 9 :: Int])]"
            , "$(DT.deriveSchema \"Sales\" sales0)"
            , "sales <- freezeOrThrow @Sales sales0"
            , ""
            , "C.chart sales |> C.mark C.Bar |> C.enc C.X (DT.col @\"quarter\") |> C.enc C.Y (DT.col @\"units\") |> C.title \"Sales\""
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
        "Points, a line over them, or a histogram"
        ( T.unlines
            [ "-- cabal: build-depends: dataframe, text"
            , "-- cabal: default-extensions: DataKinds, TemplateHaskell, TypeApplications, OverloadedStrings"
            , "import qualified DataFrame as D"
            , "import qualified DataFrame.Typed as DT"
            , "import qualified DataFrame.Display.Web.Chart.Typed as C"
            , "import DataFrame.Typed.Freeze (freezeOrThrow)"
            , "import DataFrame ((|>))"
            , ""
            , "rows0 = D.fromNamedColumns [(\"x\", D.fromList [1 .. 20 :: Int]), (\"y\", D.fromList [i * i | i <- [1 .. 20 :: Int]])]"
            , "$(DT.deriveSchema \"Rows\" rows0)"
            , "rows <- freezeOrThrow @Rows rows0"
            , ""
            , "-- Points with a line layered over them:"
            , "C.layer [C.chart rows |> C.mark C.Point |> C.enc C.X (DT.col @\"x\") |> C.enc C.Y (DT.col @\"y\"), C.chart rows |> C.mark C.Line |> C.enc C.X (DT.col @\"x\") |> C.enc C.Y (DT.col @\"y\")]"
            , "-- A histogram bins a number and counts rows:"
            , "-- C.chart rows |> C.mark C.Bar |> C.enc C.X (DT.col @\"y\") |> C.binX |> C.aggregateOn C.Y C.Count"
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
