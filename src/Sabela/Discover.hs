{-# LANGUAGE OverloadedStrings #-}

module Sabela.Discover (
    LibSuggestion (..),
    topics,
    discoverLibraries,
    cabalLine,
) where

import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T

data LibSuggestion = LibSuggestion
    { lsPackages :: [Text]
    , lsFor :: Text
    , lsModules :: [Text]
    }
    deriving (Eq, Show)

-- | The @-- cabal:@ line a suggestion implies, ready to paste as a cell's first line.
cabalLine :: LibSuggestion -> Text
cabalLine s = "-- cabal: build-depends: " <> T.intercalate ", " (lsPackages s)

topics :: [([Text], LibSuggestion)]
topics =
    [
        (
            [ "regression"
            , "linear model"
            , "logistic"
            , "train"
            , "fit"
            , "predict"
            , "machine learning"
            , "ml"
            , "ols"
            , "classif"
            ]
        , LibSuggestion
            ["dataframe", "dataframe-learn"]
            "fitting models (linear/logistic regression) over a DataFrame with fit/predict"
            ["DataFrame", "DataFrame.Operators", "DataFrame.LinearModel", "DataFrame.Model"]
        )
    ,
        (
            [ "csv"
            , "read csv"
            , "load data"
            , "parse"
            , "tsv"
            , "delimited"
            , "read a file"
            , "dataset"
            ]
        , LibSuggestion
            ["dataframe"]
            "loading tabular data with D.readCsv; for compile-checked column access, declareColumns binds each CSV column as a typed value (find_examples \"typed column\")"
            ["DataFrame"]
        )
    ,
        (
            [ "plot"
            , "chart"
            , "plotting"
            , "bar"
            , "scatter"
            , "line graph"
            , "pie"
            , "visuali"
            , "graph"
            , "svg"
            , "render"
            ]
        , LibSuggestion
            ["granite", "text"]
            "rendering charts (bars/lineGraph/scatter/pie) to SVG, shown with displaySvg"
            ["Granite.Svg"]
        )
    ,
        (
            [ "dataframe"
            , "table"
            , "column"
            , "aggregate"
            , "group by"
            , "join"
            , "select"
            , "filter rows"
            , "typed"
            , "schema"
            , "column names"
            , "type-safe"
            ]
        , LibSuggestion
            ["dataframe", "dataframe-th", "template-haskell"]
            "DataFrames with compile-checked columns: $(declareColumns =<< runIO (D.readCsv ...)) reads the header at compile time and binds each column as a typed value, so a wrong column name is a compile error"
            ["DataFrame", "DataFrame.TH.Records"]
        )
    ]

discoverLibraries :: Text -> [LibSuggestion]
discoverLibraries query =
    map snd $
        sortOn (Down . fst) $
            filter ((> 0) . fst) [(score kws, sug) | (kws, sug) <- topics]
  where
    q = T.toLower query
    score = length . filter (`T.isInfixOf` q)
