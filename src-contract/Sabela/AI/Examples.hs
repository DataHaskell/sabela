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

{- | One runnable example: search 'exTags', a one-line 'exTitle', and the full
'exCode' cell (with its @-- cabal:@ first line) the model can paste and adapt.
-}
data Example = Example
    { exTags :: [Text]
    , exTitle :: Text
    , exCode :: Text
    }
    deriving (Eq, Show)

-- | The curated examples. Extend here; the prompt does not change.
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
    ]

{- | Examples matching a free-text query, best match first. An example's score is
the number of its tags that occur in the (lower-cased) query, so "csv cassava"
ranks the cassava example over the plain dataframe one. Empty when nothing
matches.
-}
searchExamples :: Text -> [Example]
searchExamples query =
    map snd $
        sortOn (Down . fst) $
            filter ((> 0) . fst) [(score (exTags e), e) | e <- exampleIndex]
  where
    q = T.toLower query
    score = length . filter (`T.isInfixOf` q)
