{-# LANGUAGE OverloadedStrings #-}

{- | Summarises a sample of an artefact as a delimited table, but only when
the sample is one. The verdict carries its evidence, and a column is named
only from a header the file really has.
-}
module Sabela.AI.PeekData (
    PeekResult (..),
    PeekVerdict (..),
    DelimitedView (..),
    PeekColumn (..),
    ColType (..),
    peekData,
    peekResultJSON,
    colTypeName,
) where

import Data.Aeson (Value (..), object, toJSON, (.=))
import Data.Aeson.Types (Pair)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Shape (
    Grid (..),
    binaryish,
    columnAt,
    gridDelimiters,
    gridHeader,
    gridOf,
    isDouble,
    isInt,
 )

data ColType = ColInt | ColDouble | ColBool | ColText
    deriving (Eq, Show)

colTypeName :: ColType -> Text
colTypeName ColInt = "Int"
colTypeName ColDouble = "Double"
colTypeName ColBool = "Bool"
colTypeName ColText = "Text"

{- | A column of a delimited sample. @pcName@ is 'Nothing' when the file has
no header row: the reader has an index for that column and nothing else.
-}
data PeekColumn = PeekColumn
    { pcIndex :: Int
    , pcName :: Maybe Text
    , pcType :: ColType
    }
    deriving (Eq, Show)

data DelimitedView = DelimitedView
    { dvDelimiter :: Text
    , dvHasHeader :: Bool
    , dvColumns :: [PeekColumn]
    , dvRows :: [[Text]]
    , dvRowCount :: Int
    }
    deriving (Eq, Show)

data PeekVerdict = NotDelimited Text | Delimited DelimitedView
    deriving (Eq, Show)

data PeekResult = PeekResult
    { peekVerdict :: PeekVerdict
    , peekLineCount :: Int
    }
    deriving (Eq, Show)

-- | Reads at most @n@ data rows out of the sample.
peekData :: Int -> Text -> PeekResult
peekData n raw = PeekResult (verdictOf n raw) (length nonEmpty)
  where
    nonEmpty = filter (not . T.null) (T.lines raw)

verdictOf :: Int -> Text -> PeekVerdict
verdictOf n raw
    | binaryish raw = NotDelimited notText
    | length nonEmpty < 2 = NotDelimited tooFewLines
    | otherwise =
        maybe
            (NotDelimited noConsistentDelimiter)
            (viewOf n)
            (gridOf gridDelimiters nonEmpty)
  where
    nonEmpty = filter (not . T.null) (T.lines raw)

notText :: Text
notText = "the sample is not decodable text, so it was not read as a table"

tooFewLines :: Text
tooFewLines = "fewer than two non-empty lines, which is not enough to check a delimiter"

noConsistentDelimiter :: Text
noConsistentDelimiter =
    "no candidate delimiter splits every line into the same number of at least \
    \two fields"

viewOf :: Int -> Grid -> PeekVerdict
viewOf n g =
    Delimited
        DelimitedView
            { dvDelimiter = gridDelim g
            , dvHasHeader = hasHeader
            , dvColumns = columns
            , dvRows = take (max 0 n) body
            , dvRowCount = length body
            }
  where
    rows = gridRows g
    header = gridHeader g
    hasHeader = isJust header
    (headerRow, body) = case header of
        Just h -> (map Just h, drop 1 rows)
        Nothing -> (replicate (gridWidth g) Nothing, rows)
    columns =
        [ PeekColumn i nm (guessColType (columnAt i body))
        | (i, nm) <- zip [0 ..] headerRow
        ]

guessColType :: [Text] -> ColType
guessColType cells =
    case filter (not . T.null) (map T.strip cells) of
        [] -> ColText
        nonBlank -> foldr1 unify (map classifyCell nonBlank)

unify :: ColType -> ColType -> ColType
unify a b
    | a == b = a
    | ColText `elem` [a, b] = ColText
    | all (`elem` [ColInt, ColDouble]) [a, b] = ColDouble
    | otherwise = ColText

classifyCell :: Text -> ColType
classifyCell raw
    | T.toLower t `elem` ["true", "false"] = ColBool
    | isInt t = ColInt
    | isDouble t = ColDouble
    | otherwise = ColText
  where
    t = T.strip raw

peekResultJSON :: PeekResult -> Value
peekResultJSON r = object (["lines" .= peekLineCount r] <> verdictPairs (peekVerdict r))

verdictPairs :: PeekVerdict -> [Pair]
verdictPairs (NotDelimited reason) =
    ["verdict" .= ("not-delimited" :: Text), "reason" .= reason]
verdictPairs (Delimited v) =
    [ "verdict" .= ("delimited" :: Text)
    , "delimiter" .= dvDelimiter v
    , "hasHeader" .= dvHasHeader v
    , "fieldsPerLine" .= length (dvColumns v)
    , "columns" .= toJSON (map columnJSON (dvColumns v))
    , "rows" .= dvRows v
    , "rowCount" .= dvRowCount v
    ]

{- | A column carries the index it was read at always, and a name only when
one was read from the file.
-}
columnJSON :: PeekColumn -> Value
columnJSON c =
    object
        [ "index" .= pcIndex c
        , "name" .= maybe Null String (pcName c)
        , "type" .= colTypeName (pcType c)
        ]
