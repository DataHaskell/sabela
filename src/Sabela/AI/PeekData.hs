{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.PeekData (
    PeekResult (..),
    ColType (..),
    peekData,
    peekResultJSON,
    colTypeName,
) where

import Data.Aeson (Value, object, toJSON, (.=))
import Data.Char (isDigit)
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR

data ColType = ColInt | ColDouble | ColBool | ColText
    deriving (Eq, Show)

colTypeName :: ColType -> Text
colTypeName ColInt = "Int"
colTypeName ColDouble = "Double"
colTypeName ColBool = "Bool"
colTypeName ColText = "Text"

data PeekResult = PeekResult
    { peekDelimiter :: Text
    , peekHasHeader :: Bool
    , peekHeader :: [Text]
    , peekColTypes :: [ColType]
    , peekRows :: [[Text]]
    }
    deriving (Eq, Show)

candidateDelims :: [Text]
candidateDelims = [",", "\t", ";", "|"]

peekData :: Int -> Text -> PeekResult
peekData n raw =
    PeekResult
        { peekDelimiter = delim
        , peekHasHeader = hasHeader
        , peekHeader = header
        , peekColTypes = colTypes
        , peekRows = take (max 0 n) body
        }
  where
    nonEmpty = filter (not . T.null) (T.lines raw)
    delim = inferDelimiter nonEmpty
    rows = map (T.splitOn delim) nonEmpty
    hasHeader = looksLikeHeader rows
    (headerRow, bodyRows) = case rows of
        (h : rest) | hasHeader -> (h, rest)
        _ -> ([], rows)
    width = case rows of
        (r : _) -> length r
        [] -> 0
    header
        | hasHeader = headerRow
        | otherwise = [T.pack ("col" <> show i) | i <- [1 .. width]]
    body = bodyRows
    colTypes = [guessColType (columnAt i body) | i <- [0 .. width - 1]]

columnAt :: Int -> [[Text]] -> [Text]
columnAt i = concatMap (take 1 . drop i)

inferDelimiter :: [Text] -> Text
inferDelimiter [] = ","
inferDelimiter ls = fst (maximumBy (comparing snd) scored)
  where
    scored = [(d, score d) | d <- reverse candidateDelims]
    score d =
        let counts = map (length . T.splitOn d) ls
            modeCount = mode counts
         in modeCount * length (filter (== modeCount) counts)

mode :: (Eq a) => [a] -> a
mode [] = error "mode: empty list"
mode xs = snd (maximumBy (comparing fst) [(length (filter (== x) xs), x) | x <- xs])

looksLikeHeader :: [[Text]] -> Bool
looksLikeHeader (h : body@(_ : _)) =
    or
        [ not (isNumericCell hi) && all isNumericCell (columnAt i body)
        | (i, hi) <- zip [0 ..] h
        , not (null (columnAt i body))
        ]
looksLikeHeader _ = False

isNumericCell :: Text -> Bool
isNumericCell = (`elem` [ColInt, ColDouble]) . classifyCell

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

isInt :: Text -> Bool
isInt t = case TR.signed TR.decimal t :: Either String (Integer, Text) of
    Right (_, rest) -> T.null rest && not (T.null t) && T.all digitOrSign t
    _ -> False
  where
    digitOrSign c = isDigit c || c == '-' || c == '+'

isDouble :: Text -> Bool
isDouble t = case TR.signed TR.double t of
    Right (_, rest) -> T.null rest && not (T.null t)
    _ -> False

peekResultJSON :: PeekResult -> Value
peekResultJSON r =
    object
        [ "delimiter" .= peekDelimiter r
        , "hasHeader" .= peekHasHeader r
        , "columns" .= columns
        , "rows" .= peekRows r
        ]
  where
    columns =
        toJSON
            [ object ["name" .= name, "type" .= colTypeName ty]
            | (name, ty) <- zip (peekHeader r) (peekColTypes r)
            ]
