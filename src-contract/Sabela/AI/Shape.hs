{-# LANGUAGE OverloadedStrings #-}

{- | The shape of a rendering, read from the characters alone: a delimited
grid, a bracketed collection, or neither. Shared by the artefact summariser
and the value echo, so neither keys on a type name or a file format.
-}
module Sabela.AI.Shape (
    Grid (..),
    gridOf,
    gridHeader,
    columnAt,
    numericCell,
    isInt,
    isDouble,
    ellipsed,
    elementCount,
    undecodableCount,
    binaryish,
    gridDelimiters,
) where

import Data.Char (isDigit, isSpace)
import Data.List (sortOn)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR

data Grid = Grid
    { gridDelim :: Text
    , gridRows :: [[Text]]
    , gridWidth :: Int
    }
    deriving (Eq, Show)

-- | The separators a machine-written table is likely to use.
gridDelimiters :: [Text]
gridDelimiters = [",", "\t", ";", "|"]

{- | A grid only when the evidence is there: at least two lines, and a
candidate that splits every one of them into the same number of at least two
fields. The widest such candidate wins, ties going to the earlier candidate.
-}
gridOf :: [Text] -> [Text] -> Maybe Grid
gridOf delims ls
    | length ls < 2 = Nothing
    | otherwise =
        listToMaybe (sortOn (negate . gridWidth) (mapMaybe consistent delims))
  where
    consistent d = case map (T.splitOn d) ls of
        rows@(r : _)
            | let w = length r
            , w >= 2
            , all ((== w) . length) rows ->
                Just (Grid d rows w)
        _ -> Nothing

{- | The grid's first row, when the cells give evidence that it names the
columns: a column whose first cell does not read as a number while every
sampled cell below it does. One question, so one grid has one answer.
-}
gridHeader :: Grid -> Maybe [Text]
gridHeader g = case gridRows g of
    (h : body@(_ : _)) | named h body -> Just h
    _ -> Nothing
  where
    named h body =
        or
            [ not (numericCell hi) && all numericCell col
            | (i, hi) <- zip [0 ..] h
            , let col = columnAt i body
            , not (null col)
            ]

-- | The cells at column @i@ of each row that has one.
columnAt :: Int -> [[Text]] -> [Text]
columnAt i = concatMap (take 1 . drop i)

-- | A cell that reads as a number, which is what a column name does not do.
numericCell :: Text -> Bool
numericCell t = isInt s || isDouble s
  where
    s = T.strip t

isInt :: Text -> Bool
isInt t = case TR.signed TR.decimal t :: Either String (Integer, Text) of
    Right (_, rest) -> T.null rest && not (T.null t) && T.all digitOrSign t
    _ -> False
  where
    digitOrSign c = isDigit c || c == '-' || c == '+'

isDouble :: Text -> Bool
isDouble t = case TR.signed TR.double t :: Either String (Double, Text) of
    Right (_, rest) -> T.null rest && not (T.null t)
    _ -> False

-- | A cut excerpt says it was cut.
ellipsed :: Int -> Text -> Text
ellipsed n t
    | T.length t <= n = t
    | otherwise = T.take n t <> "…"

{- | Elements of the first bracketed collection in a rendering, counted from
the top-level separators. Covers @show@ of a list, of @fromList@ for a Map or
a Set, and encoded JSON alike, because it reads brackets rather than types.
-}
elementCount :: Text -> Maybe Int
elementCount t = case T.break (`elem` ("[{" :: String)) t of
    (_, rest)
        | T.null rest -> Nothing
        | otherwise -> walk (T.unpack (T.drop 1 rest)) (1 :: Int) 0 False False

walk :: String -> Int -> Int -> Bool -> Bool -> Maybe Int
walk [] _ _ _ _ = Nothing
walk (c : cs) depth commas seen inStr
    | inStr = case c of
        '\\' -> walk (drop 1 cs) depth commas seen True
        '"' -> walk cs depth commas seen False
        _ -> walk cs depth commas seen True
    | c == '"' = walk cs depth commas True True
    | c `elem` ("[{(" :: String) = walk cs (depth + 1) commas True False
    | c `elem` ("]})" :: String) =
        if depth <= 1
            then Just (if seen then commas + 1 else 0)
            else walk cs (depth - 1) commas seen False
    | c == ',' && depth == 1 = walk cs depth (commas + 1) seen False
    | isSpace c = walk cs depth commas seen False
    | otherwise = walk cs depth commas True False

-- | Characters the decoder could not make sense of.
undecodableCount :: Text -> Int
undecodableCount = T.length . T.filter (== '\65533')

{- | Text that is not really text: a NUL byte, a character the decoder gave
up on, or a wall of control bytes.
-}
binaryish :: Text -> Bool
binaryish t =
    T.any (== '\NUL') t
        || undecodableCount t > 0
        || (n > 0 && ctrl * 50 > n)
  where
    n = T.length t
    ctrl = T.length (T.filter isOddControl t)
    isOddControl c = c < ' ' && c `notElem` ("\n\t\r" :: String)
