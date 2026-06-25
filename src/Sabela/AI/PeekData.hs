{-# LANGUAGE OverloadedStrings #-}

{- | Pure analysis behind the @peek_data@ tool: from raw delimited-file text,
infer the delimiter, decide whether the first row is a header, take the first
N data rows, and guess a column type for each field. The tool boundary in
"Sabela.AI.Capabilities.Query" reads the file (path-checked) and hands the
text here; everything in this module is total and testable without IO.
-}
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

{- | The four column shapes the guesser distinguishes. 'ColText' is the
fallthrough for anything that is not uniformly numeric or boolean.
-}
data ColType = ColInt | ColDouble | ColBool | ColText
    deriving (Eq, Show)

colTypeName :: ColType -> Text
colTypeName ColInt = "Int"
colTypeName ColDouble = "Double"
colTypeName ColBool = "Bool"
colTypeName ColText = "Text"

-- | The structured result the @peek_data@ tool reports.
data PeekResult = PeekResult
    { peekDelimiter :: Text
    , peekHasHeader :: Bool
    , peekHeader :: [Text]
    , peekColTypes :: [ColType]
    , peekRows :: [[Text]]
    }
    deriving (Eq, Show)

-- | Candidate delimiters, tried in priority order on a tie.
candidateDelims :: [Text]
candidateDelims = [",", "\t", ";", "|"]

{- | Analyse the first lines of a delimited file: pick the delimiter that
splits the rows most consistently, decide whether row one is a header (it is
when at least one column is numeric in the body but not in row one), take the
first @n@ data rows, and guess each column's type from the body.
-}
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

-- | Cells in column @i@ across all rows (missing cells skipped).
columnAt :: Int -> [[Text]] -> [Text]
columnAt i = concatMap (take 1 . drop i)

{- | The delimiter whose split gives the most consistent, widest rows.
Score each candidate by how many rows share the most common field count,
weighted by that count; ties break by 'candidateDelims' order.
-}
inferDelimiter :: [Text] -> Text
inferDelimiter [] = ","
inferDelimiter ls = fst (maximumBy (comparing snd) scored)
  where
    scored = [(d, score d) | d <- reverse candidateDelims]
    score d =
        let counts = map (length . T.splitOn d) ls
            modeCount = mode counts
         in modeCount * length (filter (== modeCount) counts)

-- | The most frequent value in a non-empty list (0 for empty).
mode :: (Eq a) => [a] -> a
mode [] = error "mode: empty list"
mode xs = snd (maximumBy (comparing fst) [(length (filter (== x) xs), x) | x <- xs])

{- | Row one is a header when at least one column parses as numeric in the
body rows but not in row one. A single-row file has no header.
-}
looksLikeHeader :: [[Text]] -> Bool
looksLikeHeader (h : body@(_ : _)) =
    or
        [ not (isNumericCell hi) && all isNumericCell (columnAt i body)
        | (i, hi) <- zip [0 ..] h
        , not (null (columnAt i body))
        ]
looksLikeHeader _ = False

-- | A field that parses cleanly as an integer or a decimal.
isNumericCell :: Text -> Bool
isNumericCell = (`elem` [ColInt, ColDouble]) . classifyCell

guessColType :: [Text] -> ColType
guessColType cells =
    case filter (not . T.null) (map T.strip cells) of
        [] -> ColText
        nonBlank -> foldr1 unify (map classifyCell nonBlank)

{- | Combine two cell classifications into the column's type: any 'ColText'
poisons the column, an 'Int' beside a 'Double' widens to 'Double', a mix of
otherwise-incompatible kinds falls back to 'ColText'.
-}
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
