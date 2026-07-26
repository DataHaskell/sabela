{-# LANGUAGE OverloadedStrings #-}

module Eval.Applicability (
    PairCategory (..),
    transcriptBody,
    voidPair,
    discoverClassCalls,
    discoverClassNames,
    surfaceAnswered,
    classifyPair,
    readVoidFlags,
    readNaFlags,
    readSaturatedFlags,
    voidNote,
    naNote,
    saturatedNote,
    excludeFlagged,
) where

import Data.List (sort)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesDirectoryExist, listDirectory)
import Text.Read (readMaybe)

data PairCategory = PairSound | PairVoid | PairSaturated | PairNotApplicable
    deriving (Eq, Show)

headerOpen, headerClose :: Text
headerOpen = "<!-- episode-config"
headerClose = "-->"

transcriptBody :: Text -> Text
transcriptBody t = case T.lines t of
    (h : rest)
        | h == headerOpen ->
            T.unlines (drop 1 (dropWhile (/= headerClose) rest))
    _ -> t

voidPair :: Text -> Text -> Bool
voidPair a b = transcriptBody a == transcriptBody b

discoverClassNames :: [Text]
discoverClassNames =
    [ "discover"
    , "find_function"
    , "search_capability"
    , "api_reference"
    , "describe_function"
    , "find_by_type"
    , "find_example_cell"
    ]

discoverClassCalls :: Text -> Int
discoverClassCalls t =
    length (filter isCallLine ls) + length (filter isResultHeader ls)
  where
    ls = map T.strip (T.lines t)

surfaceAnswered :: Text -> Bool
surfaceAnswered = any (isResultHeader . T.strip) . T.lines

isCallLine :: Text -> Bool
isCallLine s = any (\n -> ("- `" <> n <> "`") `T.isPrefixOf` s) discoverClassNames

isResultHeader :: Text -> Bool
isResultHeader s =
    ("## " `T.isPrefixOf` s)
        && any (\n -> ("(" <> n <> ")") `T.isInfixOf` s) discoverClassNames

classifyPair :: Text -> Text -> PairCategory
classifyPair off on
    | searchFree off && searchFree on = PairNotApplicable
    | transcriptBody off /= transcriptBody on = PairSound
    | surfaceAnswered (transcriptBody on) = PairSaturated
    | otherwise = PairVoid
  where
    searchFree = (== 0) . discoverClassCalls . transcriptBody

readVoidFlags :: FilePath -> IO [(Text, Int)]
readVoidFlags = readFlags ".VOID"

readNaFlags :: FilePath -> IO [(Text, Int)]
readNaFlags = readFlags ".NA"

readSaturatedFlags :: FilePath -> IO [(Text, Int)]
readSaturatedFlags = readFlags ".SATURATED"

readFlags :: Text -> FilePath -> IO [(Text, Int)]
readFlags suffix dir = do
    exists <- doesDirectoryExist dir
    if not exists
        then pure []
        else sort . mapMaybe parseFlag <$> listDirectory dir
  where
    parseFlag name = do
        stem <- T.stripSuffix suffix (T.pack name)
        let (t, s) = T.breakOnEnd "-s" stem
        task <- T.stripSuffix "-s" t
        seed <- readMaybe (T.unpack s)
        pure (task, seed)

voidNote :: [(Text, Int)] -> Text
voidNote =
    flagNote
        "VOID pairs (byte-identical arms, discover surface never answered — \
        \lever dead; excluded from measurement)"

saturatedNote :: [(Text, Int)] -> Text
saturatedNote =
    flagNote
        "lever-saturated pairs (lever fired — the discover surface answered \
        \identically in both arms; a decided category, excluded from lever deltas)"

naNote :: [(Text, Int)] -> Text
naNote =
    flagNote
        "not-applicable pairs (zero discover-class calls both arms — lever axis \
        \inapplicable, excluded from deltas as a category)"

flagNote :: Text -> [(Text, Int)] -> Text
flagNote _ [] = ""
flagNote label vs =
    label
        <> ": "
        <> T.intercalate ", " [t <> " s" <> tshow s | (t, s) <- vs]
        <> "\n\n"

excludeFlagged ::
    [(Text, Int)] -> [(Text, Int, a, b)] -> [(Text, a, b)]
excludeFlagged flagged rows =
    [(t, m, s) | (t, seed, m, s) <- rows, (t, seed) `notElem` flagged]

tshow :: (Show a) => a -> Text
tshow = T.pack . show
