{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Capability (
    Capability (..),
    Match (..),
    Hit (..),
    Synonyms,
    defaultSynonyms,
    searchCapabilities,
    coalesce,
    declMembers,
    parseCapabilities,
    relevanceScore,
    statedType,
    synonymDecl,
    unqualify,
) where

import Control.Monad (guard)
import Data.Char (isAlphaNum)
import Data.List (nubBy, sortOn)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capability.Parse (
    Capability (..),
    coalesce,
    declMembers,
    parseCapabilities,
    statedType,
    synonymDecl,
    unqualify,
 )
import Sabela.AI.Similarity (trigramSimilarity)

data Match = ByName | ByType | BySynonym | ByModule
    deriving (Eq, Show)

data Hit = Hit
    { hitCap :: Capability
    , hitScore :: Int
    , hitVia :: Match
    }
    deriving (Eq, Show)

type Synonyms = [(Text, [Text])]

defaultSynonyms :: Synonyms
defaultSynonyms =
    [ ("animation", ["anim"])
    , ("animate", ["anim"])
    , ("move", ["anim"])
    , ("frame", ["anim"])
    , ("classification", ["logistic"])
    , ("classify", ["logistic"])
    , ("classifier", ["logistic"])
    , ("plot", ["bars", "linegraph", "scatter", "pie", "area"])
    , ("chart", ["bars", "linegraph"])
    , ("graph", ["linegraph", "scatter"])
    , ("regression", ["linear", "fit"])
    , ("reactive", ["frp", "behavior", "event"])
    , ("frp", ["behavior", "event"])
    , ("overlay", ["group", "mconcat"])
    , ("superimpose", ["group", "mconcat"])
    , ("combine", ["group", "mconcat"])
    , ("compose", ["group", "mconcat"])
    , ("stack", ["group", "mconcat"])
    , ("pictures", ["picture"])
    ]

searchCapabilities :: Synonyms -> [Capability] -> Text -> [Hit]
searchCapabilities syns idx query =
    focus $
        nubByNameType $
            sortOn rank $
                [Hit c s v | c <- idx, Just (s, v) <- [scoreCap syns ql qToks c]]
  where
    ql = lexQuery (T.toLower (T.strip query))
    qRaw = lexQuery (T.strip query)
    qToks = tokens ql
    rank h = (Down (hitScore h), caseRank h, T.length (capModule (hitCap h)))
    caseRank h = if capName (hitCap h) == qRaw then 0 else 1 :: Int
    nubByNameType =
        nubBy (\a b -> sameKey (hitCap a) (hitCap b))
    sameKey x y = capName x == capName y && capType x == capType y
    focus hits = case hits of
        (h : _)
            | hitScore h >= exactScore ->
                take 5 (takeWhile ((>= exactScore) . hitScore) hits)
        _ -> take 8 hits
    exactScore = 100

relevanceScore :: Synonyms -> Text -> Capability -> Int
relevanceScore syns query c = maybe 0 fst (scoreCap syns ql (tokens ql) c)
  where
    ql = lexQuery (T.toLower (T.strip query))

lexQuery :: Text -> Text
lexQuery q =
    T.unwords
        [ w
        | w <- T.words (dropLiterals q)
        , not ("@" `T.isPrefixOf` w)
        , w /= "@"
        ]
  where
    dropLiterals = T.pack . go False . T.unpack
    go _ [] = []
    go True ('"' : cs) = go False cs
    go True (_ : cs) = go True cs
    go False ('"' : cs) = go True cs
    go False (c : cs) = c : go False cs

scoreCap :: Synonyms -> Text -> [Text] -> Capability -> Maybe (Int, Match)
scoreCap syns ql qToks c =
    listToMaybe $
        catMaybes
            [ (100, ByName) <$ guard (ql == nameL)
            , (80, ByName) <$ guard (ql `T.isPrefixOf` nameL)
            , (60, ByName) <$ guard (ql `T.isInfixOf` nameL)
            , (58, ByType) <$ guard (typeShaped && typeMatch)
            , (55, ByName) <$ guard (any tokenInName qToks)
            , (50, ByType) <$ guard typeMatch
            , (45, ByName) <$ guard nearSpelling
            , (40, BySynonym) <$ guard synMatch
            , (30, ByModule) <$ guard (ql `T.isInfixOf` T.toLower (capModule c))
            ]
  where
    nameL = T.toLower (capName c)
    typeL = T.toLower (statedType c)
    tokenInName t = T.length t >= 3 && t `T.isInfixOf` nameL
    typeMatch = length qToks >= 2 && all (`T.isInfixOf` typeL) qToks
    synMatch = any (`T.isInfixOf` nameL) (synonymsFor syns ql)
    nearSpelling =
        T.length ql >= minFuzzyQuery
            && trigramSimilarity ql nameL >= fuzzyNameThreshold
    typeShaped = "->" `T.isInfixOf` ql

fuzzyNameThreshold :: Double
fuzzyNameThreshold = 0.4

minFuzzyQuery :: Int
minFuzzyQuery = 4

synonymsFor :: Synonyms -> Text -> [Text]
synonymsFor syns ql = concat [vs | (k, vs) <- syns, k `elem` toks]
  where
    toks = tokens ql

tokens :: Text -> [Text]
tokens = filter (not . T.null) . T.split (not . isAlphaNum)
