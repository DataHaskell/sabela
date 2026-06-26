{-# LANGUAGE OverloadedStrings #-}

{- | A local, Hoogle-style search over the session's installed modules: each
exposed function is a 'Capability' (module, name, type), and 'searchCapabilities'
ranks the index (built from @:browse@) against a free-text query.
-}
module Sabela.AI.Capability (
    Capability (..),
    Match (..),
    Hit (..),
    Synonyms,
    defaultSynonyms,
    searchCapabilities,
    parseCapabilities,
) where

import Control.Monad (guard)
import Data.Char (isAlphaNum)
import Data.List (nubBy, sortOn)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T

-- | One exposed function: the unit of search, parsed from a @:browse@ line.
data Capability = Capability
    { capModule :: Text
    , capName :: Text
    , capType :: Text
    }
    deriving (Eq, Show)

-- | Why a capability matched (the scoring signal that won).
data Match = ByName | ByType | BySynonym | ByModule
    deriving (Eq, Show)

data Hit = Hit
    { hitCap :: Capability
    , hitScore :: Int
    , hitVia :: Match
    }
    deriving (Eq, Show)

-- | Query-vocabulary → API-vocabulary bridges, all lower-case.
type Synonyms = [(Text, [Text])]

-- | Query-word → API-word bridges for cases substring matching misses
-- (e.g. "classification" → "logistic").
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
    ]

{- | Rank an index against a free-text query; best matches first, capped. Ties
break toward the more specific (longer) module, then duplicates of the same
function — an umbrella module re-exporting it — collapse to one row.
-}
searchCapabilities :: Synonyms -> [Capability] -> Text -> [Hit]
searchCapabilities syns idx query =
    take 20 $
        nubByNameType $
            sortOn rank $
                [Hit c s v | c <- idx, Just (s, v) <- [scoreCap syns ql qToks c]]
  where
    ql = T.toLower (T.strip query)
    qToks = tokens ql
    rank h = (Down (hitScore h), Down (T.length (capModule (hitCap h))))
    nubByNameType =
        nubBy (\a b -> sameKey (hitCap a) (hitCap b))
    sameKey x y = capName x == capName y && capType x == capType y

{- | The highest-scoring signal a capability matches on, or Nothing — ordered
strongest first (exact > prefix > substring > token > type > synonym > module),
so 'listToMaybe' takes the winner.
-}
scoreCap :: Synonyms -> Text -> [Text] -> Capability -> Maybe (Int, Match)
scoreCap syns ql qToks c =
    listToMaybe $
        catMaybes
            [ (100, ByName) <$ guard (ql == nameL)
            , (80, ByName) <$ guard (ql `T.isPrefixOf` nameL)
            , (60, ByName) <$ guard (ql `T.isInfixOf` nameL)
            , (55, ByName) <$ guard (any tokenInName qToks)
            , (50, ByType) <$ guard typeMatch
            , (40, BySynonym) <$ guard synMatch
            , (30, ByModule) <$ guard (ql `T.isInfixOf` T.toLower (capModule c))
            ]
  where
    nameL = T.toLower (capName c)
    typeL = T.toLower (capType c)
    tokenInName t = T.length t >= 3 && t `T.isInfixOf` nameL
    typeMatch = length qToks >= 2 && all (`T.isInfixOf` typeL) qToks
    synMatch = any (`T.isInfixOf` nameL) (synonymsFor syns ql)

-- | Synonym expansions whose key occurs in the query.
synonymsFor :: Synonyms -> Text -> [Text]
synonymsFor syns ql = concat [vs | (k, vs) <- syns, k `T.isInfixOf` ql]

-- | Alphanumeric tokens: "double -> picture" → ["double","picture"].
tokens :: Text -> [Text]
tokens = filter (not . T.null) . T.split (not . isAlphaNum)

{- | Parse @:browse M@ output into capabilities, handling its two quirks:
fully-qualified names (stripped) and signatures wrapped across indented
continuation lines (coalesced). Skips type/data/class declarations.
-}
parseCapabilities :: Text -> Text -> [Capability]
parseCapabilities modName raw =
    [ Capability modName (unqualify nm) (cleanType ty)
    | ent <- coalesce (T.lines raw)
    , Just (nm, ty) <- [valueBinding ent]
    ]

-- | Join indented continuation lines into the entry they continue.
coalesce :: [Text] -> [Text]
coalesce [] = []
coalesce (l : ls) = go l ls
  where
    go cur [] = [cur]
    go cur (x : xs)
        | isCont x = go (cur <> " " <> T.strip x) xs
        | otherwise = cur : go x xs
    isCont x = case T.uncons x of
        Just (h, _) -> h == ' ' || h == '\t'
        Nothing -> False

-- | A @name :: type@ value binding (not a type/data/class/instance decl).
valueBinding :: Text -> Maybe (Text, Text)
valueBinding ent
    | any (`T.isPrefixOf` ent) declKeywords = Nothing
    | otherwise =
        let (nm, rest) = T.breakOn " :: " ent
         in if T.null rest
                then Nothing
                else Just (T.strip nm, T.strip (T.drop 4 rest))

declKeywords :: [Text]
declKeywords = ["type ", "data ", "newtype ", "class ", "instance ", "pattern "]

-- | The unqualified name: the last @.@-separated component.
unqualify :: Text -> Text
unqualify = T.takeWhileEnd (/= '.')

{- | Render a qualified signature readably: drop @pkg-version:@ prefixes and
module qualifiers, keeping each name's last component (so
@sabela-notebook-…:…Picture.Internal.Picture@ becomes @Picture@).
-}
cleanType :: Text -> Text
cleanType = T.concat . map reduce . chunk
  where
    chunk s
        | T.null s = []
        | isAtom (T.head s) = let (a, r) = T.span isAtom s in a : chunk r
        | otherwise = let (a, r) = T.break isAtom s in a : chunk r
    isAtom c = isAlphaNum c || c `elem` ['.', '_', '\'', ':', '-']
    reduce a
        | T.any (`elem` ['.', ':']) a = lastComponent a
        | otherwise = a
    lastComponent a =
        let afterColon =
                if T.any (== ':') a then T.drop 1 (T.dropWhile (/= ':') a) else a
         in T.takeWhileEnd (/= '.') afterColon
