{-# LANGUAGE OverloadedStrings #-}

{- | Ranking a module's exports against what the caller asked for.

A module card is a truncation: @Chart.Primitive@ has 116 exports and @Chart@ has
840, and every budget downstream takes the head of the list. So the order /is/
the answer — an alphabetical head shows @blank, blankChart1, box, boxes@ and
buries the constructor the caller needed.

Relevance reuses 'coverage', the same scorer the Hoogle path uses, so a term is
matched against an export's name, its type and its docs by one rule rather than
two divergent ones. Popularity, within a single module, is provenance: an export
the module itself defines outranks one it re-exports from elsewhere.
-}
module Sabela.AI.Search.Export (
    ExportRow (..),
    rankExportRows,
    rankExportsBy,
    exportRelevance,
    producesType,
    resultOf,
    isReExport,
    exportBand,
    bareName,
) where

import Data.Char (isAlpha, isAlphaNum, isUpper)
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.HoogleClient (HoogleHit (..))
import Sabela.AI.Search.Need (Need (..))
import Sabela.AI.Search.Row (coverage, termsCovered)

data ExportRow = ExportRow
    { erName :: Text
    , erType :: Text
    , erDocs :: Text
    }
    deriving (Eq, Show)

{- | Best first. The caller's terms lead; then the module's own API ahead of its
re-exports; then values ahead of type-level names ahead of internals; then the
plainest name, so @glyphChart@ precedes @glyphChart'@. Ties keep input order.
-}
rankExportRows :: Need -> Text -> [ExportRow] -> [ExportRow]
rankExportRows need modName = rankExportsBy need modName id

-- | Rank anything that carries an 'ExportRow', keeping the payload attached.
rankExportsBy :: Need -> Text -> (a -> ExportRow) -> [a] -> [a]
rankExportsBy need modName toRow xs =
    map snd (sortOn key (zip [0 :: Int ..] xs))
  where
    key (i, x) =
        let r = toRow x
            score = exportRelevance need modName r
         in ( negate (termsCovered need (asHit modName r))
            , negate score
            , if isReExport modName (erName r) then 1 else 0 :: Int
            , exportBand (erName r)
            , -- Prefer the plainest name only among rows that actually matched;
              -- otherwise a plain browse gets reshuffled by name length.
              if score > 0 then T.length (erName r) else 0
            , i
            )

exportRelevance :: Need -> Text -> ExportRow -> Int
exportRelevance need modName r =
    coverage need (asHit modName r) + producerCredit need r

{- | A tie-layer inside relevance, never a key above it: among rows the query
covers equally, the one that /makes/ the type asked for is the lead the caller
cannot reach any other way, and the one that consumes it is not.
-}
producerCredit :: Need -> ExportRow -> Int
producerCredit need r =
    length [t | t <- needTerms need, typeShaped t, producesType t r]
  where
    typeShaped t = maybe False (isUpper . fst) (T.uncons t)

-- | Does the row's signature end in the named type?
producesType :: Text -> ExportRow -> Bool
producesType ty r = ty `elem` map bareName (typeTokens (resultOf (erType r)))

{- | The result of a signature: what stands right of its last top-level arrow.
Arrows under a bracket belong to an argument, so @(a -> T) -> Int@ produces
@Int@ and not @T@.
-}
resultOf :: Text -> Text
resultOf = T.pack . reverse . go (0 :: Int) "" . T.unpack
  where
    go _ acc [] = acc
    go d acc ('-' : '>' : cs)
        | d <= 0 = go d "" cs
        | otherwise = go d ('>' : '-' : acc) cs
    go d acc (c : cs)
        | c `elem` ("([" :: String) = go (d + 1) (c : acc) cs
        | c `elem` (")]" :: String) = go (d - 1) (c : acc) cs
        | otherwise = go d (c : acc) cs

typeTokens :: Text -> [Text]
typeTokens = filter (not . T.null) . T.split (not . identChar)
  where
    identChar c = isAlphaNum c || c `elem` ("._'" :: String)

asHit :: Text -> ExportRow -> HoogleHit
asHit modName r =
    HoogleHit (bareName (erName r)) "" modName (erType r) (erDocs r) ""

{- | @:browse@ qualifies a re-exported name with the module that defines it:
@Chart@ lists @NumHask.Algebra.Additive.+@. An unqualified name, or one
qualified inside the browsed module's own namespace, is the module's own API.
-}
isReExport :: Text -> Text -> Bool
isReExport modName name = case qualifierOf name of
    Nothing -> False
    Just q -> not (q `T.isPrefixOf` modName) && not (modName `T.isPrefixOf` q)

qualifierOf :: Text -> Maybe Text
qualifierOf name
    | T.null dotted = Nothing
    | otherwise = Just (T.dropWhileEnd (== '.') dotted)
  where
    dotted = T.dropWhileEnd (/= '.') (unwrap name)

bareName :: Text -> Text
bareName = T.takeWhileEnd (/= '.') . unwrap

unwrap :: Text -> Text
unwrap = T.dropAround (`elem` ("()" :: String)) . T.strip

-- | Ordinary values first, then type-level names, then operators and internals.
exportBand :: Text -> Int
exportBand name
    | "_" `T.isPrefixOf` bare = 3
    | not (T.any isAlpha bare) = 2
    | maybe False (isUpper . fst) (T.uncons bare) = 1
    | otherwise = 0
  where
    bare = bareName name
