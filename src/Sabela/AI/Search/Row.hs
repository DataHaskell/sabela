{-# LANGUAGE OverloadedStrings #-}

{- | Classifying and scoring a single Hoogle row against a 'Need'.

Two ideas carry the module. A row's /kind/ says whether it is an answer or a
lead: a package or module row is Hoogle telling you where to look next.
A row's /coverage/ says how much of the query it accounts for, weighted by
which field the term was found in, so a name match outranks a docs match.
-}
module Sabela.AI.Search.Row (
    RowKind (..),
    rowKind,
    kindPrior,
    coverage,
    termsCovered,
    grounded,
    humps,
) where

import Data.Char (isAlphaNum, isUpper, toLower)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.HoogleClient (HoogleHit (..))
import Sabela.AI.Search.Need (Need (..))

data RowKind = RowSymbol | RowModule | RowPackage
    deriving (Eq, Ord, Show)

{- | Hoogle flattens packages, modules and symbols into one row shape; the kind
is recoverable from which fields are empty.
-}
rowKind :: HoogleHit -> RowKind
rowKind h
    | hhName h == hhPackage h && T.null (hhModule h) && T.null (hhType h) =
        RowPackage
    | hhName h == hhModule h && T.null (hhType h) = RowModule
    | otherwise = RowSymbol

-- | A symbol answers the question; a module or package only narrows it.
kindPrior :: RowKind -> Int
kindPrior RowSymbol = 0
kindPrior RowModule = 1
kindPrior RowPackage = 2

{- | Weighted count of query terms this row accounts for. Zero means the row
has nothing to do with the query and must never be returned as an answer.

Fields corroborate rather than compete: a symbol whose name and haddock both
speak to a term outranks one whose name alone does. Naming a package outright
is the strongest signal a caller can give, so an exact package match is
weighted like an exact name match.
-}
coverage :: Need -> HoogleHit -> Int
coverage need h = sum (map (min termCeiling . termWeight . T.toLower) (needTerms need))
  where
    termWeight t =
        sum
            [ nameWeight t
            , packageWeight t
            , fieldWeight 2 t (hhDocs h)
            , fieldWeight 2 t (hhType h)
            , fieldWeight 1 t (hhModule h)
            ]
    nameWeight t
        | T.null t = 0
        | lowerName == t = 6
        | t `elem` nameHumps || t `T.isPrefixOf` lowerName = 4
        | t `T.isInfixOf` lowerName = 3
        | otherwise = 0
    packageWeight t
        | T.null t = 0
        | lowerPackage == t = 6
        | otherwise = fieldWeight 1 t (hhPackage h)
    fieldWeight w t field
        | T.null t = 0
        | t `T.isInfixOf` T.toLower field = w
        | otherwise = 0
    lowerName = T.toLower (hhName h)
    lowerPackage = T.toLower (hhPackage h)
    nameHumps = humps (hhName h)

grounded :: Need -> HoogleHit -> Bool
grounded need h = coverage need h > 0

{- | How many distinct query terms the row speaks to at all. Accounting for
more of the question beats accounting for one part of it emphatically.
-}
termsCovered :: Need -> HoogleHit -> Int
termsCovered need h =
    length [t | t <- needTerms need, coverage (need{needTerms = [t]}) h > 0]

{- | No single term may count for more than an exact name match. Without the
ceiling a row that restates a term across its type and docs outscores the
plainer function that answers the question: @readParquetWithOpts@, whose type
mentions @ParquetReadOptions@, would bury @readParquet@.
-}
termCeiling :: Int
termCeiling = 6

{- | Lower-cased camel humps and separator-delimited words of an identifier:
@readParquet@ becomes @["read","parquet"]@, @DataFrame.IO.Parquet@ becomes
@["data","frame","io","parquet"]@.
-}
humps :: Text -> [Text]
humps = filter (not . T.null) . map T.pack . go "" . T.unpack
  where
    go acc [] = [reverse acc]
    go acc (c : cs)
        | not (isAlphaNum c) = reverse acc : go "" cs
        | isUpper c = reverse acc : go [toLower c] cs
        | otherwise = go (c : acc) cs
