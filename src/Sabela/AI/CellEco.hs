{-# LANGUAGE OverloadedStrings #-}

{- | The ecosystem a notebook cell has committed to — the modules it imports and
the packages its @-- cabal: build-depends:@ line declares — and the pure ranking
of type-directed hole-fit candidates within it. Lets self-heal prefer the cell's
own libraries and DECLINE a type-incompatible cross-import (attoparsec's
@takeWhile1 :: … -> Parser Text@ for a megaparsec @… -> ParsecT@ goal).
-}
module Sabela.AI.CellEco (
    CellEco (..),
    FitCand (..),
    cellEco,
    fitProvenance,
    rankFits,
    resultHead,
    concreteHead,
) where

import Data.Char (isAlphaNum, isUpper)
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

-- | The modules/packages a cell already commits to, so resolution can prefer them.
data CellEco = CellEco
    { ecoModules :: Set Text
    , ecoPackages :: Set Text
    }
    deriving (Eq, Show)

-- | A type-directed candidate: a name, its type, and its home module + package.
data FitCand = FitCand
    { fcName :: Text
    , fcType :: Text
    , fcModule :: Text
    , fcPackage :: Text
    }
    deriving (Eq, Show)

{- | Read a cell's committed ecosystem: the modules it imports and the packages
its @-- cabal: build-depends:@ line declares.
-}
cellEco :: Text -> CellEco
cellEco src =
    CellEco
        (S.fromList (mapMaybe importedModule ls))
        (S.fromList (concatMap cabalDeps ls))
  where
    ls = T.lines src

{- | The module a single @import@ line brings in (qualifier / alias / import list
stripped), or 'Nothing' for a non-import line.
-}
importedModule :: Text -> Maybe Text
importedModule l = case T.words (T.strip l) of
    ("import" : rest) -> case dropWhile (== "qualified") rest of
        (m : _) ->
            let name = T.takeWhile (/= '(') m
             in if concreteHead name then Just name else Nothing
        [] -> Nothing
    _ -> Nothing

{- | Package names from a @-- cabal: build-depends: a, b >= 1@ line, version
bounds stripped. Empty for any other line.
-}
cabalDeps :: Text -> [Text]
cabalDeps l = case T.stripPrefix "build-depends:" body of
    Just deps ->
        filter
            (not . T.null)
            (map (T.takeWhile isPkgChar . T.strip) (T.splitOn "," deps))
    Nothing -> []
  where
    body = maybe "" T.strip (T.stripPrefix "-- cabal:" (T.strip l))
    isPkgChar c = isAlphaNum c || c == '-' || c == '_'

{- | Pair each hole fit with the module its @(imported from M)@ line names — the
provenance the fit parser otherwise discards, needed to map a fit to its package.
-}
fitProvenance :: Text -> [(Text, Text)]
fitProvenance blob = go Nothing (T.lines blob)
  where
    go _ [] = []
    go mName (l : ls) =
        let s = T.strip l
         in case (T.stripPrefix "(imported from " s, mName) of
                (Just rest, Just name) ->
                    (name, moduleOf rest) : go Nothing ls
                _ -> go (fitName s `orElse` mName) ls
    -- GHC 9.12 wraps the module in smart quotes and may close the paren on the
    -- NEXT line, so strip the quotes and stop at either terminator.
    moduleOf rest =
        T.takeWhile (\c -> c /= ')' && c /= '\8217') (T.dropWhile (== '\8216') rest)
    fitName s = case T.breakOn "::" s of
        (nm, rest)
            | not (T.null rest)
            , let n = T.strip nm
            , not (T.null n)
            , not (T.any (== ' ') n) ->
                Just n
        _ -> Nothing
    orElse (Just x) _ = Just x
    orElse Nothing y = y

{- | Rank type-directed candidates for a goal type within a cell's ecosystem: a
concrete result-head mismatch (the foreign type-incompatible import) DEMOTES a
fit below every matching one — the decline itself is the scratch vet's job,
where the compiler is the oracle — then the cell's own modules/packages rank
first. A polymorphic result head never counts as a mismatch.
-}
rankFits :: Text -> CellEco -> [FitCand] -> [FitCand]
rankFits goal eco = sortOn (\c -> (mismatchTier c, ecoTier c))
  where
    gh = resultHead goal
    mismatchTier c =
        let ch = resultHead (fcType c)
         in if not (concreteHead gh) || not (concreteHead ch) || gh == ch
                then 0 :: Int
                else 1
    ecoTier c
        | fcModule c `S.member` ecoModules eco
            || fcPackage c `S.member` ecoPackages eco =
            0 :: Int
        | otherwise = 1

{- | Head type constructor of a signature's result (after class context +
arrows), module qualifier and list/tuple punctuation stripped; @""@ for empty.
-}
resultHead :: Text -> Text
resultHead typ =
    case T.words (T.filter (`notElem` ("[]()" :: String)) res) of
        (t : _) -> last (T.splitOn "." t)
        [] -> ""
  where
    res = last (T.splitOn "->" (last (T.splitOn "=>" typ)))

{- | A concrete (constructor) head — starts uppercase; a type variable or empty
is not, so a polymorphic goal never gates the resolver.
-}
concreteHead :: Text -> Bool
concreteHead t = case T.uncons t of
    Just (c, _) -> isUpper c
    Nothing -> False
