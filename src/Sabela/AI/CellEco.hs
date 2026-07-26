{-# LANGUAGE OverloadedStrings #-}

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

data CellEco = CellEco
    { ecoModules :: Set Text
    , ecoPackages :: Set Text
    }
    deriving (Eq, Show)

data FitCand = FitCand
    { fcName :: Text
    , fcType :: Text
    , fcModule :: Text
    , fcPackage :: Text
    }
    deriving (Eq, Show)

cellEco :: Text -> CellEco
cellEco src =
    CellEco
        (S.fromList (mapMaybe importedModule ls))
        (S.fromList (concatMap cabalDeps ls))
  where
    ls = T.lines src

importedModule :: Text -> Maybe Text
importedModule l = case T.words (T.strip l) of
    ("import" : rest) -> case dropWhile (== "qualified") rest of
        (m : _) ->
            let name = T.takeWhile (/= '(') m
             in if concreteHead name then Just name else Nothing
        [] -> Nothing
    _ -> Nothing

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

resultHead :: Text -> Text
resultHead typ =
    case T.words (T.filter (`notElem` ("[]()" :: String)) res) of
        (t : _) -> last (T.splitOn "." t)
        [] -> ""
  where
    res = last (T.splitOn "->" (last (T.splitOn "=>" typ)))

concreteHead :: Text -> Bool
concreteHead t = case T.uncons t of
    Just (c, _) -> isUpper c
    Nothing -> False
