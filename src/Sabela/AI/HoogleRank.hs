{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.HoogleRank (
    ecosystemScore,
    rankHits,
    rankHitsInScope,
    nubOnKey,
) where

import Data.List (sortOn)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.HoogleClient (HoogleHit (..))
import Sabela.AI.ModuleResolve (isNoiseModule)

ecosystemScore :: Text -> Int
ecosystemScore pkg
    | pkg `elem` ecosystemPackages = 0
    | isNiche pkg = 2
    | otherwise = 1
  where
    isNiche p =
        let segs = T.splitOn "-" p
         in length segs >= 3
                || T.length p >= 22
                || any (`T.isInfixOf` p) vendorTokens
    vendorTokens = ["http-haskell", "client", "sdk", "api-"]

ecosystemPackages :: [Text]
ecosystemPackages =
    [ "base"
    , "containers"
    , "text"
    , "bytestring"
    , "vector"
    , "time"
    , "aeson"
    , "conduit"
    , "mtl"
    , "transformers"
    , "unordered-containers"
    , "regex-tdfa"
    , "megaparsec"
    , "fgl"
    , "JuicyPixels"
    , "async"
    , "http-conduit"
    , "directory"
    , "filepath"
    , "process"
    , "stm"
    , "dataframe"
    , "dataframe-core"
    , "dataframe-viz"
    , "dataframe-operations"
    , "granite"
    ]

rankHits :: [HoogleHit] -> [HoogleHit]
rankHits = rankHitsInScope Set.empty

rankHitsInScope :: Set Text -> [HoogleHit] -> [HoogleHit]
rankHitsInScope inScope hits =
    map snd (nub' (sortOn rankKey (zip [0 :: Int ..] keep)))
  where
    keep = filter (not . isNoiseModule . hhModule) hits
    rankKey (i, h) = (outOfScope h, ecosystemScore (hhPackage h), i)
    outOfScope h = if hhPackage h `Set.member` inScope then 0 else 1 :: Int
    nub' = nubOnKey (\(_, h) -> (hhName h, hhModule h, hhPackage h))

nubOnKey :: (Eq b) => (a -> b) -> [a] -> [a]
nubOnKey f = go []
  where
    go _ [] = []
    go seen (x : xs)
        | f x `elem` seen = go seen xs
        | otherwise = x : go (f x : seen) xs
