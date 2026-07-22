{-# LANGUAGE OverloadedStrings #-}

{- | Ranking signals shared by the hoogle resolve and prose pipelines: the
no-popularity ecosystem score and the rich-hit ranking for capability search.
Split from "Sabela.AI.HoogleResolve" for the module-size cap.
-}
module Sabela.AI.HoogleRank (
    ecosystemScore,
    rankHits,
    nubOnKey,
) where

import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.HoogleClient (HoogleHit (..))
import Sabela.AI.ModuleResolve (isNoiseModule)

{- | A no-popularity-data ranking signal: 0 for a curated well-known ecosystem
package, 2 for a clearly niche/odd one (long hyphenated name or a vendor-ish
token), 1 otherwise. Lower sorts first.
-}
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

{- | A modest allow-list of well-known ecosystem + Sabela domain packages
(no popularity data).
-}
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

{- | Rank rich hits for capability search: promote allow-listed ecosystem
packages ('ecosystemScore') first, keep hoogle's relevance order per band,
dedup by (name, module, package), drop noise modules ('isNoiseModule').
-}
rankHits :: [HoogleHit] -> [HoogleHit]
rankHits hits =
    map snd (nub' (sortOn rankKey (zip [0 :: Int ..] keep)))
  where
    keep = filter (not . isNoiseModule . hhModule) hits
    rankKey (i, h) = (ecosystemScore (hhPackage h), i)
    nub' = nubOnKey (\(_, h) -> (hhName h, hhModule h, hhPackage h))

-- | Order-preserving dedup keyed by a projection.
nubOnKey :: (Eq b) => (a -> b) -> [a] -> [a]
nubOnKey f = go []
  where
    go _ [] = []
    go seen (x : xs)
        | f x `elem` seen = go seen xs
        | otherwise = x : go (f x : seen) xs
