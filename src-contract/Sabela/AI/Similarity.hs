{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Similarity (
    trigrams,
    trigramSimilarity,
) where

import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

trigramSimilarity :: Text -> Text -> Double
trigramSimilarity a b
    | Set.null union = 0
    | otherwise =
        fromIntegral (Set.size inter) / fromIntegral (Set.size union)
  where
    inter = Set.intersection ta tb
    union = Set.union ta tb
    ta = trigrams a
    tb = trigrams b

trigrams :: Text -> Set.Set Text
trigrams t
    | T.length t < 3 = Set.singleton t
    | otherwise = Set.fromList [T.take 3 (T.drop i t) | i <- [0 .. T.length t - 3]]
