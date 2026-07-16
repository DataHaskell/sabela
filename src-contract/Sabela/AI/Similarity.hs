{-# LANGUAGE OverloadedStrings #-}

{- | Character-trigram Jaccard similarity, the general fuzzy-match primitive
shared by the package-token resolver ('Sabela.Diagnose.Packages') and the
module-name resolver ('Sabela.AI.ModuleResolve'). Pure, so it lives in the
contract library and is used by both the product and the eval harness.
-}
module Sabela.AI.Similarity (
    trigrams,
    trigramSimilarity,
) where

import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

-- | Jaccard similarity of the two tokens' character-trigram sets.
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

-- | The character-trigram set of a token (the whole token when shorter than 3).
trigrams :: Text -> Set.Set Text
trigrams t
    | T.length t < 3 = Set.singleton t
    | otherwise = Set.fromList [T.take 3 (T.drop i t) | i <- [0 .. T.length t - 3]]
