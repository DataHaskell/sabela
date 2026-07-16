{-# LANGUAGE OverloadedStrings #-}

{- | The pure core of the module-not-found repair: pick the installed modules
closest to a wrong module name. GHC often gives no "Perhaps you meant" for a
wholly wrong name (@Data.DataFrame@ when the module is @DataFrame@), so this
resolves against the real installed-module list instead of GHC's own hint. The
session query that supplies that list, and the verify-and-revert that vets each
rewrite, live in the product layer ("Sabela.AI.Capabilities.Edit.Run").
-}
module Sabela.AI.ModuleResolve (
    closestModules,
) where

import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Similarity (trigramSimilarity)

{- | Up to @k@ installed modules most similar to @wrong@, best first, keeping only
those clearing the similarity @threshold@. The wrong name itself is excluded (it is
not installed, by definition of "not found"). Empty when nothing clears the bar, so
a genuinely novel module falls through untouched rather than being mis-rewritten.
Ties break towards the shorter, then lexicographically smaller, name.
-}
closestModules :: Int -> Double -> Text -> [Text] -> [Text]
closestModules k threshold wrong mods =
    take k
        . map fst
        . sortOn rank
        $ scored
  where
    scored =
        [ (m, s)
        | m <- mods
        , m /= wrong
        , let s = trigramSimilarity wrong m
        , s >= threshold
        ]
    rank (m, s) = (Down s, T.length m, m)
