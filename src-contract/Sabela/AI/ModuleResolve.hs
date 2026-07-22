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
    isNoiseModule,
) where

import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Similarity (trigramSimilarity)

{- | Up to @k@ installed modules clearing the similarity @threshold@ for @wrong@,
best first (ties to the shorter, then lexicographically smaller name). Empty when
nothing clears the bar, so a novel module falls through rather than mis-rewritten.
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

{- | Modules that are never a useful resolution or search target: internal,
example / demo / tutorial, and doc modules — an API-visibility class
judgement, never a library judgement. Shared by resolver and discover rank.
-}
isNoiseModule :: Text -> Bool
isNoiseModule m =
    m == "Internal"
        || "Documentation." `T.isPrefixOf` m
        || any (`T.isInfixOf` m) [".Internal", ".Example", ".Demo", ".Tutorial"]
