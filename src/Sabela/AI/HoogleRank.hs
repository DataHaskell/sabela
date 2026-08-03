module Sabela.AI.HoogleRank (
    keepHit,
    rankHits,
    rankHitsInScope,
    rankHitsInScopeWith,
    nubOnKey,
) where

import Data.List (sortOn)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

import Sabela.AI.HoogleClient (HoogleHit (..))
import Sabela.AI.ModuleResolve (isNoiseModule, isOutOfScopePackage)
import Sabela.AI.Popularity (Popularity, emptyPopularity, packageRankKey)

{- | The one admissibility test every retrieval path shares. Compiler-toolchain
rows are never an answer to a library question, however well they rank.
-}
keepHit :: HoogleHit -> Bool
keepHit h =
    not (isNoiseModule (hhModule h)) && not (isOutOfScopePackage (hhPackage h))

rankHits :: [HoogleHit] -> [HoogleHit]
rankHits = rankHitsInScope Set.empty

rankHitsInScope :: Set Text -> [HoogleHit] -> [HoogleHit]
rankHitsInScope = rankHitsInScopeWith emptyPopularity

{- | Order hits for presentation. Packages the cell already builds against lead;
the measured ecosystem prior breaks the rest, retrieval order breaks the prior.
-}
rankHitsInScopeWith :: Popularity -> Set Text -> [HoogleHit] -> [HoogleHit]
rankHitsInScopeWith pop inScope hits =
    map snd (nub' (sortOn rankKey (zip [0 :: Int ..] keep)))
  where
    keep = filter keepHit hits
    rankKey (i, h) = (outOfScope h, packageRankKey pop (hhPackage h), i)
    outOfScope h = if hhPackage h `Set.member` inScope then 0 else 1 :: Int
    nub' = nubOnKey (\(_, h) -> (hhName h, hhModule h, hhPackage h))

nubOnKey :: (Eq b) => (a -> b) -> [a] -> [a]
nubOnKey f = go []
  where
    go _ [] = []
    go seen (x : xs)
        | f x `elem` seen = go seen xs
        | otherwise = x : go (f x : seen) xs
