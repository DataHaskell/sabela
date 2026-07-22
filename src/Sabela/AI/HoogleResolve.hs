{-# LANGUAGE OverloadedStrings #-}

{- | Phase-0.2 LEVER: a LOCAL Hoogle name→(package, module) resolver. When GHC
says a name is not in scope, shell out to the local @hoogle@ CLI, parse its JSON,
and rank an exact-name hit to the package and module that provides it — ready for
'Sabela.AI.DepRepair.addBuildDepend' + 'Sabela.AI.ImportRepair.addImport'. The
public hoogle.haskell.org API is NEVER consulted; on any failure (binary absent,
no database, empty/malformed output) the resolver returns Nothing. The prose
pipeline lives in "Sabela.AI.HoogleProse", ranking in "Sabela.AI.HoogleRank";
both re-export here so consumers keep one import surface.
-}
module Sabela.AI.HoogleResolve (
    HoogleHit (..),
    parseHoogleBlob,
    hoogleDbArgSets,
    rankResolve,
    rankResolveTopK,
    hoogleResolve,
    hoogleResolveTopK,
    hoogleQuery,
    rankHits,
    isNoiseModule,
    ecosystemScore,
    keywords,
    denoise,
    isTypeOrName,
    isSingleToken,
    bigrams,
    roundRobin,
) where

import Data.List (nub, sortOn)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.CellEco (concreteHead, resultHead)
import Sabela.AI.HoogleClient (
    HoogleHit (..),
    hoogleDbArgSets,
    parseHoogleBlob,
    queryAllDbs,
 )
import Sabela.AI.HoogleProse (
    bigrams,
    denoise,
    hoogleQuery,
    isSingleToken,
    isTypeOrName,
    keywords,
    roundRobin,
 )
import Sabela.AI.HoogleRank (ecosystemScore, rankHits)
import Sabela.AI.ModuleResolve (isNoiseModule)

{- | The best (package, module) for an exact-name match, or Nothing when no hit's
name equals the query. A thin head-of-shortlist wrapper over 'rankResolveTopK'.
-}
rankResolve :: Text -> [HoogleHit] -> Maybe (Text, Text)
rankResolve name hits = case rankResolveTopK 1 name Nothing hits of
    (c : _) -> Just c
    [] -> Nothing

{- | The top-K (package, module) exact-name candidates, ranked best-first and
deduplicated. Noise modules ('isNoiseModule') are dropped. Without popularity
data the order is (ecosystemScore, module length, package length, module): a
well-known ecosystem package outranks a niche one, then shorter names win.
-}
rankResolveTopK :: Int -> Text -> Maybe Text -> [HoogleHit] -> [(Text, Text)]
rankResolveTopK k name mGoal hits =
    take (max 0 k) (nub (map toPair (sortOn rankKey exact)))
  where
    exact =
        filter
            (\h -> hhName h == name && not (isNoiseModule (hhModule h)))
            hits
    -- A goal-type result-head mismatch DEMOTES a hit below every matching
    -- one; the decline itself is the scratch vet's job, where the compiler —
    -- not this text heuristic — is the oracle. Polymorphic heads never mismatch.
    typeFits h = case mGoal of
        Nothing -> True
        Just goal ->
            let gh = resultHead goal
                hh = resultHead (hhType h)
             in not (concreteHead gh) || not (concreteHead hh) || gh == hh
    toPair h = (hhPackage h, hhModule h)
    rankKey h =
        ( if typeFits h then 0 else 1 :: Int
        , ecosystemScore (hhPackage h)
        , T.length (hhModule h)
        , T.length (hhPackage h)
        , hhModule h
        )

{- | Resolve a not-in-scope name to its (package, module) via the LOCAL hoogle CLI
only. Env @SABELA_HOOGLE_BIN@ overrides the binary (default @hoogle@) and
@SABELA_HOOGLE_DB@ adds @--database@. Returns Nothing on any failure — there is no
network fallback.
-}
hoogleResolve :: Text -> IO (Maybe (Text, Text))
hoogleResolve name = do
    cands <- hoogleResolveTopK 1 name Nothing
    pure $ case cands of
        (c : _) -> Just c
        [] -> Nothing

{- | The top-K (package, module) candidates via the LOCAL hoogle CLI only. Env
@SABELA_HOOGLE_BIN@ overrides the binary (default @hoogle@) and
@SABELA_HOOGLE_DB@ adds @--database@. Empty on any failure — no network fallback.
-}
hoogleResolveTopK :: Int -> Text -> Maybe Text -> IO [(Text, Text)]
hoogleResolveTopK k name mGoal
    | T.null name = pure []
    | otherwise = do
        hits <- queryAllDbs ("search" : "--count=20" : "--json" : [T.unpack name])
        pure (rankResolveTopK k name mGoal hits)
