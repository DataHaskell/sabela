{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.HoogleResolve (
    HoogleHit (..),
    parseHoogleBlob,
    hoogleDbArgSets,
    rankResolve,
    rankResolveTopK,
    rankResolveTopKWith,
    hoogleResolve,
    hoogleResolveTopK,
    hoogleQuery,
    hoogleQueryInScope,
    rankHits,
    rankHitsInScope,
    rankHitsInScopeWith,
    isNoiseModule,
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
    hoogleQueryInScope,
    isSingleToken,
    isTypeOrName,
    keywords,
    roundRobin,
 )
import Sabela.AI.HoogleRank (rankHits, rankHitsInScope, rankHitsInScopeWith)
import Sabela.AI.ModuleResolve (isNoiseModule, isOutOfScopePackage)
import Sabela.AI.Popularity (
    Popularity,
    emptyPopularity,
    loadPopularity,
    packageRankKey,
 )

rankResolve :: Text -> [HoogleHit] -> Maybe (Text, Text)
rankResolve name hits = case rankResolveTopK 1 name Nothing hits of
    (c : _) -> Just c
    [] -> Nothing

rankResolveTopK :: Int -> Text -> Maybe Text -> [HoogleHit] -> [(Text, Text)]
rankResolveTopK = rankResolveTopKWith emptyPopularity

{- | Shortlist the exact-name hits: one contradicting the goal's result head
sinks, the measured prior orders the rest, the module path settles what is
left. No term reads the package's name, its shape or its length.
-}
rankResolveTopKWith ::
    Popularity -> Int -> Text -> Maybe Text -> [HoogleHit] -> [(Text, Text)]
rankResolveTopKWith pop k name mGoal hits =
    take (max 0 k) (nub (map toPair (sortOn rankKey exact)))
  where
    exact =
        filter
            ( \h ->
                hhName h == name
                    && not (isNoiseModule (hhModule h))
                    && not (isOutOfScopePackage (hhPackage h))
            )
            hits
    typeFits h = case mGoal of
        Nothing -> True
        Just goal ->
            let gh = resultHead goal
                hh = resultHead (hhType h)
             in not (concreteHead gh) || not (concreteHead hh) || gh == hh
    toPair h = (hhPackage h, hhModule h)
    rankKey h =
        ( if typeFits h then 0 else 1 :: Int
        , packageRankKey pop (hhPackage h)
        , T.length (hhModule h)
        , hhModule h
        )

hoogleResolve :: Text -> IO (Maybe (Text, Text))
hoogleResolve name = do
    cands <- hoogleResolveTopK 1 name Nothing
    pure $ case cands of
        (c : _) -> Just c
        [] -> Nothing

hoogleResolveTopK :: Int -> Text -> Maybe Text -> IO [(Text, Text)]
hoogleResolveTopK k name mGoal
    | T.null name = pure []
    | otherwise = do
        pop <- loadPopularity
        hits <- queryAllDbs ("search" : "--count=20" : "--json" : [T.unpack name])
        pure (rankResolveTopKWith pop k name mGoal hits)
