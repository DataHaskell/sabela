{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.HoogleResolve (
    HoogleHit (..),
    parseHoogleBlob,
    hoogleDbArgSets,
    rankResolve,
    rankResolveTopK,
    hoogleResolve,
    hoogleResolveTopK,
    hoogleQuery,
    hoogleQueryInScope,
    rankHits,
    rankHitsInScope,
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
    hoogleQueryInScope,
    isSingleToken,
    isTypeOrName,
    keywords,
    roundRobin,
 )
import Sabela.AI.HoogleRank (ecosystemScore, rankHits, rankHitsInScope)
import Sabela.AI.ModuleResolve (isNoiseModule, isOutOfScopePackage)

rankResolve :: Text -> [HoogleHit] -> Maybe (Text, Text)
rankResolve name hits = case rankResolveTopK 1 name Nothing hits of
    (c : _) -> Just c
    [] -> Nothing

rankResolveTopK :: Int -> Text -> Maybe Text -> [HoogleHit] -> [(Text, Text)]
rankResolveTopK k name mGoal hits =
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
        , ecosystemScore (hhPackage h)
        , T.length (hhModule h)
        , T.length (hhPackage h)
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
        hits <- queryAllDbs ("search" : "--count=20" : "--json" : [T.unpack name])
        pure (rankResolveTopK k name mGoal hits)
