{-# LANGUAGE OverloadedStrings #-}

{- | Prose and name search over Hoogle.

This is a thin front for "Sabela.AI.Search": parse the query into a 'Need',
fan out over every probe derived from its terms, fuse and rank once.
-}
module Sabela.AI.HoogleProse (
    hoogleQuery,
    hoogleQueryInScope,
    hoogleQueryWith,
    keywords,
    denoise,
    isTypeOrName,
    isSingleToken,
    bigrams,
    roundRobin,
) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.HoogleClient (HoogleHit (..), queryAllDbs)
import Sabela.AI.Popularity (loadPopularity)
import Sabela.AI.Search.Gather (Retriever, searchNeed, searchNeedWith)
import Sabela.AI.Search.Need (
    denoise,
    isSingleToken,
    isTypeOrName,
    keywords,
    parseNeed,
 )
import Sabela.AI.Search.Probe (bigrams)

hoogleQuery :: Int -> Text -> IO [HoogleHit]
hoogleQuery = hoogleQueryInScope Set.empty

hoogleQueryInScope :: Set Text -> Int -> Text -> IO [HoogleHit]
hoogleQueryInScope inScope k query = do
    pop <- loadPopularity
    searchNeedWith pop rawQuery k (parseNeed inScope query)

{- | Search with a caller-supplied retriever. Used by tests to replay recorded
Hoogle responses without the binary.
-}
hoogleQueryWith :: Retriever -> Int -> Text -> IO [HoogleHit]
hoogleQueryWith run k query = searchNeed run k (parseNeed Set.empty query)

roundRobin :: [[a]] -> [a]
roundRobin xss = case filter (not . null) xss of
    [] -> []
    nonEmpty -> map head nonEmpty ++ roundRobin (map tail nonEmpty)

rawQuery :: Retriever
rawQuery k q
    | T.null (T.strip q) = pure []
    | otherwise = do
        let cnt = "--count=" ++ show (max 1 k * 3)
        queryAllDbs (["search", cnt, "--json"] ++ [T.unpack q])
