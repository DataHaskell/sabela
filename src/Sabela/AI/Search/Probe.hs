{-# LANGUAGE OverloadedStrings #-}

-- | Turning a 'Need' into the set of Hoogle queries to issue.
--
-- Every probe is derived from the caller's own terms. Nothing here invents
-- vocabulary the user did not supply: a probe that cannot be traced back to a
-- query term can only match by coincidence.
module Sabela.AI.Search.Probe (
    Probe (..),
    planProbes,
    expandProbes,
    bigrams,
    maxPlanProbes,
    maxPivotProbes,
) where

import Data.List (nubBy)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.HoogleClient (HoogleHit (..))
import Sabela.AI.Search.Need (Need (..))
import Sabela.AI.Search.Row (RowKind (..), rowKind)

data Probe = Probe
    { probeQuery :: Text
    , probeVia :: Text
    , probeWeight :: Double
    }
    deriving (Eq, Show)

maxPlanProbes :: Int
maxPlanProbes = 10

maxPivotProbes :: Int
maxPivotProbes = 12

-- | Terms carried into a scoped re-query. Enough to discriminate, few enough
-- that the pivot stays cheap.
maxPivotTerms :: Int
maxPivotTerms = 2

-- | The first wave: the query as asked, plus progressively narrower
-- decompositions of it. All of them run; none short-circuits the others.
planProbes :: Need -> [Probe]
planProbes need =
    take maxPlanProbes (dedupe (whole ++ pairs ++ singles))
  where
    whole =
        [Probe (needRaw need) "raw" 1.0]
            ++ [ Probe (needCleaned need) "cleaned" 1.0
               | needCleaned need /= needRaw need
               ]
            ++ [ Probe joined "keywords" 1.0
               | joined /= needCleaned need
               , joined /= needRaw need
               ]
    pairs = [Probe b "bigram" 0.9 | b <- bigrams (needTerms need)]
    singles = [Probe t "unigram" 0.7 | t <- needTerms need]
    joined = T.unwords (needTerms need)

-- | The second wave: a package or module row is a lead, so ask again inside it.
-- This is the pivot the whole design turns on — Hoogle answers @parquet
-- dataframe@ with nothing but package rows, and the answer lives one scoped
-- query further in.
expandProbes :: Need -> HoogleHit -> [Probe]
expandProbes need h = case rowKind h of
    RowSymbol -> []
    RowPackage -> scopedInto (hhPackage h) "pivot:pkg"
    RowModule -> scopedInto (hhModule h) "pivot:mod"
  where
    scopedInto target via
        | T.null target || null terms = []
        | otherwise =
            Probe ("+" <> target <> " " <> T.unwords terms) via 1.0
                : [Probe ("+" <> target <> " " <> t) via 1.0 | t <- terms]
      where
        terms = take maxPivotTerms (needTerms need)

bigrams :: [Text] -> [Text]
bigrams ws = zipWith (\a b -> a <> " " <> b) ws (drop 1 ws)

dedupe :: [Probe] -> [Probe]
dedupe =
    nubBy (\a b -> norm a == norm b)
        . filter (not . T.null . T.strip . probeQuery)
  where
    -- Case is significant to Hoogle: a capitalised word searches types,
    -- a lower-case one searches names. Only exact repeats are dropped.
    norm = T.strip . probeQuery
