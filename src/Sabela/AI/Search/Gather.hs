-- | Fan out over every probe, fuse the results, rank once.
--
-- The rule this module exists to enforce: arrival order is not relevance.
-- Probes do not compete to answer first; they all run, and a row found by
-- several of them is corroborated rather than shadowed. A row that shares no
-- term with the query is not an answer and is never returned.
module Sabela.AI.Search.Gather (
    Retriever,
    searchNeed,
    searchNeedWith,
    fuse,
    rankEvidence,
    Evidence (..),
    rrfK,
) where

import Data.List (foldl', sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.HoogleClient (HoogleHit (..))
import Sabela.AI.HoogleRank (keepHit)
import Sabela.AI.Popularity (
    Popularity,
    emptyPopularity,
    popularityRank,
 )
import Sabela.AI.Search.Need (Need (..), isTypeOrName)
import Sabela.AI.Search.Probe (
    Probe (..),
    expandProbes,
    maxPivotProbes,
    planProbes,
 )
import Sabela.AI.Search.Row (
    coverage,
    grounded,
    kindPrior,
    rowKind,
    termsCovered,
 )

type Retriever = Int -> Text -> IO [HoogleHit]

data Evidence = Evidence
    { evRow :: HoogleHit
    , evRrf :: Double
    , evVia :: [Text]
    , evSeen :: Int
    }

-- | Reciprocal-rank-fusion damping. Matches the constant the capability
-- reranker already uses, so the two agree when their outputs are compared.
rrfK :: Double
rrfK = 60

type RowKey = (Text, Text, Text)

rowKey :: HoogleHit -> RowKey
rowKey h = (T.toLower (hhName h), hhModule h, hhPackage h)

-- | Run the whole probe frontier and return the ranked, grounded rows.
searchNeed :: Retriever -> Int -> Need -> IO [HoogleHit]
searchNeed = searchNeedWith emptyPopularity

searchNeedWith :: Popularity -> Retriever -> Int -> Need -> IO [HoogleHit]
searchNeedWith pop run k need
    | T.null (needRaw need) = pure []
    -- A type-shaped query is already precise; Hoogle's type search answers it
    -- directly and its terms do not decompose into meaningful words.
    | isTypeOrName (needRaw need) =
        take k . filter keepHit <$> run k (needRaw need)
    | otherwise = do
        wave1 <- runProbes run k (planProbes need)
        let leads = pivotProbes need (map evRow (Map.elems wave1))
            asked = Set.fromList (map probeQuery (planProbes need))
            fresh = filter ((`Set.notMember` asked) . probeQuery) leads
        wave2 <- runProbes run k fresh
        pure (take k (rankEvidence pop need (Map.unionWith fuse wave1 wave2)))

runProbes :: Retriever -> Int -> [Probe] -> IO (Map.Map RowKey Evidence)
runProbes run k = fmap (foldl' (Map.unionWith fuse) Map.empty) . mapM one
  where
    one p = harvest p . filter keepHit <$> run k (probeQuery p)

harvest :: Probe -> [HoogleHit] -> Map.Map RowKey Evidence
harvest p hits =
    Map.fromListWith
        fuse
        [ (rowKey h, single p rank h)
        | (rank, h) <- zip [1 :: Int ..] hits
        ]

single :: Probe -> Int -> HoogleHit -> Evidence
single p rank h =
    Evidence
        { evRow = h
        , evRrf = probeWeight p / (rrfK + fromIntegral rank)
        , evVia = [probeVia p]
        , evSeen = 1
        }

-- | Corroboration, not replacement: two probes finding the same row make it a
-- stronger candidate than either alone.
fuse :: Evidence -> Evidence -> Evidence
fuse a b =
    Evidence
        { evRow = if richer (evRow a) (evRow b) then evRow a else evRow b
        , evRrf = evRrf a + evRrf b
        , evVia = evVia a <> evVia b
        , evSeen = evSeen a + evSeen b
        }
  where
    richer x y = T.length (hhType x) + T.length (hhDocs x) >= T.length (hhType y) + T.length (hhDocs y)

-- | Grounded rows only, best first.
--
-- 'kindPrior' outranks coverage deliberately: a package whose name happens to
-- concatenate the query terms (@dataframe-parquet@ for \"parquet dataframe\")
-- would otherwise bury the function that does the work. A lead is never a
-- better answer than a symbol, only a better next question.
rankEvidence :: Popularity -> Need -> Map.Map RowKey Evidence -> [HoogleHit]
rankEvidence pop need =
    map evRow . sortOn key . filter (grounded need . evRow) . Map.elems
  where
    key e =
        ( outOfScope (evRow e)
        , kindPrior (rowKind (evRow e))
        , negate (termsCovered need (evRow e))
        , negate (coverage need (evRow e))
        , T.length (hhName (evRow e))
        , negate (evRrf e)
        , negate (popularityRank pop (hhPackage (evRow e)))
        , T.length (hhModule (evRow e))
        , hhName (evRow e)
        )
    outOfScope h =
        if hhPackage h `Set.member` needScope need then 0 else 1 :: Int

-- | Spend the pivot budget on the leads that best explain the query; the
-- evidence map has no meaningful order of its own.
pivotProbes :: Need -> [HoogleHit] -> [Probe]
pivotProbes need =
    take maxPivotProbes
        . concatMap (expandProbes need)
        . sortOn (negate . coverage need)
        . filter (grounded need)
