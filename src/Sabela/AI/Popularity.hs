{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | How much of the ecosystem uses a package, measured rather than assumed:
downloads, which under-count anything GHC bundles, and reverse dependencies,
which are near zero for a young end-user library. Each covers the other.
-}
module Sabela.AI.Popularity (
    Popularity,
    PackagePrior (..),
    loadPopularity,
    popularityFromList,
    popularityOf,
    popularityRank,
    packagePrior,
    packageRankKey,
    emptyPopularity,
) where

import Control.Exception (SomeException, try)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Data.List (sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.FilePath ((</>))

{- | The table, and the median rank that places an unmeasured package against
it — absent when the measured ranks carry no spread at all.
-}
data Popularity = Popularity (Map Text (Int, Int)) (Maybe Int)

emptyPopularity :: Popularity
emptyPopularity = popularityFromList []

popularityFromList :: [(Text, (Int, Int))] -> Popularity
popularityFromList = fromTable . Map.fromList

fromTable :: Map Text (Int, Int) -> Popularity
fromTable m = Popularity m (spreadMedian (map rankOf (Map.elems m)))

{- | The median measured rank, or nothing when every measured package ranks
alike: a table with no spread has not discriminated between anything.
-}
spreadMedian :: [Int] -> Maybe Int
spreadMedian rs
    | null rs || minimum rs == maximum rs = Nothing
    | otherwise = Just (sort rs !! (length rs `div` 2))

popularityPath :: IO FilePath
popularityPath =
    maybe ("data" </> "hackage-popularity.json") (</> "hackage-popularity.json")
        <$> lookupEnv "SABELA_DATA_DIR"

{- | Read the table once; a caller holds the result. A miss leaves an empty
table, which ranks everything equally rather than pretending to a signal.
-}
loadPopularity :: IO Popularity
loadPopularity = do
    path <- popularityPath
    ok <- doesFileExist path
    if not ok
        then pure emptyPopularity
        else do
            r <- try (BL.readFile path)
            pure $ case r of
                Left (_ :: SomeException) -> emptyPopularity
                Right bs -> case A.decode bs of
                    Just m -> fromTable (Map.map pair m)
                    Nothing -> emptyPopularity
  where
    pair :: [Int] -> (Int, Int)
    pair (d : r : _) = (d, r)
    pair [d] = (d, 0)
    pair [] = (0, 0)

popularityOf :: Popularity -> Text -> (Int, Int)
popularityOf (Popularity m _) pkg = Map.findWithDefault (0, 0) pkg m

{- | A small non-negative rank, higher meaning more used. The two signals
correlate at only r=0.50 over the 14,409 packages carrying both, so the sum of
their logs discriminates better than either alone.
-}
popularityRank :: Popularity -> Text -> Int
popularityRank p pkg = rankOf (popularityOf p pkg)

rankOf :: (Int, Int) -> Int
rankOf (downloads, revdeps) = round (10 * (logish downloads + logish revdeps))
  where
    logish :: Int -> Double
    logish n = log (1 + fromIntegral (max 0 n))

{- | Where a package sits against the measured population: above its median,
absent from it, or below it. Absence is its own band because a table that has
never heard of a package has not measured that nothing uses it.
-}
data PackagePrior
    = WellUsed Int
    | Unmeasured
    | LittleUsed Int
    deriving (Eq, Show)

-- | A table that has not discriminated leaves every package unmeasured.
packagePrior :: Popularity -> Text -> PackagePrior
packagePrior p@(Popularity m mMed) pkg
    | Just med <- mMed
    , Map.member pkg m =
        if r >= med then WellUsed r else LittleUsed r
    | otherwise = Unmeasured
  where
    r = popularityRank p pkg

{- | An ascending sort key: the band leads, and within a band the measured rank
descends, so a more-used package sorts first.
-}
priorKey :: PackagePrior -> (Int, Int)
priorKey (WellUsed r) = (0, negate r)
priorKey Unmeasured = (1, 0)
priorKey (LittleUsed r) = (2, negate r)

packageRankKey :: Popularity -> Text -> (Int, Int)
packageRankKey p = priorKey . packagePrior p
