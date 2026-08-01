{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | How much of the ecosystem uses a package.

Two signals, because they disagree and each covers the other's blind spot.
Downloads measure how many people fetched a package: they under-count anything
GHC bundles (@base@ and @random@ look tiny) and over-count build-chain traffic.
Reverse dependencies measure how much of Hackage builds on it: they are near
zero for a young end-user library, which is exactly the class a notebook reaches
for — @dataframe@ has 322 downloads and three reverse dependencies.

Measured over the 14,409 packages carrying both, they correlate at only r=0.50,
so the sum of their logs discriminates better than either alone. Absent means
zero, never unknown: nothing depending on a package is a fact about it.
-}
module Sabela.AI.Popularity (
    Popularity,
    loadPopularity,
    popularityOf,
    popularityRank,
    emptyPopularity,
) where

import Control.Exception (SomeException, try)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.FilePath ((</>))

newtype Popularity = Popularity (Map Text (Int, Int))

emptyPopularity :: Popularity
emptyPopularity = Popularity Map.empty

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
                    Just m -> Popularity (Map.map pair m)
                    Nothing -> emptyPopularity
  where
    pair :: [Int] -> (Int, Int)
    pair (d : r : _) = (d, r)
    pair [d] = (d, 0)
    pair [] = (0, 0)

popularityOf :: Popularity -> Text -> (Int, Int)
popularityOf (Popularity m) pkg = Map.findWithDefault (0, 0) pkg m

{- | A small non-negative rank, higher meaning more used. Scaled so it can sit
in an ordering key beside integer terms without swamping them.
-}
popularityRank :: Popularity -> Text -> Int
popularityRank p pkg = round (10 * (logish downloads + logish revdeps))
  where
    (downloads, revdeps) = popularityOf p pkg
    logish :: Int -> Double
    logish n = log (1 + fromIntegral (max 0 n))
