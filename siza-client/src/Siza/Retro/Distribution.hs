{- | The shape of an episode's payload spend, not just its total. A mean hides
the answer that cost ten times the median, which is the one a budget is set
for.
-}
module Siza.Retro.Distribution (
    Spread (..),
    payloadSpread,
    quantile,
    spreadOf,
) where

import Data.List (sort)
import qualified Data.Map.Strict as M
import Data.Text (Text)

data Spread = Spread
    { spCount :: Int
    , spMedian :: Int
    , spP90 :: Int
    , spMax :: Int
    , spTotal :: Int
    }
    deriving (Eq, Show)

{- | Per-tool spread over the sizes each of its results actually carried.
Keyed the way 'Siza.Retro.Metrics.tmPayloadPerTool' is keyed, so a reader can
put the two side by side.
-}
payloadSpread :: [(Text, Int)] -> M.Map Text Spread
payloadSpread rows =
    M.mapMaybe spreadOf (M.fromListWith (++) [(t, [n]) | (t, n) <- rows])

{- | A sample with nothing in it has no median, no maximum and no shape, so it
reports none rather than a row of zeroes that reads like a measurement.
-}
spreadOf :: [Int] -> Maybe Spread
spreadOf [] = Nothing
spreadOf ns =
    Spread
        <$> Just (length ns)
        <*> quantile 50 sorted
        <*> quantile 90 sorted
        <*> Just (last sorted)
        <*> Just (sum sorted)
  where
    sorted = sort ns

{- | The nearest-rank quantile: the smallest observation at or above the given
percentage of the sample. No interpolation, so every number reported is a
size some result really was — and an empty sample has none.
-}
quantile :: Int -> [Int] -> Maybe Int
quantile _ [] = Nothing
quantile p sorted = Just (sorted !! max 0 (min (n - 1) (rank - 1)))
  where
    n = length sorted
    rank = ceiling (fromIntegral (p * n) / 100 :: Double)
