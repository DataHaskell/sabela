{- |
Technique: cell-layer repeat detector [Gating/Repair].
Guarantee: a repeated red signature on one cell surfaces a contrast hint at the threshold.
Entry: 'bumpStreak'. Sibling: 'Siza.Agent.Owned.noProgressStep' (episode layer).
-}
module Siza.Agent.Streak (
    bumpStreak,
    streakContrast,
    streakThreshold,
) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Sabela.AI.CellResult (CellId)
import Sabela.AI.SelfHeal (contrastLine)

bumpStreak ::
    Map CellId (Text, Int) -> CellId -> Text -> (Map CellId (Text, Int), Int)
bumpStreak m cid diag = (Map.insert cid (diag, n) m, n)
  where
    n = case Map.lookup cid m of
        Just (d, k) | d == diag -> k + 1
        _ -> 1

streakThreshold :: Int
streakThreshold = 3

streakContrast :: Int -> Text -> Maybe Text
streakContrast n diag
    | n == streakThreshold = contrastLine diag
    | otherwise = Nothing
