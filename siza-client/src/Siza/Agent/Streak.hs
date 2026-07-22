{- | Mid-loop wrong-vs-real contrast: fire 'contrastLine' when the SAME red
diagnostic persists on a cell for 'streakThreshold' turns — the stop-rail
contrast never reaches an episode that ends by max_turns while still working.
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

{- | Fold one red diagnostic into the per-cell streak map: an identical
diagnostic extends the cell's streak, a changed one restarts it. Returns the
cell's current streak length.
-}
bumpStreak ::
    Map CellId (Text, Int) -> CellId -> Text -> (Map CellId (Text, Int), Int)
bumpStreak m cid diag = (Map.insert cid (diag, n) m, n)
  where
    n = case Map.lookup cid m of
        Just (d, k) | d == diag -> k + 1
        _ -> 1

-- | Consecutive identical-diagnostic turns before the contrast fires.
streakThreshold :: Int
streakThreshold = 3

{- | The contrast for a cell EXACTLY at the threshold — once per streak, so the
same message is not re-injected every subsequent red turn.
-}
streakContrast :: Int -> Text -> Maybe Text
streakContrast n diag
    | n == streakThreshold = contrastLine diag
    | otherwise = Nothing
