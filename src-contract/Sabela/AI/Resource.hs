{-# LANGUAGE OverloadedStrings #-}

{- | The @resource@ runaway diagnostic class (R3.9/R6.5): when a cell exceeds
its wall budget with climbing heap or no output progress, ONE bounded line
tells the proposer to interrupt or shrink its own proposal. The trigger is a
function of the resource evidence alone — never of cell content or task —
so it applies to any runaway, in any library.
-}
module Sabela.AI.Resource (
    ResourceEvidence (..),
    resourceTriggered,
    heapClimbing,
    resourceLine,
    resourceWallBudgetMs,
    defaultResourceWallSecs,
) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

{- | What the harness observed about the running cell: wall time since the
run began, heap-byte samples (oldest first; empty when unavailable), and how
many execution events (output, cell progress) the last await window saw.
-}
data ResourceEvidence = ResourceEvidence
    { reElapsedMs :: !Int
    , reHeapBytes :: ![Int]
    , reEventsSeen :: !Int
    }
    deriving (Eq, Show)

-- | Default wall budget before a silent or heap-climbing run is flagged.
defaultResourceWallSecs :: Int
defaultResourceWallSecs = 30

-- | The wall budget in ms; @SABELA_RESOURCE_WALL_SECS@ overrides.
resourceWallBudgetMs :: IO Int
resourceWallBudgetMs = do
    m <- lookupEnv "SABELA_RESOURCE_WALL_SECS"
    pure (1000 * fromMaybe defaultResourceWallSecs (m >>= readMaybe))

-- | Monotone non-decreasing with real growth, over at least two samples.
heapClimbing :: [Int] -> Bool
heapClimbing (x0 : rest@(_ : _)) =
    and (zipWith (<=) (x0 : rest) rest) && foldl (\_ y -> y) x0 rest > x0
heapClimbing _ = False

{- | The trigger law: wall budget exceeded AND the run shows runaway evidence
(heap climbing, or zero observed progress events).
-}
resourceTriggered :: Int -> ResourceEvidence -> Bool
resourceTriggered budgetMs e =
    reElapsedMs e >= budgetMs
        && (heapClimbing (reHeapBytes e) || reEventsSeen e == 0)

{- | The single bounded diagnostic line: one line, <= 200 chars, naming the
cell (when known), the elapsed wall time, the evidence, and the general
recovery action. Never mentions cell content.
-}
resourceLine :: Int -> Maybe Int -> ResourceEvidence -> Maybe Text
resourceLine budgetMs mCid e
    | not (resourceTriggered budgetMs e) = Nothing
    | otherwise =
        Just $
            T.take 200 $
                subject
                    <> " executing "
                    <> tshow (reElapsedMs e `div` 1000)
                    <> "s"
                    <> evidenceClause
                    <> " - likely non-terminating or combinatorially \
                       \explosive; interrupt, then shrink the work \
                       \(smaller depth/bounds) and rewrite the cell."
  where
    subject = maybe "the running cell" (\cid -> "cell " <> tshow cid) mCid
    evidenceClause =
        T.concat $
            ["," <> " heap climbing" | heapClimbing (reHeapBytes e)]
                <> [", no output" | reEventsSeen e == 0]

tshow :: (Show a) => a -> Text
tshow = T.pack . show
