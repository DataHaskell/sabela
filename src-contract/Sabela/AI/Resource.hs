{-# LANGUAGE OverloadedStrings #-}

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

data ResourceEvidence = ResourceEvidence
    { reElapsedMs :: !Int
    , reHeapBytes :: ![Int]
    , reEventsSeen :: !Int
    }
    deriving (Eq, Show)

defaultResourceWallSecs :: Int
defaultResourceWallSecs = 30

resourceWallBudgetMs :: IO Int
resourceWallBudgetMs = do
    m <- lookupEnv "SABELA_RESOURCE_WALL_SECS"
    pure (1000 * fromMaybe defaultResourceWallSecs (m >>= readMaybe))

heapClimbing :: [Int] -> Bool
heapClimbing (x0 : rest@(_ : _)) =
    and (zipWith (<=) (x0 : rest) rest) && foldl (\_ y -> y) x0 rest > x0
heapClimbing _ = False

resourceTriggered :: Int -> ResourceEvidence -> Bool
resourceTriggered budgetMs e =
    reElapsedMs e >= budgetMs
        && (heapClimbing (reHeapBytes e) || reEventsSeen e == 0)

resourceLine :: Int -> Maybe Text -> ResourceEvidence -> Maybe Text
resourceLine budgetMs mSubject e
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
    subject = fromMaybe "the running cell" mSubject
    evidenceClause =
        T.concat $
            ["," <> " heap climbing" | heapClimbing (reHeapBytes e)]
                <> [", no output" | reEventsSeen e == 0]

tshow :: (Show a) => a -> Text
tshow = T.pack . show
