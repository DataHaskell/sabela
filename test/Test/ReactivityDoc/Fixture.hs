{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Shared helpers for the specs that pin @reactivity.md@: plan a notebook from
an edit and mark a documented semantic the planner does not yet honour.
-}
module Test.ReactivityDoc.Fixture (
    nbOf,
    planFor,
    planOf,
    stalePlanFor,
    knownGap,
    indexOf,
) where

import Control.Exception (SomeException, try)
import Data.List (elemIndex)
import Sabela.Model (Cell (..), Notebook (..))
import Sabela.Reactivity (
    ExecutionPlan (..),
    computeExecutionPlan,
    computeStaleExecutionPlan,
 )
import Test.Hspec (Expectation, expectationFailure, pendingWith)

nbOf :: [Cell] -> Notebook
nbOf cs = Notebook{nbTitle = "t", nbCells = cs}

planOf :: [Cell] -> Int -> ExecutionPlan
planOf cs cid = computeExecutionPlan cid cs (nbOf cs)

-- | The cells an edit of @cid@ runs, in execution order.
planFor :: [Cell] -> Int -> [Int]
planFor cs cid = map cellId (epCellsToRun (planOf cs cid))

-- | What a run-all would execute given the cells' current dirty flags.
stalePlanFor :: [Cell] -> [Int]
stalePlanFor cs = map cellId (epCellsToRun (computeStaleExecutionPlan cs (nbOf cs)))

{- | A semantic @reactivity.md@ promises that the code does not deliver yet. The
item stays pending while the gap is open and fails once it closes, so the marker
is removed the moment the behaviour lands rather than lingering as a false skip.
-}
knownGap :: String -> Expectation -> Expectation
knownGap why expectation = do
    outcome <- try expectation
    case outcome of
        Left (_ :: SomeException) -> pendingWith ("known gap: " <> why)
        Right () ->
            expectationFailure
                ("gap closed, remove the knownGap marker: " <> why)

indexOf :: Int -> [Int] -> Maybe Int
indexOf = elemIndex
