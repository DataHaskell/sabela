{-# LANGUAGE OverloadedStrings #-}

{- | Pins the stale-run contract: run-all re-executes only cells whose
source changed since their last run (or whose last run errored), plus
their transitive dependents; everything else keeps its outputs.
-}
module Test.StaleRunSpec (spec) where

import Sabela.Handlers (updateCellSource)
import Sabela.Model (Cell (..), Notebook (..))
import Sabela.Reactivity (
    ExecutionPlan (..),
    cellStale,
    computeStaleExecutionPlan,
 )
import Test.Hspec
import Test.TopoSpec.Helpers (mkCell)

nbOf :: [Cell] -> Notebook
nbOf cells = Notebook{nbTitle = "t", nbCells = cells}

dirty :: Cell -> Cell
dirty c = c{cellDirty = True}

errored :: Cell -> Cell
errored c = c{cellError = Just "boom"}

planIds :: [Cell] -> [Int]
planIds cells =
    map cellId (epCellsToRun (computeStaleExecutionPlan cells (nbOf cells)))

spec :: Spec
spec = do
    describe "cellStale" $ do
        it "is False for a clean, successfully-run cell" $
            cellStale (mkCell 0 "x = 1") `shouldBe` False
        it "is True for an edited cell" $
            cellStale (dirty (mkCell 0 "x = 1")) `shouldBe` True
        it "is True for a cell whose last run errored" $
            cellStale (errored (mkCell 0 "x = 1")) `shouldBe` True

    describe "computeStaleExecutionPlan" $ do
        it "runs nothing when every cell is clean" $
            planIds [mkCell 0 "x = 1", mkCell 1 "y = x + 1"]
                `shouldBe` []
        it "runs a dirty cell and its transitive dependents, in order" $
            planIds
                [ dirty (mkCell 0 "x = 1")
                , mkCell 1 "y = x + 1"
                , mkCell 2 "z = y + 1"
                ]
                `shouldBe` [0, 1, 2]
        it "leaves unrelated clean cells out of the plan" $
            planIds
                [ dirty (mkCell 0 "x = 1")
                , mkCell 1 "a = 42"
                , mkCell 2 "y = x + 1"
                ]
                `shouldBe` [0, 2]
        it "runs a dirty leaf alone" $
            planIds [mkCell 0 "x = 1", dirty (mkCell 1 "y = x + 1")]
                `shouldBe` [1]
        it "treats an errored clean cell as a root" $
            planIds [errored (mkCell 0 "x = 1"), mkCell 1 "y = x + 1"]
                `shouldBe` [0, 1]
        it "deduplicates a dependent shared by two dirty roots" $
            planIds
                [ dirty (mkCell 0 "x = 1")
                , dirty (mkCell 1 "y = 2")
                , mkCell 2 "z = x + y"
                ]
                `shouldBe` [0, 1, 2]

    describe "updateCellSource" $ do
        it "keeps a cell clean when the source is unchanged" $ do
            let nb = nbOf [mkCell 0 "x = 1"]
                nb' = updateCellSource 0 "x = 1" nb
            map cellDirty (nbCells nb') `shouldBe` [False]
        it "marks a cell dirty when the source changes" $ do
            let nb = nbOf [mkCell 0 "x = 1"]
                nb' = updateCellSource 0 "x = 2" nb
            map cellDirty (nbCells nb') `shouldBe` [True]
            map cellSource (nbCells nb') `shouldBe` ["x = 2"]
        it "marks transitive dependents dirty too, so a solo run of the\
           \ root cannot strand them" $ do
            let nb =
                    nbOf
                        [ mkCell 0 "x = 1"
                        , mkCell 1 "y = x + 1"
                        , mkCell 2 "a = 42"
                        ]
                nb' = updateCellSource 0 "x = 2" nb
            map cellDirty (nbCells nb') `shouldBe` [True, True, False]
        it "marks nothing on an identical write" $ do
            let nb = nbOf [mkCell 0 "x = 1", mkCell 1 "y = x + 1"]
                nb' = updateCellSource 0 "x = 1" nb
            map cellDirty (nbCells nb') `shouldBe` [False, False]
