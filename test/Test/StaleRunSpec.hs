{-# LANGUAGE OverloadedStrings #-}

module Test.StaleRunSpec (spec) where

import Sabela.Handlers (clearAllOutputs, updateCellSource)
import Sabela.Model (
    Cell (..),
    CellType (..),
    MimeType (MimePlain),
    Notebook (..),
    OutputItem (..),
 )
import Sabela.Reactivity (
    EnvState (..),
    ExecutionPlan (..),
    ModuleState (..),
    cellSettled,
    cellStale,
    clearCellResult,
    computeStaleExecutionPlan,
    computeStaleExecutionPlanIn,
    haskellCodeCells,
    runAllNeedsRun,
 )
import Test.CellFixture (mkCell)
import Test.Hspec

nbOf :: [Cell] -> Notebook
nbOf cells = Notebook{nbTitle = "t", nbCells = cells}

dirty :: Cell -> Cell
dirty c = c{cellDirty = True}

errored :: Cell -> Cell
errored c = c{cellError = Just "boom"}

prose :: Cell -> Cell
prose c = c{cellType = ProseCell}

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
        it "is False for a cell whose last run errored: settled, not stale" $
            cellStale (errored (mkCell 0 "x = 1")) `shouldBe` False
        it "is True for an errored cell whose source was then edited" $
            cellStale (dirty (errored (mkCell 0 "x = 1"))) `shouldBe` True

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
        it "leaves an errored clean cell alone: re-running repeats the failure" $
            planIds [errored (mkCell 0 "x = 1"), mkCell 1 "y = x + 1"]
                `shouldBe` []
        it "runs an errored cell again once its source changes" $
            planIds [dirty (errored (mkCell 0 "x = 1")), mkCell 1 "y = x + 1"]
                `shouldBe` [0, 1]
        it "deduplicates a dependent shared by two dirty roots" $
            planIds
                [ dirty (mkCell 0 "x = 1")
                , dirty (mkCell 1 "y = 2")
                , mkCell 2 "z = x + y"
                ]
                `shouldBe` [0, 1, 2]

    describe "runAllNeedsRun (idempotent run-all)" $ do
        let needs building ready cells =
                runAllNeedsRun building ready (haskellCodeCells (nbOf cells)) (nbOf cells)
        it "skips when a build/restart is already in flight (no restart thrash)" $
            needs True False [dirty (mkCell 0 "x = 1")] `shouldBe` False
        it "runs when the session is not ready (cold start needed)" $
            needs False False [mkCell 0 "x = 1"] `shouldBe` True
        it "skips a ready session with no stale cell (unchanged notebook)" $
            needs False True [mkCell 0 "x = 1", mkCell 1 "y = x + 1"] `shouldBe` False
        it "runs a ready session that has a stale cell" $
            needs False True [dirty (mkCell 0 "x = 1")] `shouldBe` True
        it "converges: a settled failure does not keep asking for a run-all" $
            needs False True [errored (mkCell 0 "x = 1"), mkCell 1 "y = x + 1"]
                `shouldBe` False
        it "runs a clean notebook whose environment went stale" $
            needs False False [mkCell 0 "x = 1"] `shouldBe` True

    describe "cellSettled (does the kernel reflect this cell?)" $ do
        it "is True only for a cell that ran to completion without error" $
            cellSettled (mkCell 0 "x = 1") `shouldBe` True
        it "is False for an edited cell" $
            cellSettled (dirty (mkCell 0 "x = 1")) `shouldBe` False
        it "is False for an errored cell, which cellStale no longer reports" $ do
            cellSettled (errored (mkCell 0 "x = 1")) `shouldBe` False
            cellStale (errored (mkCell 0 "x = 1")) `shouldBe` False

    describe "a stale environment is a root, not a side condition" $ do
        let planWith env cells =
                computeStaleExecutionPlanIn env ModulesLoaded cells (nbOf cells)
            cells = [mkCell 0 "x = 1", mkCell 1 "y = x + 1"]
        it "runs nothing when the kernel is the one the notebook needs" $ do
            let p = planWith EnvFresh cells
            map cellId (epCellsToRun p) `shouldBe` []
            epRunEnv p `shouldBe` False
        it "rebuilds the environment when it no longer matches" $
            epRunEnv (planWith EnvStale cells) `shouldBe` True
        it
            "re-runs every cell after a rebuild, even ones nobody edited:\
            \ a new kernel holds none of their bindings"
            $ map cellId (epCellsToRun (planWith EnvStale cells))
                `shouldBe` [0, 1]
        it
            "rebuilds even when there is no cell to run at all, so a notebook\
            \ that cannot execute can still recover its environment"
            $ do
                let p = planWith EnvStale []
                epRunEnv p `shouldBe` True
                map cellId (epCellsToRun p) `shouldBe` []
        it "rebuilds while still skipping cells the plan rejects" $ do
            let p = planWith EnvStale [mkCell 0 "x = 1", mkCell 1 "x = 2"]
            epRunEnv p `shouldBe` True
            map cellId (epCellsToRun p) `shouldBe` [0]

    describe "clearCellResult (Clear, Reset, and a language switch)" $ do
        let ran = (mkCell 0 "x = 1"){cellOutputs = [OutputItem MimePlain "1"]}
        it "drops the outputs it was showing" $
            cellOutputs (clearCellResult ran) `shouldBe` []
        it "drops a previous error" $
            cellError (clearCellResult (errored ran)) `shouldBe` Nothing
        it "invalidates a code cell: with no result it is not current" $
            cellDirty (clearCellResult ran) `shouldBe` True
        it "leaves a prose cell clean, since prose never runs" $
            cellDirty (clearCellResult (prose ran)) `shouldBe` False

    describe "clearAllOutputs (handleReset, which kills every session)" $ do
        let nb =
                nbOf
                    [ prose (mkCell 0 "# title")
                    , mkCell 1 "x = 1"
                    , errored (mkCell 2 "y = x + 1")
                    ]
            cleared = clearAllOutputs nb
        it "invalidates every code cell, because no kernel holds anything" $
            map cellDirty (nbCells cleared) `shouldBe` [False, True, True]
        it "leaves nothing claiming a result" $ do
            concatMap cellOutputs (nbCells cleared) `shouldBe` []
            map cellError (nbCells cleared) `shouldBe` [Nothing, Nothing, Nothing]

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
        it
            "marks transitive dependents dirty too, so a solo run of the\
            \ root cannot strand them"
            $ do
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
