{-# LANGUAGE OverloadedStrings #-}

{- | Pins the execute_cell run-target contract. A forced run (the AI
@execute_cell@ tool) executes an existing cell even when clean; an unforced
run (browser / reactive flush) skips a clean code cell; a missing cell never
runs. And execute_cell fails fast with a clear, id-naming message instead of
waiting out the 130s listener on a result event that will never broadcast.
-}
module Test.ExecuteCellSpec (spec) where

import Sabela.AI.Capabilities.Edit.Run (missingCellError)
import Sabela.Handlers (cellRunnable)
import Sabela.Model (Cell (..), CellType (..))
import Test.Hspec
import Test.TopoSpec.Helpers (mkCell)

dirty :: Cell -> Cell
dirty c = c{cellDirty = True}

errored :: Cell -> Cell
errored c = c{cellError = Just "boom"}

prose :: Cell -> Cell
prose c = c{cellType = ProseCell}

spec :: Spec
spec = do
    describe "cellRunnable (handleRunCell dispatch decision)" $ do
        it "never runs a missing cell, even forced" $ do
            cellRunnable False Nothing `shouldBe` False
            cellRunnable True Nothing `shouldBe` False
        it "skips a clean code cell on an unforced run" $
            cellRunnable False (Just (mkCell 0 "x = 1")) `shouldBe` False
        it "forces a clean code cell to run (the execute_cell contract)" $
            cellRunnable True (Just (mkCell 0 "x = 1")) `shouldBe` True
        it "runs a dirty code cell either way" $ do
            cellRunnable False (Just (dirty (mkCell 0 "x = 1"))) `shouldBe` True
            cellRunnable True (Just (dirty (mkCell 0 "x = 1"))) `shouldBe` True
        it "runs an errored code cell either way" $
            cellRunnable False (Just (errored (mkCell 0 "x = 1"))) `shouldBe` True
        it "runs a prose cell either way (non-code always runs)" $ do
            cellRunnable False (Just (prose (mkCell 0 "hi"))) `shouldBe` True
            cellRunnable True (Just (prose (mkCell 0 "hi"))) `shouldBe` True

    describe "missingCellError (execute_cell fast-fail)" $ do
        let cells = [mkCell 1 "x = 1", mkCell 3 "y = 2"]
        it "is Nothing when the target exists" $ do
            missingCellError cells 1 `shouldBe` Nothing
            missingCellError cells 3 `shouldBe` Nothing
        it "names the missing id so the failure is obvious" $
            missingCellError cells 8 `shouldBe` Just "No cell with id 8"
        it "fails against an empty notebook" $
            missingCellError [] 0 `shouldBe` Just "No cell with id 0"
