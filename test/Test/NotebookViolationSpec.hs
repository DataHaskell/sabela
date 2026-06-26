{-# LANGUAGE OverloadedStrings #-}

{- | The AI insert contract as an illegal-states-unrepresentable chokepoint:
'checkedAppend' appends a cell at the end and refuses when the notebook has a
cell with a settled error (fix/delete it first) or the candidate duplicates a
definition. Pure, so no kernel is needed.
-}
module Test.NotebookViolationSpec (spec) where

import Data.Either (isRight)
import Data.Text (Text)
import Sabela.Handlers.Shared (
    DefConflict (..),
    NotebookViolation (..),
    checkedAppend,
    pendingError,
 )
import Sabela.Model (Cell (..), CellType (..), Notebook (..))
import Sabela.SessionTypes (CellLang (..))
import Test.Hspec

hsCell :: Int -> Text -> Cell
hsCell i src = Cell i CodeCell Haskell src [] Nothing False

redCell :: Int -> Text -> Cell
redCell i err = (hsCell i "broken = ()"){cellError = Just err}

nb :: [Cell] -> Notebook
nb = Notebook "t"

spec :: Spec
spec = describe "Sabela.Handlers.Shared — AI insert invariants" $ do
    it "appends a fresh cell at the END of a clean notebook (not prepend)" $
        case checkedAppend (hsCell 9 "y = 2") (nb [hsCell 1 "x = 1"]) of
            Right nb' -> map cellId (nbCells nb') `shouldBe` [1, 9]
            Left v -> expectationFailure ("unexpectedly rejected: " <> show v)

    it "rejects insert while a cell has a settled error (the illegal state)" $
        case checkedAppend (hsCell 9 "y = 2") (nb [redCell 1 "boom", hsCell 2 "x = 1"]) of
            Left (VPendingError cid _) -> cid `shouldBe` 1
            other -> expectationFailure ("expected VPendingError, got " <> show other)

    it "rejects a duplicate top-level definition" $
        case checkedAppend (hsCell 9 "x = 2") (nb [hsCell 1 "x = 1"]) of
            Left (VDuplicateDef dc) -> dcName dc `shouldBe` "x"
            other -> expectationFailure ("expected VDuplicateDef, got " <> show other)

    it "the error-gate clears once the red cell is removed (delete unblocks)" $ do
        let cleaned = nb [hsCell 2 "x = 1"]
        pendingError cleaned `shouldBe` Nothing
        checkedAppend (hsCell 9 "y = 2") cleaned `shouldSatisfy` isRight

    it "a pending error takes priority over a duplicate (fix the error first)" $
        case checkedAppend (hsCell 9 "x = 2") (nb [redCell 5 "boom", hsCell 1 "x = 1"]) of
            Left (VPendingError cid _) -> cid `shouldBe` 5
            other -> expectationFailure ("expected VPendingError, got " <> show other)
