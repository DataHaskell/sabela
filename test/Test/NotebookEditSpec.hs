{-# LANGUAGE OverloadedStrings #-}

{- | The checked notebook mutations: a duplicate top-level binding cannot be
committed through 'insertCellChecked' or 'setCellSourceChecked'. The success
branch is the only one that yields a 'Notebook', so the conflicting state is
unrepresentable through the constructor — every discrete add/replace path in
the server routes through these.
-}
module Test.NotebookEditSpec (spec) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Either (isRight)
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.Capabilities.Edit (conflictJson)
import Sabela.Api (InsertAt (..))
import Sabela.Handlers.Shared (
    DefConflict (..),
    insertCellChecked,
    setCellSourceChecked,
 )
import Sabela.Model (Cell (..), CellType (..), Notebook (..))
import Sabela.SessionTypes (CellLang (..))
import Test.Hspec

-- 'codeCell' / 'proseCell' build a code or prose cell with the given source.
codeCell :: Int -> Text -> Cell
codeCell cid src = Cell cid CodeCell Haskell src [] Nothing True

proseCell :: Int -> Text -> Cell
proseCell cid src = Cell cid ProseCell Haskell src [] Nothing True

c1, c2 :: Cell
c1 = Cell 1 CodeCell Haskell "total = 1" [] Nothing True
c2 = Cell 2 CodeCell Haskell "other = 2" [] Nothing True

base :: Notebook
base = Notebook "t.md" [c1, c2]

spec :: Spec
spec = describe "checked notebook mutations (duplicate-def is uncommittable)" $ do
    describe "insertCellChecked" $ do
        it "inserts a cell whose bindings are all fresh" $
            insertCellChecked AtBeginning (codeCell 9 "extra = 2") base
                `shouldSatisfy` isRight

        it "rejects a cell that redefines a binding another cell owns" $
            insertCellChecked (After 1) (codeCell 9 "total = 99") base
                `shouldBe` Left (DefConflict "total" 1)

        it "detects the conflict regardless of insert position (before the owner)" $
            insertCellChecked AtBeginning (codeCell 9 "total = 99") base
                `shouldBe` Left (DefConflict "total" 1)

        it "never conflicts on a prose cell (it defines no bindings)" $
            insertCellChecked AtBeginning (proseCell 9 "total is the sum") base
                `shouldSatisfy` isRight

    describe "setCellSourceChecked" $ do
        it "rejects changing a cell to redefine another cell's binding" $
            fmap fst (setCellSourceChecked c2 "total = 7" base)
                `shouldBe` Left (DefConflict "total" 1)

        it "allows editing the owner cell's own definition (no self-conflict)" $
            setCellSourceChecked c1 "total = 7" base `shouldSatisfy` isRight

        it "allows changing a cell to an all-fresh binding" $
            setCellSourceChecked c2 "fresh = 3" base `shouldSatisfy` isRight

        it "clears outputs and marks the changed cell dirty on success" $
            case setCellSourceChecked c2 "fresh = 3" base of
                Right (_, newCell) -> do
                    cellSource newCell `shouldBe` "fresh = 3"
                    cellDirty newCell `shouldBe` True
                    cellOutputs newCell `shouldBe` []
                Left _ -> expectationFailure "expected a successful source change"

    describe "conflictJson (the actionable reject the model reads)" $ do
        let v = conflictJson (DefConflict "housing" 3)
        it "names the binding and owner cell, and points at replace_cell_source" $ do
            ("housing" `T.isInfixOf` errorText v) `shouldBe` True
            ("cell 3" `T.isInfixOf` errorText v) `shouldBe` True
            ("replace_cell_source" `T.isInfixOf` errorText v) `shouldBe` True
        it "carries the structured binding and ownerCell fields" $ do
            field "binding" v `shouldBe` Just (String "housing")
            field "ownerCell" v `shouldBe` Just (Number 3)

-- | The @error@ string of a reject payload, or empty if absent.
errorText :: Value -> Text
errorText v = case field "error" v of
    Just (String s) -> s
    _ -> ""

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (Key.fromText k) o
field _ _ = Nothing
