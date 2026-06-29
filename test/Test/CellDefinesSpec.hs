{-# LANGUAGE OverloadedStrings #-}

{- | The @list_cells@ pure projections: 'cellDefines' (top-level binding names,
so the weak model reuses real names) and 'cellListEntry' (a cell's metadata plus
its FULL source, so the model reads the whole notebook in one call instead of a
@read_cell@ per cell). Both pure.
-}
module Test.CellDefinesSpec (spec) where

import Data.Aeson (Value (..), toJSON)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.Capabilities.Notebook (
    cellDefines,
    cellListEntry,
    listCellSourceCap,
 )
import Sabela.Model (Cell (..), CellType (..))
import Sabela.SessionTypes (CellLang (..))
import Test.Hspec

codeCell :: Text -> Cell
codeCell src = Cell 1 CodeCell Haskell src [] Nothing False

proseCell :: Text -> Cell
proseCell src = Cell 1 ProseCell Haskell src [] Nothing False

pyCell :: Text -> Cell
pyCell src = Cell 1 CodeCell Python src [] Nothing False

spec :: Spec
spec = describe "Sabela.AI.Capabilities.Notebook.cellDefines" $ do
    it "lists the top-level binding names of a Haskell code cell, sorted" $
        cellDefines (codeCell "points = [1, 2, 3]\nsquare n = n * n")
            `shouldBe` ["points", "square"]

    it "surfaces a data/type name so the model can reuse it" $
        cellDefines (codeCell "data Tree = Leaf | Node Tree Tree")
            `shouldContain` ["Tree"]

    it "is empty for a bare expression (introduces no binding)" $
        cellDefines (codeCell "print (1 + 2)") `shouldBe` []

    it "is empty for a ProseCell even if it parses as code" $
        cellDefines (proseCell "x = 1") `shouldBe` []

    it "is empty for a Python code cell (the Haskell parser does not apply)" $
        cellDefines (pyCell "x = 1") `shouldBe` []

    describe "cellListEntry" $ do
        let field k = KM.lookup (Key.fromText k) . asObject
            asObject (Object o) = o
            asObject _ = error "cellListEntry: not an object"

        it "previews (full=False) with first line only, no source" $ do
            let src = T.unlines ["firstLine = 1", "secondLine = 2", "thirdLine = 3"]
                e = cellListEntry False 1 (codeCell src)
            field "source" e `shouldBe` Nothing
            field "firstLine" e `shouldBe` Just (String "firstLine = 1")
            field "lineCount" e `shouldBe` Just (Number 3)

        it "returns the cell's FULL multi-line source when full=True" $ do
            let src = T.unlines ["firstLine = 1", "secondLine = 2", "thirdLine = 3"]
                e = cellListEntry True 1 (codeCell src)
            field "source" e `shouldBe` Just (String src)
            field "firstLine" e `shouldBe` Nothing

        it "carries the metadata the model needs to act" $ do
            let e = cellListEntry True 4 (codeCell "answer = 42")
            field "position" e `shouldBe` Just (Number 4)
            field "defines" e `shouldBe` Just (toJSON ["answer" :: Text])
            field "truncated" e `shouldBe` Nothing

        it "bounds an oversized cell's source (full=True) and flags it for read_cell" $ do
            let big = T.replicate (listCellSourceCap * 3) "x"
                e = cellListEntry True 1 (codeCell big)
            field "truncated" e `shouldBe` Just (Bool True)
            case field "source" e of
                Just (String s) -> do
                    T.length s `shouldSatisfy` (< listCellSourceCap + 80)
                    s `shouldSatisfy` T.isInfixOf "read_cell"
                other -> expectationFailure ("expected truncated source string, got " <> show other)
