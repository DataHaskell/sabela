{-# LANGUAGE OverloadedStrings #-}

{- | 'cellDefines' (N1): the @list_cells@ @defines@ projection that surfaces a
Haskell code cell's top-level binding names, so the weak model reuses real
names instead of inventing them. Pure; gated to Haskell code cells.
-}
module Test.CellDefinesSpec (spec) where

import Data.Text (Text)
import Sabela.AI.Capabilities.Notebook (cellDefines)
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
