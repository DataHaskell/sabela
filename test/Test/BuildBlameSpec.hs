{-# LANGUAGE OverloadedStrings #-}

module Test.BuildBlameSpec (spec) where

import Sabela.Deps (blameCells, depDeclaringCells)
import Sabela.Model (Notebook (..))
import Test.CellFixture (mkCell, proseCell)
import Test.Hspec

-- | Two dependency-declaring cells and two that declare nothing.
nb :: Notebook
nb =
    Notebook
        "t"
        [ proseCell 0 "some prose"
        , mkCell 1 "-- cabal: build-depends: text\nimport Data.Text"
        , mkCell 2 "x = 1"
        , mkCell 3 "-- cabal: build-depends: vector, containers\ny = 2"
        ]

spec :: Spec
spec = do
    describe "depDeclaringCells (which cell asked for what)" $ do
        it "maps each cell to the dependencies it alone declares" $
            depDeclaringCells nb
                `shouldBe` [(1, ["text"]), (3, ["containers", "vector"])]

        it "omits cells that declare none, including prose" $
            map fst (depDeclaringCells nb) `shouldBe` [1, 3]

    describe "blameCells (where a build failure belongs)" $ do
        it
            "blames the cell whose package the compiler named, not the whole\
            \ notebook: that cell is the one the user must edit"
            $ blameCells nb "cabal: Could not resolve dependency vector"
                `shouldBe` [3]

        it "blames every declaring cell when the error names no known package" $
            blameCells nb "cabal: internal error" `shouldBe` [1, 3]

        it "blames each cell once when the error names several of its packages" $
            blameCells nb "unknown package vector and containers" `shouldBe` [3]

        it "blames more than one cell when the error implicates both" $
            blameCells nb "text and vector both failed" `shouldBe` [1, 3]

        it "matches whole package names, so 'text' does not fire on 'context'" $
            blameCells nb "cabal: failure in context resolution" `shouldBe` [1, 3]

        it "blames nothing in a notebook that declares no dependencies" $
            blameCells (Notebook "t" [mkCell 1 "x = 1"]) "anything" `shouldBe` []
