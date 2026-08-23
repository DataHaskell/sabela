{-# LANGUAGE OverloadedStrings #-}

{- | What a candidate is compiled against. A candidate replacing a cell needs
the cells above it and nothing else: the cells below consume what it defines,
so replaying them without it refuses edits that would compile. Every edit to a
cell whose imports or bindings a later cell uses hit this.
-}
module Test.GatePrefixSpec (spec) where

import Data.Text (Text)
import Test.Hspec

import Sabela.Model (Cell (..), CellType (..), Notebook (..))
import Sabela.Session.Materialize (CandidateSpec (..))
import Sabela.Session.Materialize.Candidate (prefixFor)
import Sabela.SessionTypes (CellLang (..))

cell :: Int -> Text -> Cell
cell cid src =
    Cell
        { cellId = cid
        , cellType = CodeCell
        , cellLang = Haskell
        , cellSource = src
        , cellOutputs = []
        , cellError = Nothing
        , cellDirty = False
        }

-- | Imports, then the cell an edit targets, then two cells that consume it.
notebook :: Notebook
notebook =
    Notebook
        "n"
        [ cell 1 "import Torch"
        , cell 2 "sampleSize = 2000"
        , cell 3 "pixels = load sampleSize"
        , cell 4 "graph = neighbours pixels"
        ]

candidateReplacing :: Maybe Int -> CandidateSpec
candidateReplacing mCid =
    CandidateSpec
        { candidateMetadataSource = "sampleSize = 4000"
        , candidateSetup = "sampleSize = 4000"
        , candidateExpression = Nothing
        , candidateReplacesCellId = mCid
        , candidateDeliberate = True
        }

ids :: Notebook -> [Int]
ids = map cellId . nbCells

spec :: Spec
spec = describe "the context a candidate is compiled against" $ do
    it "keeps the cells above the one being replaced" $
        ids (prefixFor (candidateReplacing (Just 2)) notebook) `shouldBe` [1]

    it "drops the cells below it, which consume what it defines" $
        ids (prefixFor (candidateReplacing (Just 2)) notebook)
            `shouldSatisfy` all (< 2)

    it "keeps the whole notebook when the candidate replaces nothing" $
        ids (prefixFor (candidateReplacing Nothing) notebook) `shouldBe` [1, 2, 3, 4]

    it "is empty when the first cell is the one being replaced" $
        ids (prefixFor (candidateReplacing (Just 1)) notebook) `shouldBe` []

    it "keeps the notebook when the id names no cell" $
        ids (prefixFor (candidateReplacing (Just 99)) notebook) `shouldBe` [1, 2, 3, 4]
