{-# LANGUAGE OverloadedStrings #-}

-- | @reactivity.md@, "Environment, UI, and Side Effects".
module Test.ReactivityDoc.Effects (spec) where

import Sabela.Model (Cell (..), Notebook (..))
import Sabela.Reactivity (markRootedDirty)
import Test.CellFixture (dirty, mkCell)
import Test.Hspec
import Test.ReactivityDoc.Fixture

spec :: Spec
spec = describe "reactivity.md: environment, UI and side effects" $ do
    describe "impure dependencies" $ do
        let cells =
                [ mkCell 1 "filepath = \"notes.txt\""
                , mkCell 2 "content <- readFile filepath"
                , mkCell 3 "wordCount = length (words content)"
                , mkCell 4 "summary = show wordCount ++ \" words\""
                ]
        it "changing filepath re-triggers the IO action and the pure chain" $
            planFor cells 1 `shouldBe` [1, 2, 3, 4]
        it "editing the IO action re-runs its pure consumers" $
            planFor cells 2 `shouldBe` [2, 3, 4]
        it "a pure consumer edit does not re-run the IO action" $
            planFor cells 3 `shouldBe` [3, 4]

    describe "explicit binding: an isolated IO cell does not re-poll" $ do
        let cells =
                [ mkCell 1 "currentTime <- getCurrentTime"
                , mkCell 2 "x = 1"
                , mkCell 3 "y = x + 1"
                , mkCell 4 "stamp = show currentTime"
                ]
        it "an unrelated pure edit leaves the clock cell alone" $
            planFor cells 2 `shouldBe` [2, 3]
        it "a run-all with other stale cells does not re-poll the clock" $
            stalePlanFor
                [mkCell 1 "currentTime <- getCurrentTime", dirty (mkCell 2 "x = 1")]
                `shouldBe` [2]
        it "a consumer of the clock does not re-run the clock either" $
            planFor cells 4 `shouldBe` [4]
        it "a bare action with no binding is likewise a leaf" $
            planFor (mkCell 5 "print 42" : cells) 2 `shouldBe` [2, 3]

    describe "interactive UI elements: a slider streams into the graph" $ do
        let cells =
                [ mkCell 1 "sliderValue <- slider 1 100"
                , mkCell 2 "radius = fromIntegral sliderValue :: Double"
                , mkCell 3 "area = pi * radius ^ (2 :: Int)"
                , mkCell 4 "other = 3"
                ]
        it "a widget move rooted at its cell recalculates radius and area in order" $
            planFor cells 1 `shouldBe` [1, 2, 3]
        it "a widget move does not touch unrelated cells" $
            planFor cells 1 `shouldNotContain` [4]
        it "in deferred mode the move marks the widget cell and its consumers stale" $ do
            let nb = markRootedDirty 1 (nbOf cells)
                dirtyIds = [cellId c | c <- nbCells nb, cellDirty c]
            dirtyIds `shouldBe` [1, 2, 3]
