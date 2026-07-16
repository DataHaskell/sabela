{-# LANGUAGE OverloadedStrings #-}

{- | The reactive-closure selection behind the closure-verified accept: which
non-owned cells a repair to an owned cell forces a health re-check on.
-}
module Test.VerifyDownstreamSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Sabela.AI.Orchestrator.Loop (downstreamDependents)
import Sabela.Model (Cell)
import Test.TopoSpec.Helpers (mkCell)

-- | @c1@ defines @x@; @c2@ uses @x@; @c3@ uses @c2@'s @y@ — a 1 -> 2 -> 3 chain.
cells :: [Cell]
cells =
    [ mkCell 1 "x = 1"
    , mkCell 2 "y = x + 1"
    , mkCell 3 "z = y + 1"
    ]

owned :: [Int] -> Map.Map Int ()
owned ids = Map.fromList [(i, ()) | i <- ids]

spec :: Spec
spec = describe "Sabela.AI.Orchestrator.Loop.downstreamDependents" $ do
    it "returns the non-owned cells downstream of an owned cell, in topo order" $
        downstreamDependents (owned [1]) cells `shouldBe` [2, 3]

    it "excludes owned cells from the re-check set" $
        downstreamDependents (owned [1, 2]) cells `shouldBe` [3]

    it "is empty when the owned cell has no dependents" $
        downstreamDependents (owned [3]) cells `shouldBe` []
