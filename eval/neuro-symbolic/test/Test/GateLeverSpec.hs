{-# LANGUAGE OverloadedStrings #-}

{- | The A/B lever env mapping: pins that a default-ON server flag lever sets
its var explicitly in BOTH arms, so the OFF arm actually disables the feature
rather than inheriting the default.
-}
module Test.GateLeverSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Eval.Bench (ArmResult (..), Comparison (..), renderComparison)
import Eval.Gate (GateLever (..), armOrder, searchEnv)
import Eval.GateResult (SearchMode (..))

spec :: Spec
spec = describe "Eval.Gate.searchEnv" $ do
    describe "ServerFlagLever (default-ON self-healing flags)" $ do
        it "sets the flag to 1 on the ON arm" $
            searchEnv (ServerFlagLever "SABELA_HOLE_FIT") SearchOn
                `shouldBe` [("SABELA_HOLE_FIT", "1")]
        it "sets the flag to 0 on the OFF arm (explicit, not unset)" $
            searchEnv (ServerFlagLever "SABELA_HOLE_FIT") SearchOff
                `shouldBe` [("SABELA_HOLE_FIT", "0")]

    describe "ResolverLever" $ do
        it "sets the resolver var only on the ON arm" $
            searchEnv ResolverLever SearchOn
                `shouldBe` [("SABELA_HOOGLE_RESOLVE", "1")]
        it "pins the resolver var to 0 on the OFF arm (unset means default-ON)" $
            searchEnv ResolverLever SearchOff
                `shouldBe` [("SABELA_HOOGLE_RESOLVE", "0")]

    describe "CapabilityLever" $
        it "never touches the server env (toggles the gate process instead)" $ do
            searchEnv CapabilityLever SearchOn `shouldBe` []
            searchEnv CapabilityLever SearchOff `shouldBe` []

    describe "armOrder (cold-install bias)" $ do
        -- Off always ran first, so dep-heavy tasks paid the cold cabal install
        -- in the OFF arm and reused the warm store in ON — a measured bias
        -- (thumbInfo/shortest flipped on exactly this pattern).
        it "alternates which arm runs first per (task, seed) pair" $ do
            armOrder 0 `shouldBe` [SearchOff, SearchOn]
            armOrder 1 `shouldBe` [SearchOn, SearchOff]
            armOrder 2 `shouldBe` [SearchOff, SearchOn]

    describe "renderComparison (z-noise discipline)" $ do
        let cmp z = Comparison (ArmResult 5 9) (ArmResult 7 9) 0.22 z
        it "labels an insignificant delta as noise" $
            renderComparison (cmp 1.0) `shouldSatisfy` T.isInfixOf "NOISE"
        it "does not label a significant delta" $
            renderComparison (cmp 3.0)
                `shouldSatisfy` (not . T.isInfixOf "NOISE")
