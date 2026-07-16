{-# LANGUAGE OverloadedStrings #-}

{- | The A/B lever env mapping: pins that a default-ON server flag lever sets
its var explicitly in BOTH arms, so the OFF arm actually disables the feature
rather than inheriting the default.
-}
module Test.GateLeverSpec (spec) where

import Test.Hspec

import Eval.Gate (GateLever (..), searchEnv)
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
        it "leaves the server env empty on the OFF arm" $
            searchEnv ResolverLever SearchOff `shouldBe` []

    describe "CapabilityLever" $
        it "never touches the server env (toggles the gate process instead)" $ do
            searchEnv CapabilityLever SearchOn `shouldBe` []
            searchEnv CapabilityLever SearchOff `shouldBe` []
