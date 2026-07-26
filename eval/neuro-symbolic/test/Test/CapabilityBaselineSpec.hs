{-# LANGUAGE OverloadedStrings #-}

module Test.CapabilityBaselineSpec (spec) where

import Test.Hspec

import Eval.Gate (GateLever (..), capabilityEnvFor)
import Eval.GateResult (SearchMode (..))

spec :: Spec
spec = describe "capability baseline in the gate process (intention)" $ do
    describe "CapabilityLever — the one gate that varies it" $ do
        it "sets it on the ON arm" $
            capabilityEnvFor CapabilityLever SearchOn `shouldBe` Just "1"
        it "unsets it on the OFF arm" $
            capabilityEnvFor CapabilityLever SearchOff `shouldBe` Nothing

    describe "every other lever — baseline ON in both arms" $ do
        it "ResolverLever keeps the capability backend on in both arms" $ do
            capabilityEnvFor ResolverLever SearchOn `shouldBe` Just "1"
            capabilityEnvFor ResolverLever SearchOff `shouldBe` Just "1"
        it "ServerFlagLever keeps the capability backend on in both arms" $ do
            capabilityEnvFor (ServerFlagLever "SABELA_HOLE_FIT") SearchOn
                `shouldBe` Just "1"
            capabilityEnvFor (ServerFlagLever "SABELA_HOLE_FIT") SearchOff
                `shouldBe` Just "1"
