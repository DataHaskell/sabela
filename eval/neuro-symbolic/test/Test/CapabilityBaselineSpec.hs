{-# LANGUAGE OverloadedStrings #-}

{- | Intention specs for the capability BASELINE in non-capability gates
(evalExpr deep-dive finding 5).

In the fixed-lever ResolverLever gate, @SABELA_CAPABILITY_SEARCH@ was simply
inherited-unset, so @discover@'s prose route silently degraded to keyword-only
in BOTH arms — the capability backend was off even though it was not the lever
under test. A knob that is not the lever should sit at its production baseline
(ON); only 'CapabilityLever' itself may vary it between arms.

Proposed API (extends Eval.Gate; 'setCapabilityEnv' applies it):

  capabilityEnvFor :: GateLever -> SearchMode -> Maybe String
    -- Just "1" = set, Nothing = unset in the gate process
-}
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
