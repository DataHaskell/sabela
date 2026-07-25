{-# LANGUAGE OverloadedStrings #-}

{- | There is no install tool (C4-3): a garbled or nonexistent call shaped
like "install X" should steer to the real @-- cabal: build-depends:@ idiom
through the SAME message-construction path the unknown-tool/garbled-name
recovery already uses, not a parallel special case.
-}
module Test.InstallAffordanceSpec (installAffordanceSpec) where

import qualified Data.Text as T
import Siza.Agent.ToolRoute (installSteer)
import Siza.Agent.Tools (unknownToolMsg)
import Test.Hspec

installAffordanceSpec :: Spec
installAffordanceSpec = describe "install affordance (C4-3)" $ do
    describe "installSteer" $ do
        it "steers a name literally shaped like install?" $
            installSteer "install?"
                `shouldSatisfy` T.isInfixOf "-- cabal: build-depends:"

        it "matches case-insensitively and mid-word" $
            installSteer "Install_Granite"
                `shouldSatisfy` T.isInfixOf "-- cabal: build-depends:"

        it "is empty for a name with no install intent" $
            installSteer "frobnicate" `shouldBe` ""

    describe "unknownToolMsg" $ do
        it "steers 'install?' toward the -- cabal: idiom" $
            unknownToolMsg "install?"
                `shouldSatisfy` T.isInfixOf "-- cabal: build-depends:"

        it "still lists the valid tools alongside the steering" $
            unknownToolMsg "install?" `shouldSatisfy` T.isInfixOf "Valid tools:"

        it "does not add install steering for an unrelated unknown name" $
            unknownToolMsg "frobnicate"
                `shouldNotSatisfy` T.isInfixOf "-- cabal: build-depends:"
