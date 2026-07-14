{-# LANGUAGE OverloadedStrings #-}

{- | Pins the AI-config wire shapes the frontend picker + config persistence
depend on: the persisted @.sabela/config.json@ keys (now @anthropicKey@ /
@anthropicModel@ / @provider@) and the @knownModels@ picker entry shape.
-}
module Test.ConfigWireSpec (spec) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.List (sort)
import Test.Hspec

import Sabela.Server.Ai (knownModels)
import Sabela.State (configJson)

keysOf :: Value -> [String]
keysOf (Object o) = sort (map Key.toString (KM.keys o))
keysOf _ = []

field :: String -> Value -> Maybe Value
field k (Object o) = KM.lookup (Key.fromString k) o
field _ _ = Nothing

spec :: Spec
spec = describe "AI config wire shapes" $ do
    describe "persisted .sabela/config.json" $ do
        it "has exactly key + model + provider + numCtx + toolLimit" $
            keysOf (configJson "k" "m" "p" 4096 7)
                `shouldBe` ["anthropicKey", "anthropicModel", "numCtx", "provider", "toolLimit"]
        it "carries the key, model, provider, and both knobs" $ do
            field "anthropicKey" (configJson "K" "M" "P" 4096 7) `shouldBe` Just (String "K")
            field "anthropicModel" (configJson "K" "M" "P" 4096 7) `shouldBe` Just (String "M")
            field "provider" (configJson "K" "M" "P" 4096 7) `shouldBe` Just (String "P")
            field "numCtx" (configJson "K" "M" "P" 4096 7) `shouldBe` Just (Number 4096)
            field "toolLimit" (configJson "K" "M" "P" 4096 7) `shouldBe` Just (Number 7)

    describe "knownModels picker entries" $ do
        it "is non-empty" $
            knownModels `shouldNotBe` []
        it "each entry has exactly id, label, description" $
            map keysOf knownModels
                `shouldSatisfy` all (== ["description", "id", "label"])
