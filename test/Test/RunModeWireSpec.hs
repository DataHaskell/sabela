{-# LANGUAGE OverloadedStrings #-}

module Test.RunModeWireSpec (spec) where

import Data.Aeson (Value (..), decode, encode, object, toJSON, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Set as Set
import Data.Text (Text)

import Sabela.AI.Capabilities.Kernel (kernelStatusValue)
import Sabela.Api (RunModeUpdate (..))
import Sabela.Model (NotebookEvent (..), RunMode (..), parseRunMode, runModeTag)
import Sabela.State (newApp)
import Test.Hspec (Spec, describe, it, shouldBe)

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (Key.fromText k) o
field _ _ = Nothing

spec :: Spec
spec = describe "run-mode wire shapes" $ do
    it "tags are the two lowercase words clients switch on" $ do
        runModeTag RunReactive `shouldBe` "reactive"
        runModeTag RunDeferred `shouldBe` "deferred"
        parseRunMode "reactive" `shouldBe` Just RunReactive
        parseRunMode "deferred" `shouldBe` Just RunDeferred
        parseRunMode "lazy" `shouldBe` Nothing

    it "EvRunMode serializes as {type: runMode, mode: <tag>}" $
        toJSON (EvRunMode RunDeferred)
            `shouldBe` object
                [ "type" .= ("runMode" :: Text)
                , "mode" .= ("deferred" :: Text)
                ]

    it "RunModeUpdate round-trips through {mode: <tag>}" $ do
        encode (RunModeUpdate RunDeferred) `shouldBe` "{\"mode\":\"deferred\"}"
        decode "{\"mode\":\"reactive\"}" `shouldBe` Just (RunModeUpdate RunReactive)
        (decode "{\"mode\":\"lazy\"}" :: Maybe RunModeUpdate) `shouldBe` Nothing

    it "kernel_status carries the current runMode" $ do
        app <- newApp "." Set.empty Nothing Nothing []
        status <- kernelStatusValue app
        field "runMode" status `shouldBe` Just (String "reactive")
