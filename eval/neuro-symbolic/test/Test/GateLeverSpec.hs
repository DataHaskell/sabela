{-# LANGUAGE OverloadedStrings #-}

module Test.GateLeverSpec (spec) where

import Data.Aeson (encode, object, (.=))
import Data.IORef (newIORef, readIORef)
import qualified Data.Text as T
import Network.HTTP.Client (
    Request (requestBody),
    RequestBody (RequestBodyLBS),
    managerModifyRequest,
    parseRequest,
 )
import Test.Hspec

import Eval.Bench (ArmResult (..), Comparison (..), renderComparison)
import Eval.Gate (
    GateLever (..),
    armOrder,
    meteredTlsManagerSettings,
    searchEnv,
 )
import Eval.GateResult (SearchMode (..))
import Eval.Ollama (OllamaReqOpts (..), chatRequestBody)

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

    describe "gate model-request payload meter" $ do
        it "counts encoded bytes, including the exact offered catalogue" $ do
            withTotal <- newIORef 0
            withoutTotal <- newIORef 0
            request <- parseRequest "http://localhost:11434/api/chat"
            let messages = [object ["role" .= ("user" :: T.Text), "content" .= ("λ" :: T.Text)]]
                catalogue =
                    [ object
                        [ "type" .= ("function" :: T.Text)
                        , "function"
                            .= object
                                [ "name" .= ("discover" :: T.Text)
                                , "description" .= ("look up one fact" :: T.Text)
                                ]
                        ]
                    ]
                opts = OllamaReqOpts False (Just 7) "30m" 32768 0.4
                withBody = encode (chatRequestBody opts "gpt-oss:20b" messages catalogue)
                withoutBody = encode (chatRequestBody opts "gpt-oss:20b" messages [])
                withSettings = meteredTlsManagerSettings withTotal
                withoutSettings = meteredTlsManagerSettings withoutTotal
            _ <-
                managerModifyRequest
                    withSettings
                    request{requestBody = RequestBodyLBS withBody}
            _ <-
                managerModifyRequest
                    withoutSettings
                    request{requestBody = RequestBodyLBS withoutBody}
            withBytes <- readIORef withTotal
            withoutBytes <- readIORef withoutTotal
            withBytes `shouldSatisfy` (> withoutBytes)

        it "accumulates every request rather than measuring the final transcript" $ do
            total <- newIORef 0
            request <- parseRequest "http://localhost:11434/api/chat"
            let settings = meteredTlsManagerSettings total
                -- Aeson writes lambda as two UTF-8 bytes: 16 bytes, 15 characters.
                firstBody = encode (object ["content" .= ("λ" :: T.Text)])
                first = request{requestBody = RequestBodyLBS firstBody}
                second = request{requestBody = RequestBodyLBS "1234567"}
            _ <- managerModifyRequest settings first
            _ <- managerModifyRequest settings second
            measured <- readIORef total
            measured `shouldBe` 23
