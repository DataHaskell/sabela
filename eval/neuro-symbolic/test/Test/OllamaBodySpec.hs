{-# LANGUAGE OverloadedStrings #-}

{- | Golden pin for the Ollama @/api/chat@ outbound request body. Guards the
wire shape ('num_ctx', 'keep_alive', 'stream:false', options, seed) so the
Phase 2 move of the client into @Sabela.LLM.Ollama@ cannot change what the
model sees byte-for-byte.
-}
module Test.OllamaBodySpec (spec) where

import Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Test.Hspec

import Eval.Ollama (OllamaReqOpts (..), chatRequestBody)

field :: String -> Value -> Maybe Value
field k (Object o) = KM.lookup (Key.fromString k) o
field _ _ = Nothing

defaultOpts :: OllamaReqOpts
defaultOpts =
    OllamaReqOpts
        { oroThink = False
        , oroSeed = Nothing
        , oroKeepAlive = "30m"
        , oroNumCtx = 32768
        , oroTemperature = 0.4
        }

spec :: Spec
spec = describe "chatRequestBody (Ollama /api/chat outbound)" $ do
    let msgs =
            [ object
                [ "role" .= ("user" :: String)
                , "content" .= ("hi" :: String)
                ]
            ]
        tools = [object ["type" .= ("function" :: String)]]
        body = chatRequestBody defaultOpts "gpt-oss:20b" msgs tools

    it "sets model, non-streaming, and the think flag" $ do
        field "model" body `shouldBe` Just (String "gpt-oss:20b")
        field "stream" body `shouldBe` Just (Bool False)
        field "think" body `shouldBe` Just (Bool False)

    it "echoes messages and tools through unchanged" $ do
        field "messages" body `shouldBe` Just (toJSON msgs)
        field "tools" body `shouldBe` Just (toJSON tools)

    it "carries temperature 0.4 and the resolved num_ctx in options" $ do
        let opts = field "options" body
        (opts >>= field "temperature") `shouldBe` Just (toJSON (0.4 :: Double))
        (opts >>= field "num_ctx") `shouldBe` Just (toJSON (32768 :: Int))

    it "carries a raised temperature when the opts set one (for diverse sampling)" $ do
        let b = chatRequestBody defaultOpts{oroTemperature = 0.9} "m" [] []
        (field "options" b >>= field "temperature")
            `shouldBe` Just (toJSON (0.9 :: Double))

    it "reflects the resolved keep_alive" $
        field "keep_alive" body `shouldBe` Just (String "30m")

    it "omits seed when unseeded" $
        (field "options" body >>= field "seed") `shouldBe` Nothing

    it "includes seed when seeded" $ do
        let b = chatRequestBody defaultOpts{oroSeed = Just 7} "m" [] []
        (field "options" b >>= field "seed") `shouldBe` Just (toJSON (7 :: Int))

    it "tracks num_ctx and think from the opts" $ do
        let b = chatRequestBody defaultOpts{oroNumCtx = 4096, oroThink = True} "m" [] []
        (field "options" b >>= field "num_ctx") `shouldBe` Just (toJSON (4096 :: Int))
        field "think" b `shouldBe` Just (Bool True)
