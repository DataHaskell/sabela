{-# LANGUAGE OverloadedStrings #-}

module Test.UsageEventSpec (spec) where

import Data.Aeson (Value (..), decode, encode, object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import Test.Hspec

import Sabela.Model (NotebookEvent (..))

field :: String -> Value -> Maybe Value
field k (Object o) = KM.lookup (Key.fromString k) o
field _ _ = Nothing

spec :: Spec
spec = do
    describe "EvChatUsageUpdate wire format" $ do
        it "serialises with type=chatUsageUpdate, a turnId, and a usage object" $ do
            let payload =
                    object
                        [ "inputTokens" .= (1200 :: Int)
                        , "outputTokens" .= (400 :: Int)
                        , "cacheReadInputTokens" .= (800 :: Int)
                        , "cacheCreationInputTokens" .= (0 :: Int)
                        , "iterations" .= (3 :: Int)
                        , "toolCalls" .= (5 :: Int)
                        , "wallTimeMs" .= (7123 :: Int)
                        ]
                ev = EvChatUsageUpdate 42 payload
                v = case decode (encode ev) of
                    Just x -> x
                    Nothing -> error "encode failed"
            field "type" v `shouldBe` Just (String "chatUsageUpdate")
            field "turnId" v `shouldBe` Just (Number 42)
            case field "usage" v of
                Just (Object u) -> do
                    KM.lookup (Key.fromString "inputTokens") u
                        `shouldBe` Just (Number 1200)
                    KM.lookup (Key.fromString "iterations") u
                        `shouldBe` Just (Number 3)
                    KM.lookup (Key.fromString "wallTimeMs") u
                        `shouldBe` Just (Number 7123)
                other ->
                    expectationFailure ("usage should be an object, got " ++ show other)

        it "survives round-trip encode/decode" $ do
            let payload = object ["inputTokens" .= (10 :: Int)]
                ev = EvChatUsageUpdate 1 payload
                v = case decode (encode ev) of
                    Just x -> x :: Value
                    Nothing -> error "decode failed"
                v2 = case decode (encode v) of
                    Just x -> x :: Value
                    Nothing -> error "decode roundtrip failed"
            v `shouldBe` v2

    describe "EvChatUsageUpdate is distinct from other chat events" $ do
        it "does not collide with chatDone" $ do
            let done = case decode (encode (EvChatDone 1)) of
                    Just x -> x :: Value
                    Nothing -> error "encode chatDone failed"
                usage = case decode (encode (EvChatUsageUpdate 1 (object []))) of
                    Just x -> x :: Value
                    Nothing -> error "encode usage failed"
            field "type" done `shouldBe` Just (String "chatDone")
            field "type" usage `shouldBe` Just (String "chatUsageUpdate")
