{-# LANGUAGE OverloadedStrings #-}

module Test.CacheControlSpec (spec) where

import Data.Aeson (ToJSON, Value (..), decode, encode, object)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text.Lazy.Encoding as TLE
import Test.Hspec

import Sabela.Anthropic.Types (
    CacheControl (..),
    SystemBlock (..),
    ToolDef (..),
 )

toJson :: (ToJSON a) => a -> Value
toJson x = case decode (encode x) of
    Just v -> v
    Nothing -> error ("failed to decode: " ++ show (TLE.decodeUtf8 (encode x)))

field :: String -> Value -> Maybe Value
field k (Object o) = KM.lookup (Key.fromString k) o
field _ _ = Nothing

spec :: Spec
spec = do
    describe "CacheControl JSON" $ do
        it "Ephemeral serialises as {type: ephemeral} with no ttl" $ do
            let v = toJson Ephemeral
            field "type" v `shouldBe` Just (String "ephemeral")
            field "ttl" v `shouldBe` Nothing

        it "EphemeralHour serialises with ttl: 1h" $ do
            let v = toJson EphemeralHour
            field "type" v `shouldBe` Just (String "ephemeral")
            field "ttl" v `shouldBe` Just (String "1h")

    describe "SystemBlock with cache_control" $ do
        it "emits cache_control object when present" $ do
            let sb = SystemBlock "hello" (Just EphemeralHour)
                v = toJson sb
                cc = field "cache_control" v
            field "text" v `shouldBe` Just (String "hello")
            case cc of
                Just (Object o) -> do
                    KM.lookup (Key.fromString "type") o
                        `shouldBe` Just (String "ephemeral")
                    KM.lookup (Key.fromString "ttl") o
                        `shouldBe` Just (String "1h")
                other -> expectationFailure ("unexpected cache_control: " ++ show other)

        it "omits cache_control when Nothing" $ do
            let sb = SystemBlock "hello" Nothing
            field "cache_control" (toJson sb) `shouldBe` Nothing

    describe "ToolDef cache_control" $ do
        it "omits cache_control when tdCacheControl = Nothing" $ do
            let td = ToolDef "x" "desc" (object []) Nothing
            field "cache_control" (toJson td) `shouldBe` Nothing

        it "emits 1h cache_control when tdCacheControl = Just EphemeralHour" $ do
            let td = ToolDef "x" "desc" (object []) (Just EphemeralHour)
            case field "cache_control" (toJson td) of
                Just (Object o) ->
                    KM.lookup (Key.fromString "ttl") o
                        `shouldBe` Just (String "1h")
                other -> expectationFailure ("unexpected cache_control: " ++ show other)
