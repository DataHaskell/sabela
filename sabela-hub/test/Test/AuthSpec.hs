{-# LANGUAGE OverloadedStrings #-}

module Test.AuthSpec (spec) where

import Hub.Proxy (extractSessionId)
import Hub.Types
import Network.Wai (defaultRequest)
import Network.Wai.Internal (Request (..))
import Test.Hspec

spec :: Spec
spec = describe "Auth" $ do
    describe "extractSessionId" $ do
        it "extracts session ID from cookie" $ do
            let req = defaultRequest{requestHeaders = [("Cookie", "_sabela_session=abc123")]}
            extractSessionId req `shouldBe` Just (SessionId "abc123")

        it "returns Nothing when no cookie" $ do
            extractSessionId defaultRequest `shouldBe` Nothing

        it "returns Nothing for empty cookie value" $ do
            let req = defaultRequest{requestHeaders = [("Cookie", "_sabela_session=")]}
            extractSessionId req `shouldBe` Nothing

        it "handles multiple cookies" $ do
            let req =
                    defaultRequest
                        { requestHeaders = [("Cookie", "other=x; _sabela_session=mytoken; foo=bar")]
                        }
            extractSessionId req `shouldBe` Just (SessionId "mytoken")
