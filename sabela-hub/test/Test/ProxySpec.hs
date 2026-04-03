{-# LANGUAGE OverloadedStrings #-}

module Test.ProxySpec (spec) where

import Hub.Proxy (extractSessionId, hubApp)
import Hub.Session (newSessionManager)
import Hub.Types (SessionId (..))
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Client.TLS as TLS
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Test
import Test.Hspec
import Test.MockEcs

spec :: Spec
spec = describe "Proxy" $ do
    describe "hubApp" $ do
        it "shows login page when no session cookie" $ do
            app <- makeApp
            resp <- runSession (request defaultRequest) app
            simpleStatus resp `shouldBe` status200

        it "shows login page for unknown session" $ do
            app <- makeApp
            let req = defaultRequest{requestHeaders = [("Cookie", "_sabela_session=unknown")]}
            resp <- runSession (request req) app
            simpleStatus resp `shouldBe` status200

        it "redirects /_hub/login to Google" $ do
            app <- makeApp
            let req = defaultRequest{rawPathInfo = "/_hub/login"}
            resp <- runSession (request req) app
            simpleStatus resp `shouldBe` status302

    describe "extractSessionId" $ do
        it "extracts session ID from cookie" $ do
            let req = defaultRequest{requestHeaders = [("Cookie", "_sabela_session=tok123")]}
            extractSessionId req `shouldBe` Just (SessionId "tok123")

        it "returns Nothing without cookie" $ do
            extractSessionId defaultRequest `shouldBe` Nothing

makeApp :: IO Application
makeApp = do
    ms <- newMockState
    sm <- newSessionManager (mockEcsBackend ms) testConfig
    mgr <- HC.newManager TLS.tlsManagerSettings
    hubApp sm mgr
