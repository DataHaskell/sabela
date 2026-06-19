{-# LANGUAGE OverloadedStrings #-}

module Test.ProxySpec (spec) where

import Control.Exception (SomeException, try)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (addUTCTime, getCurrentTime)
import Hub.Auth (dropExpiredStates, secureAttr)
import Hub.Gallery (GalleryStore, addFeatured, newGalleryStore)
import Hub.Proxy (hubApp)
import Hub.Proxy.Forward (
    filterRequestHeaders,
    hardenResponseHeaders,
 )
import Hub.Session (insertSession, newSessionManager)
import Hub.Share (
    Share (..),
    ShareStore,
    newShareStore,
    publishShare,
    writeShareSource,
 )
import Hub.Shares.Api (publishMode)
import Hub.Types (
    DockerConfig (..),
    ExportMode (..),
    HubConfig (..),
    Session (..),
    SessionId (..),
    SessionKey (..),
    SessionKind (..),
    SessionState (..),
    TaskId (..),
    UserId (..),
 )
import Hub.Users (newUserStore)
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Client.TLS as TLS
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Test hiding (Session)
import System.Directory (
    getTemporaryDirectory,
    listDirectory,
    removeDirectoryRecursive,
 )
import System.FilePath ((</>))
import Test.Hspec
import Test.MockEcs
import Test.ProxyHelpers

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

        it "landing page shows the hero, value prop, visual, and CTA" $ do
            app <- makeApp
            resp <- runSession (request defaultRequest) app
            let body = LC8.unpack (simpleBody resp)
            body `shouldSatisfy` isInfixOf "Reactive notebooks"
            body `shouldSatisfy` isInfixOf "DataHaskell"
            body `shouldSatisfy` isInfixOf "re-run"
            body `shouldSatisfy` isInfixOf "/_hub/login"
            body `shouldSatisfy` isInfixOf "<svg"

    describe "hardenResponseHeaders" $ do
        it "drops the cookie Domain attribute" $ do
            let out =
                    hardenResponseHeaders
                        [("Set-Cookie", "_x=1; Domain=example.com; Path=/; HttpOnly")]
            lookup "Set-Cookie" out `shouldBe` Just "_x=1; Path=/; HttpOnly"

        it "adds frame-ancestors and nosniff when absent" $ do
            let out = hardenResponseHeaders [("Content-Type", "text/html")]
            lookup "Content-Security-Policy" out
                `shouldBe` Just "frame-ancestors 'self'"
            lookup "X-Content-Type-Options" out `shouldBe` Just "nosniff"

        it "preserves an existing app CSP" $ do
            let out =
                    hardenResponseHeaders
                        [("Content-Security-Policy", "default-src 'none'")]
            lookup "Content-Security-Policy" out
                `shouldBe` Just "default-src 'none'"

    describe "publishMode" $ do
        it "accepts dashboard, slideshow, and notebook" $ do
            publishMode [("mode", Just "dashboard")] `shouldBe` ExpDashboard
            publishMode [("mode", Just "slideshow")] `shouldBe` ExpSlideshow
            publishMode [("mode", Just "notebook")] `shouldBe` ExpNotebook

        it "falls back to dashboard for unknown, missing, or valueless mode" $ do
            publishMode [("mode", Just "evil")] `shouldBe` ExpDashboard
            publishMode [] `shouldBe` ExpDashboard
            publishMode [("mode", Nothing)] `shouldBe` ExpDashboard

    describe "filterRequestHeaders" $
        it "strips Cookie + Authorization (and hop-by-hop) before forwarding" $ do
            let out =
                    filterRequestHeaders
                        [ ("Cookie", "_sabela_session=secret")
                        , ("Authorization", "Bearer x")
                        , ("Connection", "keep-alive")
                        , ("Accept", "text/html")
                        ]
            lookup "Cookie" out `shouldBe` Nothing
            lookup "Authorization" out `shouldBe` Nothing
            lookup "Connection" out `shouldBe` Nothing
            lookup "Accept" out `shouldBe` Just "text/html"

    describe "secureAttr" $ do
        it "adds Secure behind an HTTPS terminator (X-Forwarded-Proto)" $
            secureAttr defaultRequest{requestHeaders = [("X-Forwarded-Proto", "https")]}
                `shouldBe` "; Secure"
        it "omits Secure over plain HTTP (local dev)" $ do
            secureAttr defaultRequest `shouldBe` ""
            secureAttr defaultRequest{requestHeaders = [("X-Forwarded-Proto", "http")]}
                `shouldBe` ""

    describe "dropExpiredStates" $
        it "drops CSRF states past the TTL, keeps fresh ones" $ do
            now <- getCurrentTime
            let m =
                    Map.fromList
                        [ ("stale", addUTCTime (-1200) now)
                        , ("fresh", addUTCTime (-60) now)
                        ]
            Map.keys (dropExpiredStates now m) `shouldBe` ["fresh"]
