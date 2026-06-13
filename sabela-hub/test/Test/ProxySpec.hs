{-# LANGUAGE OverloadedStrings #-}

module Test.ProxySpec (spec) where

import Control.Exception (SomeException, try)
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
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

    describe "gallery + admin routes" $ do
        it "serves the gallery at /gallery" $ do
            app <- makeApp
            resp <- runSession (request (setPath defaultRequest "/gallery")) app
            simpleStatus resp `shouldBe` status200
            LC8.unpack (simpleBody resp) `shouldSatisfy` isInfixOf "/_hub/login"

        it "serves the gallery as the anonymous homepage (/)" $ do
            app <- makeApp
            resp <- runSession (request defaultRequest{rawPathInfo = "/"}) app
            simpleStatus resp `shouldBe` status200
            LC8.unpack (simpleBody resp)
                `shouldSatisfy` isInfixOf "github.com/DataHaskell/sabela"

        it "405s an admin route method mismatch (not a proxied 200)" $ do
            app <- makeApp
            resp <- runSession (request (setPath defaultRequest "/_hub/admin/feature")) app
            simpleStatus resp `shouldBe` status405

        it "downloads a featured notebook's source (.md)" $ do
            (app, store, gallery) <- makeAppFull
            publishShare store (mkShare "ab12") "<h1>x</h1>"
            writeShareSource store "ab12" "# Iris\nplot iris"
            addFeatured gallery "ab12"
            resp <- runSession (request (setPath defaultRequest "/_hub/source/ab12")) app
            simpleStatus resp `shouldBe` status200
            LC8.unpack (simpleBody resp) `shouldSatisfy` isInfixOf "plot iris"
            lookup hContentType (simpleHeaders resp)
                `shouldBe` Just "text/markdown; charset=utf-8"

        it "refuses to download a non-public slug (404)" $ do
            (app, store, _) <- makeAppFull
            publishShare store (mkShare "ab12") "<h1>x</h1>"
            writeShareSource store "ab12" "# secret-ish source"
            resp <- runSession (request (setPath defaultRequest "/_hub/source/ab12")) app
            simpleStatus resp `shouldBe` status404

        it "rejects fork without a session (401)" $ do
            app <- makeApp
            let req = (setPath defaultRequest "/_hub/fork/ab12"){requestMethod = methodPost}
            resp <- runSession (request req) app
            simpleStatus resp `shouldBe` status401

        it "forks a public notebook into the caller's work dir" $ do
            base <- getTemporaryDirectory
            let root = base </> "sabela-fork-test"
            _ <- try (removeDirectoryRecursive root) :: IO (Either SomeException ())
            (app, store, gallery) <- makeAppForkable root
            publishShare store (mkShare "ab12") "<h1>x</h1>"
            writeShareSource store "ab12" "# Iris\nplot iris"
            addFeatured gallery "ab12"
            let req =
                    (setPath defaultRequest "/_hub/fork/ab12")
                        { requestMethod = methodPost
                        , requestHeaders =
                            [ ("Cookie", "_sabela_session=usersid")
                            , ("Origin", "http://localhost:8080")
                            ]
                        }
            resp <- runSession (request req) app
            simpleStatus resp `shouldBe` status200
            LC8.unpack (simpleBody resp) `shouldSatisfy` isInfixOf "forked-"
            -- the source landed in the forker's (user@x → user_x) work dir
            files <- listDirectory (root </> "users" </> "user_x")
            length (filter (isInfixOf "forked-") files) `shouldBe` 1

        it "rejects a cross-origin fork (403)" $ do
            base <- getTemporaryDirectory
            (app, store, gallery) <- makeAppForkable (base </> "sabela-fork-test2")
            publishShare store (mkShare "ab12") "<h1>x</h1>"
            writeShareSource store "ab12" "# Iris"
            addFeatured gallery "ab12"
            let req =
                    (setPath defaultRequest "/_hub/fork/ab12")
                        { requestMethod = methodPost
                        , requestHeaders =
                            [ ("Cookie", "_sabela_session=usersid")
                            , ("Origin", "http://evil.example")
                            ]
                        }
            resp <- runSession (request req) app
            simpleStatus resp `shouldBe` status403

        it "authed non-admin gets login at /_hub/admin (not enumerable)" $ do
            app <- makeAppSess
            let req =
                    (setPath defaultRequest "/_hub/admin")
                        { requestHeaders = [("Cookie", "_sabela_session=usersid")]
                        }
            resp <- runSession (request req) app
            LC8.unpack (simpleBody resp) `shouldSatisfy` isInfixOf "Reactive notebooks"

        it "admin gets the curation page at /_hub/admin" $ do
            app <- makeAppSess
            let req =
                    (setPath defaultRequest "/_hub/admin")
                        { requestHeaders = [("Cookie", "_sabela_session=adminsid")]
                        }
            resp <- runSession (request req) app
            LC8.unpack (simpleBody resp) `shouldSatisfy` isInfixOf "Gallery curation"

        it "serves a published share at /s/<slug> with hardened headers" $ do
            (app, store) <- makeAppWithStore
            publishShare store (mkShare "abc123") "<h1>shared dash</h1>"
            let req =
                    defaultRequest
                        { rawPathInfo = "/s/abc123"
                        , pathInfo = ["s", "abc123"]
                        }
            resp <- runSession (request req) app
            simpleStatus resp `shouldBe` status200
            LC8.unpack (simpleBody resp) `shouldSatisfy` isInfixOf "shared dash"
            lookup "X-Content-Type-Options" (simpleHeaders resp)
                `shouldBe` Just "nosniff"

        it "returns 404 for an unknown share" $ do
            app <- makeApp
            let req =
                    defaultRequest
                        { rawPathInfo = "/s/deadbeef"
                        , pathInfo = ["s", "deadbeef"]
                        }
            resp <- runSession (request req) app
            simpleStatus resp `shouldBe` status404

        it "rejects publish without a session (401)" $ do
            app <- makeApp
            let req =
                    defaultRequest
                        { rawPathInfo = "/_hub/publish"
                        , pathInfo = ["_hub", "publish"]
                        , requestMethod = methodPost
                        }
            resp <- runSession (request req) app
            simpleStatus resp `shouldBe` status401

        it "rejects listing shares without a session (401)" $ do
            app <- makeApp
            let req =
                    defaultRequest
                        { rawPathInfo = "/_hub/shares"
                        , pathInfo = ["_hub", "shares"]
                        }
            resp <- runSession (request req) app
            simpleStatus resp `shouldBe` status401

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

makeApp :: IO Application
makeApp = fst <$> makeAppWithStore

makeAppWithStore :: IO (Application, ShareStore)
makeAppWithStore = do
    ms <- newMockState
    sm <- newSessionManager (mockEcsBackend ms) testConfig
    mgr <- HC.newManager TLS.tlsManagerSettings
    base <- getTemporaryDirectory
    let dir = base </> "sabela-proxy-share-test"
    _ <- try (removeDirectoryRecursive dir) :: IO (Either SomeException ())
    store <- newShareStore dir
    users <- newUserStore (dir <> "-users") Nothing
    gallery <- newGalleryStore (dir <> "-gallery")
    app <- hubApp sm store users gallery mgr
    pure (app, store)

-- | App + its share and gallery stores (for download/feature tests).
makeAppFull :: IO (Application, ShareStore, GalleryStore)
makeAppFull = do
    ms <- newMockState
    sm <- newSessionManager (mockEcsBackend ms) testConfig
    mgr <- HC.newManager TLS.tlsManagerSettings
    base <- getTemporaryDirectory
    let dir = base </> "sabela-proxy-full-test"
        wipe d = try (removeDirectoryRecursive d) :: IO (Either SomeException ())
    mapM_ wipe [dir, dir <> "-users", dir <> "-gallery"]
    store <- newShareStore dir
    users <- newUserStore (dir <> "-users") Nothing
    gallery <- newGalleryStore (dir <> "-gallery")
    app <- hubApp sm store users gallery mgr
    pure (app, store, gallery)

{- | App with a "user@x" session and the data root pointed at @root@ (so a
fork's file write lands in a temp dir, not /mnt/sabela).
-}
makeAppForkable :: FilePath -> IO (Application, ShareStore, GalleryStore)
makeAppForkable root = do
    ms <- newMockState
    let cfg =
            testConfig
                { hcDockerConfig =
                    (hcDockerConfig testConfig){dcDataRoot = T.pack root}
                }
        wipe d = try (removeDirectoryRecursive d) :: IO (Either SomeException ())
    mapM_ wipe [root <> "-shares", root <> "-users", root <> "-gallery"]
    sm <- newSessionManager (mockEcsBackend ms) cfg
    mgr <- HC.newManager TLS.tlsManagerSettings
    store <- newShareStore (root <> "-shares")
    users <- newUserStore (root <> "-users") Nothing
    gallery <- newGalleryStore (root <> "-gallery")
    now <- getCurrentTime
    insertSession sm (UserSession (SessionId "usersid")) $
        Session (TaskId "") SStarting now (UserId "user@x") Authed Nothing
    app <- hubApp sm store users gallery mgr
    pure (app, store, gallery)

-- | An app with an admin ("admin@x") and a non-admin ("user@x") session.
makeAppSess :: IO Application
makeAppSess = do
    ms <- newMockState
    sm <- newSessionManager (mockEcsBackend ms) testConfig
    mgr <- HC.newManager TLS.tlsManagerSettings
    base <- getTemporaryDirectory
    let dir = base </> "sabela-proxy-sess-test"
    _ <- try (removeDirectoryRecursive dir) :: IO (Either SomeException ())
    store <- newShareStore dir
    users <- newUserStore (dir <> "-users") (Just "admin@x")
    gallery <- newGalleryStore (dir <> "-gallery")
    now <- getCurrentTime
    let mkSess uid = Session (TaskId "") SStarting now (UserId uid) Authed Nothing
    insertSession sm (UserSession (SessionId "adminsid")) (mkSess "admin@x")
    insertSession sm (UserSession (SessionId "usersid")) (mkSess "user@x")
    hubApp sm store users gallery mgr

mkShare :: Text -> Share
mkShare slug =
    Share
        { shareSlug = slug
        , shareOwner = "owner@x"
        , shareMode = ExpDashboard
        , shareCreatedAt = "2026-05-27T00:00:00Z"
        , shareTitle = "Untitled"
        }
