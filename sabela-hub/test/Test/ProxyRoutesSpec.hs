{-# LANGUAGE OverloadedStrings #-}

-- | Gallery + admin route tests, split out of 'Test.ProxySpec' for the module-size cap.
module Test.ProxyRoutesSpec (spec) where

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
spec =
    describe "Proxy: gallery + admin routes" $ do
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

        it "serves /gallery for a logged-in user (not the proxied notebook)" $ do
            app <- makeAppSess
            let req =
                    (setPath defaultRequest "/gallery")
                        { requestHeaders = [("Cookie", "_sabela_session=usersid")]
                        }
            resp <- runSession (request req) app
            simpleStatus resp `shouldBe` status200
            LC8.unpack (simpleBody resp) `shouldSatisfy` isInfixOf "/_hub/login"
            LC8.unpack (simpleBody resp)
                `shouldNotSatisfy` isInfixOf "Starting your notebook environment"

        it "405s an admin route method mismatch (not a proxied 200)" $ do
            app <- makeApp
            resp <- runSession (request (setPath defaultRequest "/_hub/admin/feature")) app
            simpleStatus resp `shouldBe` status405

        it "downloads a featured notebook's source (.md)" $ do
            (app, store, gallery) <- makeAppFull
            publishShare store (mkShare "ab12") "<h1>x</h1>" Nothing
            writeShareSource store "ab12" "# Iris\nplot iris"
            addFeatured gallery "ab12"
            resp <- runSession (request (setPath defaultRequest "/_hub/source/ab12")) app
            simpleStatus resp `shouldBe` status200
            LC8.unpack (simpleBody resp) `shouldSatisfy` isInfixOf "plot iris"
            lookup hContentType (simpleHeaders resp)
                `shouldBe` Just "text/markdown; charset=utf-8"

        it "downloads a non-featured share's source (public-by-URL)" $ do
            (app, store, _) <- makeAppFull
            publishShare store (mkShare "ab12") "<h1>x</h1>" Nothing
            writeShareSource store "ab12" "# just shared\nplot iris"
            resp <- runSession (request (setPath defaultRequest "/_hub/source/ab12")) app
            simpleStatus resp `shouldBe` status200
            LC8.unpack (simpleBody resp) `shouldSatisfy` isInfixOf "plot iris"

        it "404s a download when no source is stored (legacy/unknown slug)" $ do
            (app, store, _) <- makeAppFull
            publishShare store (mkShare "ab12") "<h1>x</h1>" Nothing
            resp <- runSession (request (setPath defaultRequest "/_hub/source/ab12")) app
            simpleStatus resp `shouldBe` status404

        it "rejects fork without a session (401)" $ do
            app <- makeApp
            let req = (setPath defaultRequest "/_hub/fork/ab12"){requestMethod = methodPost}
            resp <- runSession (request req) app
            simpleStatus resp `shouldBe` status401

        it "signed-out browser fork redirects to login and stashes the fork cookie" $ do
            app <- makeApp
            let req =
                    (setPath defaultRequest "/_hub/fork/ab12")
                        { requestMethod = methodPost
                        , requestHeaders = [(hAccept, "text/html")]
                        }
            resp <- runSession (request req) app
            simpleStatus resp `shouldBe` status303
            lookup hLocation (simpleHeaders resp) `shouldBe` Just "/_hub/login"
            C8.unpack (fromMaybe "" (lookup "Set-Cookie" (simpleHeaders resp)))
                `shouldSatisfy` isInfixOf "sabela_fork=ab12"

        it "forks a public notebook into the caller's work dir" $ do
            base <- getTemporaryDirectory
            let root = base </> "sabela-fork-test"
            _ <- try (removeDirectoryRecursive root) :: IO (Either SomeException ())
            (app, store, gallery) <- makeAppForkable root
            publishShare store (mkShare "ab12") "<h1>x</h1>" Nothing
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

        it "forks a non-featured share (any stored source is forkable)" $ do
            base <- getTemporaryDirectory
            let root = base </> "sabela-fork-test-unfeatured"
            _ <- try (removeDirectoryRecursive root) :: IO (Either SomeException ())
            (app, store, _) <- makeAppForkable root
            publishShare store (mkShare "ab12") "<h1>x</h1>" Nothing
            writeShareSource store "ab12" "# just shared"
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
            files <- listDirectory (root </> "users" </> "user_x")
            length (filter (isInfixOf "forked-") files) `shouldBe` 1

        it "signed-in browser fork redirects to the editor opening the fork" $ do
            base <- getTemporaryDirectory
            let root = base </> "sabela-fork-test-open"
            _ <- try (removeDirectoryRecursive root) :: IO (Either SomeException ())
            (app, store, gallery) <- makeAppForkable root
            publishShare store (mkShare "ab12") "<h1>x</h1>" Nothing
            writeShareSource store "ab12" "# Iris"
            addFeatured gallery "ab12"
            let req =
                    (setPath defaultRequest "/_hub/fork/ab12")
                        { requestMethod = methodPost
                        , requestHeaders =
                            [ ("Cookie", "_sabela_session=usersid")
                            , ("Origin", "http://localhost:8080")
                            , (hAccept, "text/html")
                            ]
                        }
            resp <- runSession (request req) app
            simpleStatus resp `shouldBe` status303
            C8.unpack (fromMaybe "" (lookup hLocation (simpleHeaders resp)))
                `shouldSatisfy` isInfixOf "/?open=forked-"

        it "rejects a cross-origin fork (403)" $ do
            base <- getTemporaryDirectory
            (app, store, gallery) <- makeAppForkable (base </> "sabela-fork-test2")
            publishShare store (mkShare "ab12") "<h1>x</h1>" Nothing
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
            publishShare store (mkShare "abc123") "<h1>shared dash</h1>" Nothing
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
