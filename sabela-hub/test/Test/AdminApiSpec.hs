{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Admin curation API: admin-gating (non-admin / no-session → 403 JSON),
the config-anchored Origin CSRF check on mutations, and the JSON wire shapes
('AdminApiWireSpec'). Driven through @adminDispatch@ as a standalone WAI
'Application' (not through @hubApp@, whose signature changes only at step 12).
-}
module Test.AdminApiSpec (spec) where

import Control.Exception (SomeException, finally, try)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (getCurrentTime)
import Hub.Admin.Api (adminDispatch)
import Hub.Gallery (GalleryStore, addFeatured, newGalleryStore)
import Hub.OAuth (generateRandomToken)
import Hub.Session (insertSession, newSessionManager)
import Hub.Share (Share (..), ShareStore, newShareStore, publishShare)
import Hub.Types
import Hub.Users (newUserStore)
import Network.HTTP.Types
import Network.Wai (Application, defaultRequest, requestHeaders, requestMethod)
import Network.Wai.Test (
    SRequest (..),
    SResponse,
    runSession,
    setPath,
    simpleBody,
    simpleHeaders,
    simpleStatus,
 )
import qualified Network.Wai.Test as WT
import System.Directory (getTemporaryDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import Test.Hspec
import Test.MockEcs (mockEcsBackend, newMockState, testConfig)

-- | testConfig's GOOGLE_REDIRECT_URI → canonical origin for the CSRF check.
canonicalOrigin :: BL.ByteString
canonicalOrigin = "http://localhost:8080"

mkShare :: Text -> Text -> Share
mkShare slug title =
    Share
        { shareSlug = slug
        , shareOwner = "alice@x"
        , shareMode = ExpDashboard
        , shareCreatedAt = "2026-05-27T00:00:00Z"
        , shareTitle = title
        }

withAdminApp :: (Application -> ShareStore -> GalleryStore -> IO a) -> IO a
withAdminApp act = do
    base <- getTemporaryDirectory
    tok <- generateRandomToken
    let root = base </> "sabela-admin-test-" <> T.unpack (T.take 8 tok)
    ms <- newMockState
    sm <- newSessionManager (mockEcsBackend ms) testConfig
    users <- newUserStore (root </> "users") (Just "admin@x")
    gallery <- newGalleryStore (root </> "gallery")
    shares <- newShareStore (root </> "shares")
    now <- getCurrentTime
    let mkSess uid = Session (TaskId "") SStarting now (UserId uid) Authed Nothing
    insertSession sm (UserSession (SessionId "adminsid")) (mkSess "admin@x")
    insertSession sm (UserSession (SessionId "usersid")) (mkSess "user@x")
    act (adminDispatch sm users gallery shares) shares gallery
        `finally` (try (removeDirectoryRecursive root) :: IO (Either SomeException ()))

cookieHdr :: BL.ByteString -> Header
cookieHdr sid = ("Cookie", "_sabela_session=" <> BL.toStrict sid)

-- | A GET with the given session cookie.
getReq :: BL.ByteString -> Text -> WT.Session SResponse
getReq sid path =
    WT.request $
        setPath defaultRequest{requestHeaders = [cookieHdr sid]} (TE.encodeUtf8 path)

-- | A body-bearing mutation with optional Origin header.
mutReq ::
    Method ->
    Text ->
    BL.ByteString ->
    [Header] ->
    BL.ByteString ->
    WT.Session SResponse
mutReq method path sid hdrs bdy =
    WT.srequest $
        SRequest
            ( setPath
                defaultRequest
                    { requestMethod = method
                    , requestHeaders = cookieHdr sid : hdrs
                    }
                (TE.encodeUtf8 path)
            )
            bdy

originHdr :: BL.ByteString -> Header
originHdr o = ("Origin", BL.toStrict o)

respBody :: SResponse -> String
respBody = LC8.unpack . simpleBody

spec :: Spec
spec = describe "Hub.Admin.Api" $ do
    describe "admin gating" $ do
        it "403 JSON for an authenticated non-admin" $ withAdminApp $ \app _ _ -> do
            r <- runSession (getReq "usersid" "/_hub/admin/shares") app
            simpleStatus r `shouldBe` status403
            lookup hContentType (simpleHeaders r) `shouldBe` Just "application/json"
        it "403 for no session at all" $ withAdminApp $ \app _ _ -> do
            r <- runSession (WT.request (setPath defaultRequest "/_hub/admin/shares")) app
            simpleStatus r `shouldBe` status403
        it "200 for the admin" $ withAdminApp $ \app _ _ -> do
            r <- runSession (getReq "adminsid" "/_hub/admin/shares") app
            simpleStatus r `shouldBe` status200

    describe "AdminApiWireSpec — GET /_hub/admin/shares" $
        it "returns the curation fields and the dangling list" $
            withAdminApp $ \app shares gallery -> do
                publishShare shares (mkShare "aa11" "Iris") "x"
                addFeatured gallery "aa11"
                r <- runSession (getReq "adminsid" "/_hub/admin/shares") app
                let b = respBody r
                b `shouldSatisfy` isInfixOf "\"shares\""
                b `shouldSatisfy` isInfixOf "\"dangling\""
                b `shouldSatisfy` isInfixOf "\"featured\""
                b `shouldSatisfy` isInfixOf "\"inCollections\""
                b `shouldSatisfy` isInfixOf "\"tags\""
                b `shouldSatisfy` isInfixOf "aa11"

    describe "CSRF Origin check on mutations" $ do
        it "rejects a missing Origin (403)" $ withAdminApp $ \app shares _ -> do
            publishShare shares (mkShare "aa11" "Iris") "x"
            r <-
                runSession
                    (mutReq methodPost "/_hub/admin/feature" "adminsid" [] "{\"slug\":\"aa11\"}")
                    app
            simpleStatus r `shouldBe` status403
        it "rejects a cross-origin request (403)" $ withAdminApp $ \app shares _ -> do
            publishShare shares (mkShare "aa11" "Iris") "x"
            r <-
                runSession
                    ( mutReq
                        methodPost
                        "/_hub/admin/feature"
                        "adminsid"
                        [originHdr "http://evil.example"]
                        "{\"slug\":\"aa11\"}"
                    )
                    app
            simpleStatus r `shouldBe` status403
        it "accepts the canonical Origin (200) and features idempotently" $
            withAdminApp $ \app shares gallery -> do
                publishShare shares (mkShare "aa11" "Iris") "x"
                let feature =
                        mutReq
                            methodPost
                            "/_hub/admin/feature"
                            "adminsid"
                            [originHdr canonicalOrigin]
                            "{\"slug\":\"aa11\"}"
                r1 <- runSession feature app
                simpleStatus r1 `shouldBe` status200
                r2 <- runSession feature app
                simpleStatus r2 `shouldBe` status200
                picker <- runSession (getReq "adminsid" "/_hub/admin/shares") app
                respBody picker `shouldSatisfy` isInfixOf "\"featured\":true"

    describe "collection + tag wire shapes" $ do
        it "POST /_hub/admin/collection returns a cid" $
            withAdminApp $ \app _ _ -> do
                r <-
                    runSession
                        ( mutReq
                            methodPost
                            "/_hub/admin/collection"
                            "adminsid"
                            [originHdr canonicalOrigin]
                            "{\"title\":\"Viz\",\"description\":\"plots\"}"
                        )
                        app
                simpleStatus r `shouldBe` status200
                respBody r `shouldSatisfy` isInfixOf "\"cid\""
        it "PUT /_hub/admin/tags/{id} rejects an invalid tag (400)" $
            withAdminApp $ \app shares gallery -> do
                publishShare shares (mkShare "aa11" "Iris") "x"
                addFeatured gallery "aa11"
                r <-
                    runSession
                        ( mutReq
                            methodPut
                            "/_hub/admin/tags/aa11"
                            "adminsid"
                            [originHdr canonicalOrigin]
                            "{\"tags\":[\"BAD\"]}"
                        )
                        app
                simpleStatus r `shouldBe` status400
                respBody r `shouldSatisfy` isInfixOf "invalid"
