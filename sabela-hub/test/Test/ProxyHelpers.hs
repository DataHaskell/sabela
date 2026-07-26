{-# LANGUAGE OverloadedStrings #-}

module Test.ProxyHelpers (
    makeApp,
    makeAppWithStore,
    makeAppFull,
    makeAppForkable,
    makeAppSess,
    mkShare,
) where

import Control.Exception (SomeException, try)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Hub.Gallery (GalleryStore, newGalleryStore)
import Hub.Proxy (hubApp)
import Hub.Session (insertSession, newSessionManager)
import Hub.Share (Share (..), ShareStore, newShareStore)
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
import Network.Wai (Application)
import System.Directory (getTemporaryDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import Test.MockEcs (mockEcsBackend, newMockState, testConfig)

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
