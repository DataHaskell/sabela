{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Obtains a package's released sdist: the cabal package cache, then
sabela's mirror, then one bounded Hackage fetch, cached for later calls. The
tarball is the only source a not-installed package has; @read_source@ reads it.
-}
module Sabela.AI.Sdist (
    SdistProvenance (..),
    sdistUrl,
    sdistStatus,
    cabalCacheDir,
    sdistCacheDir,
    cachedVersions,
    acquireSdist,
    drainToCap,
    maxSdistBytes,
) where

import Control.Exception (SomeException, try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.List (sortBy)
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client (Manager)
import System.Directory (
    XdgDirectory (XdgCache, XdgData),
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getHomeDirectory,
    getXdgDirectory,
    listDirectory,
    renameFile,
 )
import System.Environment (lookupEnv)
import System.FilePath ((</>))

import Sabela.AI.Fetch (
    FetchSpec (..),
    OverCap (..),
    drainBounded,
    fetchBounded,
    statusError,
 )
import Sabela.AI.ReadSourceArgs (validPackageName, validVersionText)
import Sabela.AI.VersionKey (versionKey)

data SdistProvenance = CabalCache | CacheLocal | Fetched
    deriving (Eq, Show)

-- | Sdists can run to a few megabytes; past this one is not worth a call.
maxSdistBytes :: Int
maxSdistBytes = 8 * 1024 * 1024

sdistUrl :: Text -> Text -> Text
sdistUrl pkg ver =
    "https://hackage.haskell.org/package/"
        <> pv
        <> "/"
        <> pv
        <> ".tar.gz"
  where
    pv = pkg <> "-" <> ver

{- | Where cabal caches sdists: the test override, then @$CABAL_DIR@, then
the classic @~/.cabal@ layout when it exists, then cabal's XDG cache home.
-}
cabalCacheDir :: IO FilePath
cabalCacheDir = do
    mOverride <- lookupEnv "SABELA_CABAL_PACKAGES_DIR"
    mCabalDir <- lookupEnv "CABAL_DIR"
    case (nonEmpty mOverride, nonEmpty mCabalDir) of
        (Just p, _) -> pure p
        (Nothing, Just d) -> pure (hackageUnder (d </> "packages"))
        _ -> do
            home <- getHomeDirectory
            let classic = hackageUnder (home </> ".cabal" </> "packages")
            classicThere <- doesDirectoryExist classic
            if classicThere
                then pure classic
                else
                    hackageUnder
                        <$> getXdgDirectory XdgCache ("cabal" </> "packages")
  where
    nonEmpty (Just p) | not (null p) = Just p
    nonEmpty _ = Nothing
    hackageUnder d = d </> "hackage.haskell.org"

sdistCacheDir :: IO FilePath
sdistCacheDir = do
    mEnv <- lookupEnv "SABELA_SDIST_CACHE_DIR"
    case mEnv of
        Just p | not (null p) -> pure p
        _ -> getXdgDirectory XdgData ("sabela" </> "sdists")

-- | SABELA_NO_NETWORK set non-empty forbids the Hackage fetch entirely.
networkDisabled :: IO Bool
networkDisabled =
    maybe False (not . null) <$> lookupEnv "SABELA_NO_NETWORK"

-- | The versions of a package the cabal cache already holds, newest first.
cachedVersions :: Text -> IO [Text]
cachedVersions pkg = do
    dir <- cabalCacheDir
    eNames <-
        try (listDirectory (dir </> T.unpack pkg)) ::
            IO (Either SomeException [FilePath])
    pure $ case eNames of
        Left _ -> []
        Right names ->
            sortBy
                (comparing (Down . versionKey))
                [T.pack n | n <- names, isVersionDir n]
  where
    isVersionDir = all (\c -> c `elem` ("0123456789." :: String))

acquireSdist ::
    Maybe Manager ->
    Text ->
    Text ->
    IO (Either Text (SdistProvenance, BL.ByteString))
acquireSdist mMgr pkg ver
    | not (validPackageName pkg) =
        pure (Left ("invalid package name: " <> pkg))
    | not (validVersionText ver) =
        pure (Left ("invalid version: " <> ver))
    | otherwise = do
        cabalDir <- cabalCacheDir
        mirrorDir <- sdistCacheDir
        noNet <- networkDisabled
        let file = T.unpack (pkg <> "-" <> ver) <> ".tar.gz"
            cabalPath = cabalDir </> T.unpack pkg </> T.unpack ver </> file
            mirrorPath = mirrorDir </> file
        fromCabal <- readIfPresent cabalPath
        case fromCabal of
            Just bytes -> pure (Right (CabalCache, bytes))
            Nothing -> do
                fromMirror <- readIfPresent mirrorPath
                case fromMirror of
                    Just bytes -> pure (Right (CacheLocal, bytes))
                    Nothing -> case mMgr of
                        Just mgr
                            | not noNet ->
                                fetchInto mgr mirrorDir mirrorPath
                        _ -> pure (Left (missPr cabalPath mirrorPath))
  where
    missPr cabalPath mirrorPath =
        "the "
            <> pkg
            <> "-"
            <> ver
            <> " sdist is in neither local cache ("
            <> T.pack cabalPath
            <> ", "
            <> T.pack mirrorPath
            <> ") and no network fetch is available"
    fetchInto mgr mirrorDir mirrorPath = do
        r <- fetchSdist mgr pkg ver
        case r of
            Left e -> pure (Left e)
            Right bytes -> do
                _ <-
                    try (atomicWrite mirrorDir mirrorPath bytes) ::
                        IO (Either SomeException ())
                pure (Right (Fetched, bytes))

-- | Lands whole or not at all: a temp name in the same directory, then rename.
atomicWrite :: FilePath -> FilePath -> BL.ByteString -> IO ()
atomicWrite dir path bytes = do
    createDirectoryIfMissing True dir
    let tmp = path <> ".part"
    BL.writeFile tmp bytes
    renameFile tmp path

readIfPresent :: FilePath -> IO (Maybe BL.ByteString)
readIfPresent p = do
    ok <- doesFileExist p
    if not ok
        then pure Nothing
        else do
            r <- try (BS.readFile p) :: IO (Either SomeException BS.ByteString)
            pure (either (const Nothing) (Just . BL.fromStrict) r)

fetchSdist :: Manager -> Text -> Text -> IO (Either Text BL.ByteString)
fetchSdist mgr pkg ver = fetchBounded fs mgr (sdistUrl pkg ver)
  where
    fs =
        FetchSpec
            { fsService = "Hackage"
            , fsHeaders = [("User-Agent", "sabela")]
            , fsCap = maxSdistBytes
            , fsOverCap = FailOverCap (overCapMsg maxSdistBytes)
            , fsStatus = sdistStatus pkg ver
            }

-- | The Hackage status ladder for one sdist, pure so its branches test.
sdistStatus :: Text -> Text -> Int -> Maybe Text
sdistStatus pkg ver =
    statusError
        "Hackage"
        [
            ( 404
            , "Hackage has no sdist for "
                <> pkg
                <> "-"
                <> ver
                <> "; the version may be wrong — pass `version` or omit it"
            )
        ]

{- | Reads a body whole or not at all: a truncated tarball would parse as a
shorter archive, so over the cap is an error, never a prefix.
-}
drainToCap :: Int -> IO BS.ByteString -> IO (Either Text BL.ByteString)
drainToCap cap = drainBounded cap (FailOverCap (overCapMsg cap))

overCapMsg :: Int -> Text
overCapMsg cap =
    "the sdist exceeds the "
        <> T.pack (show cap)
        <> "-byte fetch cap, so it was not downloaded"
