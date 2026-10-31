{-# LANGUAGE CPP #-}

module Main (main) where

{- HLINT ignore "Use fewer imports" -}
import Control.Applicative ((<|>))
import Control.Exception (finally)
import Control.Monad (unless, void, when)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as S
import Data.String (fromString)
import qualified Data.Text as T
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.Wai.Handler.Warp (
    Settings,
    defaultSettings,
    runSettings,
    setHost,
    setPort,
 )
import Sabela.AI.Provenance (stateBase)
import Sabela.AI.SearchCache (
    adoptHackageDb,
    adoptLocalDb,
    searchCacheReport,
 )
import Sabela.Handlers (
    buildTimeSupportDir,
    initGlobalEnv,
    initPreinstalledPackages,
    setupReactive,
    shutdownAllSessions,
 )
import Sabela.Server (mkApp, newApp)
import Sabela.Server.Access (
    Access (..),
    accessMiddleware,
    isLoopbackHost,
    resolveAccess,
 )
import Sabela.Session.SupportSource (materializeSupportSource)
import Sabela.State (App (..))
import Sabela.State.Environment (Environment (..))
import System.Directory (
    createDirectoryIfMissing,
    doesFileExist,
    getCurrentDirectory,
    getHomeDirectory,
    makeAbsolute,
    removeDirectoryRecursive,
    removeFile,
 )
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure)
import System.FilePath (splitSearchPath, takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)
import System.Process (getCurrentPid)
import System.Timeout (timeout)
import Text.Read (readMaybe)

#if !defined(mingw32_HOST_OS)
import Control.Concurrent (myThreadId, throwTo)
import Control.Exception (AsyncException (UserInterrupt))
import Data.IORef (atomicModifyIORef', newIORef)
import System.Posix.Signals (
    Handler (Catch, Ignore),
    installHandler,
    sigHUP,
    sigINT,
    sigTERM,
 )
#endif

import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Lazy as LBS

main :: IO ()
main = do
    setLocaleEncoding utf8
    homeDir <- getHomeDirectory
    let defaultGlobal = homeDir </> ".sabela" </> "global.md"
    (hostFlag, args) <- splitHostFlag <$> getArgs
    hostEnv <- lookupEnv "SABELA_HOST"
    let host = fromMaybe "127.0.0.1" (hostFlag <|> hostEnv)
    case args of
        [] -> start host 3000 "." defaultGlobal []
        [port] -> withPort port $ \p -> start host p "." defaultGlobal []
        [port, w] -> withPort port $ \p -> start host p w defaultGlobal []
        [port, w, g] -> withPort port $ \p -> start host p w g []
        (port : w : g : pkgs) -> withPort port $ \p -> start host p w g pkgs

-- | Pull @--host H@ out of the positional arguments.
splitHostFlag :: [String] -> (Maybe String, [String])
splitHostFlag ("--host" : h : rest) = (Just h, snd (splitHostFlag rest))
splitHostFlag (a : rest) = (a :) <$> splitHostFlag rest
splitHostFlag [] = (Nothing, [])

warpSettings :: String -> Int -> Settings
warpSettings host port = setHost (fromString host) (setPort port defaultSettings)

-- | The URL to print at startup, carrying the token when one is required.
startUrl :: String -> Int -> Access -> String
startUrl host port access =
    "http://" ++ shown ++ ":" ++ show port ++ "/index.html" ++ tokenQuery
  where
    shown = if isLoopbackHost host then "localhost" else host
    tokenQuery = case access of
        TokenRequired t -> "?token=" ++ T.unpack t
        Open -> ""

-- | Parse the port argument, or exit with a clear message (never 'read'-crash).
withPort :: String -> (Int -> IO ()) -> IO ()
withPort s k = case readMaybe s of
    Just p -> k p
    Nothing -> hPutStrLn stderr ("Invalid port: " <> s) >> exitFailure

{- | Resolve @sabela-notebook@ when @SABELA_LOCAL_PACKAGES@ is unset: the
build-time path, then a sibling under the working directory, then the sources
embedded at build time, unpacked into the XDG data directory.
-}
locateSupportSource :: IO FilePath
locateSupportSource = go [buildTimeSupportDir, "sabela-notebook"]
  where
    go [] = materializeSupportSource
    go (dir : rest) = do
        present <- doesFileExist (dir </> "sabela-notebook.cabal")
        if present then makeAbsolute dir else go rest

{- | Where a local search index may live: the repository this binary was built
from, then the working directory, so a checkout that has one is used and an
install that has none simply reports none.
-}
searchCacheRoots :: IO [FilePath]
searchCacheRoots = do
    cwd <- getCurrentDirectory
    buildRoot <- makeAbsolute (takeDirectory buildTimeSupportDir)
    pure [buildRoot, cwd]

start :: String -> Int -> FilePath -> FilePath -> [String] -> IO ()
start host port workDir globalFile pkgs = do
    access <-
        resolveAccess host >>= either (\e -> hPutStrLn stderr e >> exitFailure) pure
    cwd <- getCurrentDirectory
    putStrLn $ "Working directory: " ++ cwd
    putStrLn $ "File explorer root: " ++ workDir
    globalDeps <- initGlobalEnv globalFile
    preinstalledDeps <- initPreinstalledPackages (takeDirectory globalFile) pkgs
    let allGlobalDeps = globalDeps `S.union` preinstalledDeps
    httpMgr <- newTlsManager
    mAiToken <- fmap T.pack <$> lookupEnv "SABELA_AI_TOKEN"
    mLocalPkgs <- lookupEnv "SABELA_LOCAL_PACKAGES"
    localPkgs <- case mLocalPkgs of
        Just s | not (null s) -> pure (splitSearchPath s)
        _ -> pure <$> locateSupportSource
    unless (null localPkgs) $
        putStrLn ("Local package overlays: " ++ unwords localPkgs)
    app <- newApp workDir allGlobalDeps (Just httpMgr) mAiToken localPkgs
    rn <- setupReactive app
    registryFile <- writeDiscoveryRegistry port workDir mAiToken
    roots <- searchCacheRoots
    adopted <- adoptLocalDb roots
    mapM_ (\p -> putStrLn ("  search cache: adopted local index " ++ p)) adopted
    adoptedHackage <- adoptHackageDb roots
    mapM_
        (\p -> putStrLn ("  search cache: adopted Hackage index " ++ p))
        adoptedHackage
    searchCacheReport roots >>= mapM_ (putStrLn . T.unpack)
    putStrLn $ "sabela running on " ++ startUrl host port access
    when (access == Open && not (isLoopbackHost host)) $
        putStrLn "  WARNING: network bind with SABELA_AUTH=none; every route is open"
    case mAiToken of
        Just _ -> putStrLn "  /api/ai/* requires Authorization: Bearer <SABELA_AI_TOKEN>"
        Nothing -> pure ()
    installShutdownHandlers
    runSettings (warpSettings host port) (accessMiddleware access (mkApp app rn))
        `finally` ( do
                        shutdownAllSessions app
                        cleanupRegistry registryFile
                        void (timeout 3000000 (cleanupTmpDir app))
                  )

#if defined(mingw32_HOST_OS)
installShutdownHandlers :: IO ()
installShutdownHandlers = pure ()
#else
installShutdownHandlers :: IO ()
installShutdownHandlers = do
    mainTid <- myThreadId
    fired <- newIORef False
    let trigger = do
            first <- atomicModifyIORef' fired (\b -> (True, not b))
            when first (throwTo mainTid UserInterrupt)
    _ <- installHandler sigTERM (Catch trigger) Nothing
    _ <- installHandler sigINT (Catch trigger) Nothing
    oldHup <- installHandler sigHUP (Catch trigger) Nothing
    case oldHup of
        Ignore -> void (installHandler sigHUP Ignore Nothing)
        _ -> pure ()
#endif

cleanupTmpDir :: App -> IO ()
cleanupTmpDir app = do
    let tmpDir = envTmpDir (appEnv app)
    putStrLn $ "Cleaning up temp directory: " ++ tmpDir
    removeDirectoryRecursive tmpDir

-- | Write a discovery registry file so local CLI clients can auto-find us.
writeDiscoveryRegistry :: Int -> FilePath -> Maybe T.Text -> IO FilePath
writeDiscoveryRegistry port workDir mToken = do
    base <- stateBase
    let regDir = base </> "sabela" </> "servers"
        regFile = regDir </> (show port ++ ".json")
    createDirectoryIfMissing True regDir
    pid <- getCurrentPid
    let tokenHint = fmap (T.take 4) mToken
        body =
            object
                [ "pid" .= show pid
                , "port" .= port
                , "baseUrl" .= ("http://localhost:" ++ show port)
                , "workDir" .= workDir
                , "authRequired" .= isJust mToken
                , "tokenHint" .= tokenHint
                ]
    LBS.writeFile regFile (encode body)
    pure regFile

cleanupRegistry :: FilePath -> IO ()
cleanupRegistry f = do
    exists <- doesFileExist f
    when exists (removeFile f)
