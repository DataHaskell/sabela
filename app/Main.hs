module Main (main) where

import Control.Exception (finally)
import Control.Monad (unless, when)
import Data.Maybe (isJust)
import qualified Data.Set as S
import qualified Data.Text as T
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.Wai.Handler.Warp (run)
import Sabela.Handlers (initGlobalEnv, initPreinstalledPackages, setupReactive)
import Sabela.Server (mkApp, newApp)
import Sabela.State (App (..))
import Sabela.State.Environment (Environment (..))
import System.Directory (
    createDirectoryIfMissing,
    doesFileExist,
    getCurrentDirectory,
    getHomeDirectory,
    removeDirectoryRecursive,
    removeFile,
 )
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure)
import System.FilePath (splitSearchPath, takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)
import System.Process (getCurrentPid)
import Text.Read (readMaybe)

import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Lazy as LBS

main :: IO ()
main = do
    setLocaleEncoding utf8
    homeDir <- getHomeDirectory
    let defaultGlobal = homeDir </> ".sabela" </> "global.md"
    args <- getArgs
    case args of
        [] -> start 3000 "." defaultGlobal []
        [port] -> withPort port $ \p -> start p "." defaultGlobal []
        [port, w] -> withPort port $ \p -> start p w defaultGlobal []
        [port, w, g] -> withPort port $ \p -> start p w g []
        (port : w : g : pkgs) -> withPort port $ \p -> start p w g pkgs

-- | Parse the port argument, or exit with a clear message (never 'read'-crash).
withPort :: String -> (Int -> IO ()) -> IO ()
withPort s k = case readMaybe s of
    Just p -> k p
    Nothing -> hPutStrLn stderr ("Invalid port: " <> s) >> exitFailure

start :: Int -> FilePath -> FilePath -> [String] -> IO ()
start port workDir globalFile pkgs = do
    cwd <- getCurrentDirectory
    putStrLn $ "Working directory: " ++ cwd
    putStrLn $ "File explorer root: " ++ workDir
    globalDeps <- initGlobalEnv globalFile
    preinstalledDeps <- initPreinstalledPackages (takeDirectory globalFile) pkgs
    let allGlobalDeps = globalDeps `S.union` preinstalledDeps
    httpMgr <- newTlsManager
    mAiToken <- fmap T.pack <$> lookupEnv "SABELA_AI_TOKEN"
    mLocalPkgs <- lookupEnv "SABELA_LOCAL_PACKAGES"
    let localPkgs = case mLocalPkgs of
            Just s | not (null s) -> splitSearchPath s
            _ -> []
    unless (null localPkgs) $
        putStrLn ("Local package overlays: " ++ unwords localPkgs)
    app <- newApp workDir allGlobalDeps (Just httpMgr) mAiToken localPkgs
    rn <- setupReactive app
    registryFile <- writeDiscoveryRegistry port workDir mAiToken
    putStrLn $ "sabela running on http://localhost:" ++ show port ++ "/index.html"
    case mAiToken of
        Just _ -> putStrLn "  /api/ai/* requires Authorization: Bearer <SABELA_AI_TOKEN>"
        Nothing -> pure ()
    run port (mkApp app rn)
        `finally` ( do
                        cleanupRegistry registryFile
                        cleanupTmpDir app
                  )

cleanupTmpDir :: App -> IO ()
cleanupTmpDir app = do
    let tmpDir = envTmpDir (appEnv app)
    putStrLn $ "Cleaning up temp directory: " ++ tmpDir
    removeDirectoryRecursive tmpDir

-- | Write a discovery registry file so local CLI clients can auto-find us.
writeDiscoveryRegistry :: Int -> FilePath -> Maybe T.Text -> IO FilePath
writeDiscoveryRegistry port workDir mToken = do
    home <- getHomeDirectory
    let regDir = home </> ".local" </> "state" </> "sabela" </> "servers"
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
