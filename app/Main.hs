module Main (main) where

import Control.Exception (finally)
import Control.Monad (when)
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
import System.FilePath (takeDirectory, (</>))
import System.Posix.Process (getProcessID)

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
        [port] -> start (read port) "." defaultGlobal []
        [port, w] -> start (read port) w defaultGlobal []
        [port, w, g] -> start (read port) w g []
        (port : w : g : pkgs) -> start (read port) w g pkgs

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
    app <- newApp workDir allGlobalDeps (Just httpMgr) mAiToken
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
    pid <- getProcessID
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
