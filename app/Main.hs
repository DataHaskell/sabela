module Main (main) where

import Control.Exception (finally)
import Control.Monad (when)
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Network.Wai.Handler.Warp (run)
import Sabela.Handlers (initGlobalEnv, initPreinstalledPackages, setupReactive)
import Sabela.Server (mkApp, newApp)
import Sabela.State (App (..))
import Sabela.State.Environment (Environment (..))
import System.Directory (
    doesDirectoryExist,
    getCurrentDirectory,
    getHomeDirectory,
    removeDirectoryRecursive,
 )
import System.Environment (getArgs, lookupEnv, setEnv)
import System.FilePath (takeDirectory, (</>))

main :: IO ()
main = do
    setLocaleEncoding utf8
    addElanToPath
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
    app <- newApp workDir allGlobalDeps
    rn <- setupReactive app
    putStrLn $ "sabela running on http://localhost:" ++ show port ++ "/index.html"
    run port (mkApp app rn) `finally` cleanupTmpDir app

-- | Add elan's bin directory to PATH so lake/lean are found.
addElanToPath :: IO ()
addElanToPath = do
    home <- getHomeDirectory
    let elanBin = home </> ".elan" </> "bin"
    exists <- doesDirectoryExist elanBin
    when exists $ do
        path <- fromMaybe "" <$> lookupEnv "PATH"
        setEnv "PATH" (elanBin ++ ":" ++ path)

cleanupTmpDir :: App -> IO ()
cleanupTmpDir app = do
    let tmpDir = envTmpDir (appEnv app)
    putStrLn $ "Cleaning up temp directory: " ++ tmpDir
    removeDirectoryRecursive tmpDir
