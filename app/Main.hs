module Main (main) where

import Control.Monad (unless)
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Network.Wai.Handler.Warp (run)
import Sabela.Handlers (initGlobalEnv, initPreinstalledPackages, setupReactive)
import Sabela.Server (initState, mkApp)
import System.Directory (
    doesDirectoryExist,
    doesFileExist,
    getCurrentDirectory,
    getHomeDirectory,
 )
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory, (</>))

main :: IO ()
main = do
    setLocaleEncoding utf8
    homeDir <- getHomeDirectory
    let defaultGlobal = homeDir </> ".sabela" </> "global.md"
    args <- getArgs
    case args of
        [] -> start 3000 "static" "." defaultGlobal []
        [port] -> start (read port) "static" "." defaultGlobal []
        [port, s] -> start (read port) s "." defaultGlobal []
        [port, s, w] -> start (read port) s w defaultGlobal []
        (port : s : w : g : pkgs) -> start (read port) s w g pkgs

start :: Int -> FilePath -> FilePath -> FilePath -> [String] -> IO ()
start port staticDir workDir globalFile pkgs = do
    cwd <- getCurrentDirectory
    putStrLn $ "Working directory: " ++ cwd
    putStrLn $ "File explorer root: " ++ workDir

    dirExists <- doesDirectoryExist staticDir
    if not dirExists
        then do
            putStrLn $ "Error: static directory not found: " ++ staticDir
            putStrLn "Make sure you run sabela-server from the project root,"
            putStrLn "or pass the path explicitly: sabela-server 3000 /path/to/static"
            exitFailure
        else do
            indexExists <- doesFileExist (staticDir ++ "/index.html")
            unless indexExists $
                putStrLn $
                    "Warning: " ++ staticDir ++ "/index.html not found"

            putStrLn $ "Serving static files from: " ++ staticDir

            (mGlobalEnvFile, globalDeps) <- initGlobalEnv globalFile
            (mPreinstalledEnvFile, preinstalledDeps) <-
                initPreinstalledPackages
                    (takeDirectory globalFile)
                    pkgs
            let allEnvFiles = catMaybes [mGlobalEnvFile, mPreinstalledEnvFile]
                allGlobalDeps = globalDeps `S.union` preinstalledDeps
            st <- initState workDir allEnvFiles allGlobalDeps
            rn <- setupReactive st

            putStrLn $
                "sabela-server running on http://localhost:" ++ show port ++ "/index.html"
            run port (mkApp st rn staticDir)
