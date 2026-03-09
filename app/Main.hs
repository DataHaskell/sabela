module Main (main) where

import Control.Monad (unless)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Network.Wai.Handler.Warp (run)
import Sabela.Handlers (initGlobalEnv, setupReactive)
import Sabela.Server (initState, mkApp)
import System.Directory (
    doesDirectoryExist,
    doesFileExist,
    getCurrentDirectory,
    getHomeDirectory,
 )
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.FilePath ((</>))

main :: IO ()
main = do
    setLocaleEncoding utf8
    homeDir <- getHomeDirectory
    let defaultGlobal = homeDir </> ".sabela" </> "global.md"
    args <- getArgs
    case args of
        [] -> start 3000 "static" "." defaultGlobal
        [port] -> start (read port) "static" "." defaultGlobal
        [port, s] -> start (read port) s "." defaultGlobal
        [port, s, w] -> start (read port) s w defaultGlobal
        [port, s, w, g] -> start (read port) s w g
        _ -> do
            prog <- getProgName
            putStrLn $ "Usage: " ++ prog ++ " [port] [static-dir] [work-dir] [global-file]"
            putStrLn "  default port: 3000"
            putStrLn "  default static-dir: static"
            putStrLn "  default work-dir: . (current directory)"
            putStrLn "  default global-file: ~/.sabela/global.md"
            exitFailure

start :: Int -> FilePath -> FilePath -> FilePath -> IO ()
start port staticDir workDir globalFile = do
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
            st <- initState workDir mGlobalEnvFile globalDeps
            rn <- setupReactive st

            putStrLn $
                "sabela-server running on http://localhost:" ++ show port ++ "/index.html"
            run port (mkApp st rn staticDir)
