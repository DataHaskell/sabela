module Main (main) where

import Control.Monad (when)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Network.Wai.Handler.Warp (run)
import Sabela.Handlers (setupReactive)
import Sabela.Server (initState, mkApp)
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)

main :: IO ()
main = do
    setLocaleEncoding utf8
    args <- getArgs
    case args of
        [] -> start 3000 "static" "."
        [port] -> start (read port) "static" "."
        [port, s] -> start (read port) s "."
        [port, s, w] -> start (read port) s w
        _ -> do
            prog <- getProgName
            putStrLn $ "Usage: " ++ prog ++ " [port] [static-dir] [work-dir]"
            putStrLn "  default port: 3000"
            putStrLn "  default static-dir: static"
            putStrLn "  default work-dir: . (current directory)"
            exitFailure

start :: Int -> FilePath -> FilePath -> IO ()
start port staticDir workDir = do
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
            when (not indexExists) $ putStrLn $ "Warning: " ++ staticDir ++ "/index.html not found"

            putStrLn $ "Serving static files from: " ++ staticDir

            st <- initState workDir
            rn <- setupReactive st

            putStrLn $
                "sabela-server running on http://localhost:" ++ show port ++ "/index.html"
            run port (mkApp st rn staticDir)
