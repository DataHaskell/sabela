module Main (main) where

import qualified Data.Set as S
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Network.Wai.Handler.Warp (run)
import Sabela.Handlers (initGlobalEnv, initPreinstalledPackages, setupReactive)
import Sabela.Server (initState, mkApp)
import System.Directory (
    getCurrentDirectory,
    getHomeDirectory,
 )
import System.Environment (getArgs)
import System.FilePath (takeDirectory, (</>))

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
    st <- initState workDir allGlobalDeps
    rn <- setupReactive st
    putStrLn $ "sabela running on http://localhost:" ++ show port ++ "/index.html"
    run port (mkApp st rn)
