{- | How a GHCi session is invoked: the @cabal repl@ argument list and the RTS
options it carries. Capabilities and max-heap come from the environment, with
a nursery sized per capability so a wide -N does not multiply allocation.
-}
module Sabela.Session.Process.Invocation (
    ghciProcessSpec,
    ghciArgs,
    rtsGhcOptions,
) where

import Data.Maybe (maybeToList)
import GHC.Conc (getNumProcessors)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.Process (CreateProcess, proc)
import Text.Read (readMaybe)

import Sabela.Session (SessionConfig (..))
import Sabela.Session.Proc (sessionProcessSpec)

ghciProcessSpec :: SessionConfig -> IO CreateProcess
ghciProcessSpec cfg = do
    mGhc <- lookupEnv "GHC"
    mHeap <- lookupEnv "SABELA_GHCI_MAXHEAP"
    caps <- resolveGhciCaps
    let compilerArgs = ["--with-compiler=" ++ ghc | ghc <- maybeToList mGhc]
        args = ghciArgs cfg (rtsGhcOptions caps mHeap) ++ compilerArgs
    pure (sessionProcessSpec (Just (scWorkDir cfg)) (proc "cabal" args))

resolveGhciCaps :: IO Int
resolveGhciCaps = do
    mCaps <- lookupEnv "SABELA_GHCI_CAPS"
    case mCaps >>= readMaybe of
        Just n | n > 0 -> pure n
        _ -> min detectedCapsCeiling <$> getNumProcessors

detectedCapsCeiling :: Int
detectedCapsCeiling = 8

ghciArgs :: SessionConfig -> String -> [String]
ghciArgs cfg rtsOpts =
    storeArgs
        ++ [ "repl"
           , "exe:main"
           , "--project-dir=" ++ scProjectDir cfg
           , "--builddir=" ++ scProjectDir cfg </> "dist-newstyle"
           , "-v1"
           , "--repl-options=-odir " ++ objDir ++ " -hidir " ++ objDir ++ jsonDiag
           , "--ghc-options=" ++ rtsOpts
           ]
  where
    storeArgs = maybe [] (\dir -> ["--store-dir=" ++ dir]) (scCabalStoreDir cfg)
    objDir = scProjectDir cfg </> "ghci-objs"
    jsonDiag
        | scJsonDiagnostics cfg = " -fdiagnostics-as-json"
        | otherwise = ""

rtsGhcOptions :: Int -> Maybe String -> String
rtsGhcOptions caps mHeap =
    concat
        [ "+RTS -N"
        , show n
        , " -A"
        , show (nurseryMb n)
        , "m -n4m -H1G"
        , heap
        , " -RTS"
        ]
  where
    n = max 1 caps
    heap = case mHeap of
        Nothing -> " -M" ++ defaultMaxHeap
        Just "0" -> ""
        Just h -> " -M" ++ h

nurseryMb :: Int -> Int
nurseryMb caps = max nurseryFloorMb (nurseryTotalMb `div` max 1 caps)

nurseryTotalMb :: Int
nurseryTotalMb = 512

nurseryFloorMb :: Int
nurseryFloorMb = 16

defaultMaxHeap :: String
defaultMaxHeap = "8g"
