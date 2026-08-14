{-# LANGUAGE ScopedTypeVariables #-}

{- | One resolution ladder for the machine-local search data files: an
operator override wins outright, then the repo-relative default, then the
mirror @tools/update-search-cache.sh@ writes under the XDG data directory.
-}
module Sabela.AI.DataFiles (
    dataFilePaths,
    resolveDataFile,
) where

import Control.Exception (SomeException, try)
import System.Directory (
    XdgDirectory (XdgData),
    doesFileExist,
    getXdgDirectory,
 )
import System.Environment (lookupEnv)
import System.FilePath ((</>))

{- | The paths 'resolveDataFile' consults, in order: an operator override
(the whole ladder: a missing file there is reported, not skipped), the repo
data dir (@SABELA_DATA_DIR@, default @data@), the XDG mirror if a home exists.
-}
dataFilePaths :: String -> FilePath -> IO [FilePath]
dataFilePaths envVar file = do
    mEnv <- lookupEnv envVar
    case mEnv of
        Just p | not (null p) -> pure [p]
        _ -> do
            dataDir <- maybe "data" id <$> lookupEnv "SABELA_DATA_DIR"
            mXdg <-
                try (getXdgDirectory XdgData ("sabela" </> file)) ::
                    IO (Either SomeException FilePath)
            pure ((dataDir </> file) : either (const []) pure mXdg)

{- | The first existing path on the ladder, or every path checked, so a miss
can state where it looked instead of guessing at a remedy.
-}
resolveDataFile :: String -> FilePath -> IO (Either [FilePath] FilePath)
resolveDataFile envVar file = do
    paths <- dataFilePaths envVar file
    found <- firstExisting paths
    pure (maybe (Left paths) Right found)
  where
    firstExisting [] = pure Nothing
    firstExisting (p : ps) = do
        ok <- doesFileExist p
        if ok then pure (Just p) else firstExisting ps
