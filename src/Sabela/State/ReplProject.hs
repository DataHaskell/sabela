{- | A per-work-dir kernel build tree under @$XDG_CACHE_HOME/sabela@, so its
compiled packages and cell objects survive a server restart. A file lock
keeps concurrent servers on one work dir from sharing it.
-}
module Sabela.State.ReplProject (
    claimReplProjectDir,
    claimIn,
    replProjectName,
) where

import Control.Exception (SomeException, bracketOnError, try)
import Control.Monad (forM_)
import Data.Hashable (hashWithSalt)
import GHC.IO.Handle.Lock (LockMode (ExclusiveLock), hTryLock)
import Numeric (showHex)
import System.Directory (
    XdgDirectory (XdgCache),
    createDirectoryIfMissing,
    doesDirectoryExist,
    getXdgDirectory,
    listDirectory,
    removePathForcibly,
 )
import System.FilePath (takeFileName, (<.>), (</>))
import System.IO (Handle, IOMode (ReadWriteMode), hClose, openFile)

{- | The project dir for a canonical work dir, plus the lock handle to hold for
the server's life. Failing to claim the cache yields the temp dir, no handle.
-}
claimReplProjectDir :: FilePath -> FilePath -> IO (FilePath, Maybe Handle)
claimReplProjectDir tmpDir absWork = do
    root <-
        try (getXdgDirectory XdgCache ("sabela" </> "repl-projects")) ::
            IO (Either SomeException FilePath)
    either (const (pure fallback)) (claimIn fallback absWork) root
  where
    fallback = (tmpDir </> "repl-project", Nothing)

-- | 'claimReplProjectDir' against an explicit cache root.
claimIn ::
    (FilePath, Maybe Handle) ->
    FilePath ->
    FilePath ->
    IO (FilePath, Maybe Handle)
claimIn fallback absWork root = do
    claimed <- try acquire :: IO (Either SomeException (Maybe Handle))
    pure $ case claimed of
        Right (Just h) -> (root </> name, Just h)
        _ -> fallback
  where
    name = replProjectName absWork
    acquire = do
        createDirectoryIfMissing True root
        bracketOnError
            (openFile (root </> name <.> "lock") ReadWriteMode)
            hClose
            $ \h -> do
                locked <- hTryLock h ExclusiveLock
                if locked
                    then Just h <$ sweepStaleSources (root </> name)
                    else Nothing <$ hClose h

{- | Drop compiled-cell module sources a previous server left, so an import can
never resolve to a deleted cell. Build caches and project files are kept.
-}
sweepStaleSources :: FilePath -> IO ()
sweepStaleSources dir = do
    present <- doesDirectoryExist dir
    entries <- if present then listDirectory dir else pure []
    forM_ (filter (`notElem` keptEntries) entries) $ \entry ->
        removePathForcibly (dir </> entry)

keptEntries :: [FilePath]
keptEntries =
    [ "dist-newstyle"
    , "ghci-objs"
    , "cabal.project"
    , "sabela-repl.cabal"
    , "Main.hs"
    , ".sabela-build-dirty"
    ]

-- | The work dir's basename, then a hash of its full path against collisions.
replProjectName :: FilePath -> String
replProjectName absWork = base <> "-" <> hex 1 <> hex 2
  where
    base = case filter safe (takeFileName absWork) of
        "" -> "root"
        b -> b
    safe c = c `notElem` ("/\\:" :: String)
    hex salt = showHex (fromIntegral (hashWithSalt salt absWork) :: Word) ""
