{- | Recovering a build workspace a killed kernel left inconsistent.

The kernel is spawned as @cabal repl@, so a dependency install /is/ the kernel
spawn: killing it — by interrupt, by the build budget, or by the cell-execution
watchdog — can stop GHC midway through writing a @.hi@ or @.o@, or leave a
half-cloned @source-repository-package@ checkout. Nothing rewrites those, so
every later build in that server's life fails on them. The tree lives in a
per-server temp dir, which is why restarting the server has always cured it.

A marker records that a kernel died without closing gracefully; the next build
reads it and wipes the artefact trees before spawning. Deliberately scoped to
this project's own trees: the shared cabal store and package cache are not ours
to clean, and a truncated download there needs @cabal update@, not deletion.
-}
module Sabela.Session.Workspace (
    markBuildDirty,
    clearBuildDirty,
    buildIsDirty,
    wipeBuildArtifacts,
    buildArtifactDirs,
) where

import Control.Exception (SomeException, try)
import Control.Monad (forM_, void, when)
import System.Directory (
    doesDirectoryExist,
    doesFileExist,
    removeDirectoryRecursive,
    removeFile,
 )
import System.FilePath ((</>))

{- | Lives beside the artefact trees rather than inside one, so wiping them
cannot destroy the record of why they were wiped.
-}
buildDirtyMarker :: FilePath -> FilePath
buildDirtyMarker projDir = projDir </> ".sabela-build-dirty"

{- | The trees a killed build can leave inconsistent. Not the shared store: a
corrupt download there is reported, never deleted.
-}
buildArtifactDirs :: FilePath -> [FilePath]
buildArtifactDirs projDir =
    [projDir </> "dist-newstyle", projDir </> "ghci-objs"]

-- | Record that a kernel is running and has not yet closed gracefully.
markBuildDirty :: FilePath -> IO ()
markBuildDirty projDir =
    quiet (writeFile (buildDirtyMarker projDir) "")

{- | Record that a kernel closed gracefully, so its artefacts are trustworthy.
Only the graceful path may call this: a forced kill is exactly what the marker
exists to remember.
-}
clearBuildDirty :: FilePath -> IO ()
clearBuildDirty projDir = do
    present <- doesFileExist (buildDirtyMarker projDir)
    when present (quiet (removeFile (buildDirtyMarker projDir)))

-- | Did the last kernel to use this workspace die without closing gracefully?
buildIsDirty :: FilePath -> IO Bool
buildIsDirty = doesFileExist . buildDirtyMarker

{- | Discard the artefact trees. Costs one rebuild of the local support package,
because dependencies themselves live in the shared store.
-}
wipeBuildArtifacts :: FilePath -> IO ()
wipeBuildArtifacts projDir =
    forM_ (buildArtifactDirs projDir) $ \dir -> do
        present <- doesDirectoryExist dir
        when present (quiet (removeDirectoryRecursive dir))

quiet :: IO () -> IO ()
quiet act = void (try act :: IO (Either SomeException ()))
