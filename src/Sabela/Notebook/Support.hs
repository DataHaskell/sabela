{-# LANGUAGE TemplateHaskell #-}

{- | The @sabela-notebook@ support package — the "sticky" FRP\/animation\/widget
modules that ship with every notebook — embedded into the Sabela binary at
compile time and materialised next to a notebook on session start.

The package source lives under @static\/notebook\/sabela-notebook@ and is the
single source of truth: it is a real, compilable, tested Cabal package (so it is
type-checked and unit-tested as ordinary Haskell), and 'materializeSupport'
writes it verbatim into @<workDir>\/.sabela\/sabela-notebook@ so notebook cells
can @import Sabela.Notebook.Frp@. Files are only rewritten when their bytes
change, so the object cache (and thus cold-start time) stays warm across sessions.

This deliberately mirrors how @scatter.js@ is embedded
('Sabela.Output.Scatter'). The north star is a published, versioned
@sabela-notebook@ package; until then this on-disk copy is the canonical one.
-}
module Sabela.Notebook.Support
    ( supportPackageName
    , supportPackageDir
    , materializeSupport
    ) where

import Control.Monad (forM_, when)
import qualified Data.ByteString as BS
import Data.FileEmbed (embedDir, makeRelativeToProject)
import System.Directory
    ( createDirectoryIfMissing
    , doesFileExist
    )
import Data.List (isSuffixOf)
import System.FilePath (takeDirectory, (</>))

{- | The embedded support-package tree, as @(relativePath, contents)@ pairs taken
verbatim from @static\/notebook\/sabela-notebook@ at build time.
-}
supportFiles :: [(FilePath, BS.ByteString)]
supportFiles =
    $(makeRelativeToProject "static/notebook/sabela-notebook" >>= embedDir)

-- | The Cabal package name notebook cells depend on to get the sticky modules.
supportPackageName :: String
supportPackageName = "sabela-notebook"

{- | Where the support package is materialised, given a notebook working
directory: @<workDir>\/.sabela\/sabela-notebook@. This path is added to the
repl-project's @packages:@ stanza.
-}
supportPackageDir :: FilePath -> FilePath
supportPackageDir workDir = workDir </> ".sabela" </> supportPackageName

{- | Write the embedded support package into 'supportPackageDir'. Idempotent:
a file is (re)written only when its contents differ from what is already on disk,
so unchanged sessions don't invalidate the build cache. Returns the package dir.
-}
materializeSupport :: FilePath -> IO FilePath
materializeSupport workDir = do
    let base = supportPackageDir workDir
    forM_ supportFiles $ \(rel, bytes) -> do
        let dest = base </> realName rel
        createDirectoryIfMissing True (takeDirectory dest)
        writeIfChanged dest bytes
    pure base
  where
    -- The package's @.cabal@ is stored as @.cabal.tmpl@ in the Sabela source
    -- tree so the dev build doesn't see it as a second Cabal package; restore
    -- the real name on disk.
    realName p = maybe p (<> ".cabal") (stripSuffix ".cabal.tmpl" p)
    stripSuffix suf s
        | suf `isSuffixOf` s = Just (take (length s - length suf) s)
        | otherwise = Nothing

-- | Write @bytes@ to @path@ only if the file is absent or has different contents.
writeIfChanged :: FilePath -> BS.ByteString -> IO ()
writeIfChanged path bytes = do
    exists <- doesFileExist path
    stale <-
        if exists
            then (/= bytes) <$> BS.readFile path
            else pure True
    when stale $ BS.writeFile path bytes
