{-# LANGUAGE TemplateHaskell #-}

module Sabela.Session.SupportSource (
    embeddedSupportFiles,
    materializeSupportSource,
) where

import qualified Data.ByteString as BS
import Data.FileEmbed (bsToExp, getDir, makeRelativeToProject)
import Language.Haskell.TH (Exp (ListE), runIO)
import System.Directory (
    XdgDirectory (XdgData),
    createDirectoryIfMissing,
    doesFileExist,
    getXdgDirectory,
 )
import System.FilePath (splitDirectories, takeDirectory, (</>))

{- | The @sabela-notebook@ support library's sources, embedded at build time so
an installed binary can supply them from any working directory. Keyed by path
relative to @sabela-notebook/@; build outputs are excluded.
-}
embeddedSupportFiles :: [(FilePath, BS.ByteString)]
embeddedSupportFiles =
    $( do
        dir <- makeRelativeToProject "sabela-notebook"
        files <- runIO (getDir dir)
        let keep (p, _) = "dist-newstyle" `notElem` splitDirectories p
        ListE
            <$> mapM
                (\(p, b) -> bsToExp b >>= \e -> [|(p, $(pure e))|])
                (filter keep files)
     )

{- | Unpack 'embeddedSupportFiles' into @$XDG_DATA_HOME/sabela/sabela-notebook@
and return that directory. Files whose bytes already match are left alone, so
repeat starts do not invalidate the cabal build cache there.
-}
materializeSupportSource :: IO FilePath
materializeSupportSource = do
    dir <- getXdgDirectory XdgData ("sabela" </> "sabela-notebook")
    mapM_ (writeUnder dir) embeddedSupportFiles
    pure dir

writeUnder :: FilePath -> (FilePath, BS.ByteString) -> IO ()
writeUnder dir (rel, bytes) = do
    let path = dir </> rel
    createDirectoryIfMissing True (takeDirectory path)
    exists <- doesFileExist path
    same <- if exists then (== bytes) <$> BS.readFile path else pure False
    if same then pure () else BS.writeFile path bytes
