{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Sabela.Session.Project (
    ReplSupport (..),
    buildTimeSupportDir,
    setupReplProject,
    writeFileIfChanged,
) where

import Control.Monad (unless)
import Data.FileEmbed (makeRelativeToProject)
import qualified Data.Text as T
import ScriptHs.Parser (CabalMeta (..))
import ScriptHs.Run (renderCabalFile, renderCabalProject)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import System.IO (readFile')

buildTimeSupportDir :: FilePath
buildTimeSupportDir = $(makeRelativeToProject "sabela-notebook" >>= \p -> [|p|])

supportPackageName :: String
supportPackageName = "sabela-notebook"

data ReplSupport = WithNotebookSupport | BareRepl
    deriving (Eq, Show)

setupReplProject :: ReplSupport -> [FilePath] -> FilePath -> CabalMeta -> IO ()
setupReplProject support localPkgs dir meta = do
    createDirectoryIfMissing True dir
    _ <-
        writeFileIfChanged
            (dir </> "cabal.project")
            ( T.unpack
                ( renderCabalProject
                    localPkgs
                    (metaSourceRepos meta)
                    (metaExtraLibDirs meta)
                    (metaExtraIncludeDirs meta)
                )
                <> haddockStanza
            )
    ensureFile (dir </> "Main.hs") "main :: IO ()\nmain = pure ()\n"
    let extraDeps = case support of
            WithNotebookSupport -> [T.pack supportPackageName]
            BareRepl -> []
    _ <-
        writeFileIfChanged
            (dir </> "sabela-repl.cabal")
            (renderCabalFile "sabela-repl" extraDeps meta)
    pure ()

haddockStanza :: String
haddockStanza = "\npackage *\n  ghc-options: -haddock\n"

ensureFile :: FilePath -> String -> IO ()
ensureFile path content = do
    exists <- doesFileExist path
    unless exists $ writeFile path content

writeFileIfChanged :: FilePath -> String -> IO Bool
writeFileIfChanged path content = do
    exists <- doesFileExist path
    same <- if exists then (== content) <$> readFile' path else pure False
    if same then pure False else True <$ writeFile path content
