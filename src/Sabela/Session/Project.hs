{-# LANGUAGE OverloadedStrings #-}

{- | The throwaway @cabal repl@ project scaffold, shared by the notebook session
('Sabela.Handlers.Lifecycle') and the isolated scratchpad
('Sabela.AI.Capabilities.Scratchpad') so both materialise a buildable project the
same way. A leaf module (no @Handlers@/@AI@ dependencies), which is what lets the
scratchpad scaffold its own project without depending on a prior notebook run.
-}
module Sabela.Session.Project (
    setupReplProject,
    writeFileIfChanged,
) where

import Control.Monad (unless)
import qualified Data.Text as T
import ScriptHs.Parser (CabalMeta (..))
import ScriptHs.Run (renderCabalFile, renderCabalProject)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeFileName, (</>))
import System.IO (readFile')

import Sabela.Notebook.Support (supportPackageName)

{- | Write the throwaway repl project into @dir@. Local packages resolve through
the @packages:@ stanza in the generated @cabal.project@; a package only joins
@build-depends@ (so its modules are in scope) when its dir is in @localPkgs@. The
sticky 'supportPackageName' is added exactly when its materialised dir is present,
so a cell can @import Sabela.Notebook.*@.
-}
setupReplProject :: [FilePath] -> FilePath -> CabalMeta -> IO ()
setupReplProject localPkgs dir meta = do
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
            )
    ensureFile (dir </> "Main.hs") "main :: IO ()\nmain = pure ()\n"
    let extraDeps =
            [ T.pack supportPackageName
            | any ((== supportPackageName) . takeFileName) localPkgs
            ]
    _ <-
        writeFileIfChanged
            (dir </> "sabela-repl.cabal")
            (renderCabalFile "sabela-repl" extraDeps meta)
    pure ()

ensureFile :: FilePath -> String -> IO ()
ensureFile path content = do
    exists <- doesFileExist path
    unless exists $ writeFile path content

{- | Write @content@ to @path@ only when it differs from what is already there, so
an unchanged @cabal.project@\/@.cabal@ keeps its mtime and @cabal repl@ skips the
dependency re-solve. Returns 'True' iff it actually wrote.
-}
writeFileIfChanged :: FilePath -> String -> IO Bool
writeFileIfChanged path content = do
    exists <- doesFileExist path
    same <- if exists then (== content) <$> readFile' path else pure False
    if same then pure False else True <$ writeFile path content
