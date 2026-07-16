{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- | The throwaway @cabal repl@ project scaffold, shared by the notebook session
('Sabela.Handlers.Lifecycle') and the isolated scratchpad
('Sabela.AI.Capabilities.Scratchpad') so both materialise a buildable project the
same way. A leaf module (no @Handlers@/@AI@ dependencies), which is what lets the
scratchpad scaffold its own project without depending on a prior notebook run.
-}
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

{- | Absolute path to the @sabela-notebook@ source as it sat at BUILD time. For a
dev build from the repo this is @<repo>\/sabela-notebook@ and persists on disk, so
an installed binary resolves the support package from any working directory; for a
Hackage build it points at the unpacked sdist dir, which is gone at run time, so
resolution falls back to Hackage. Used as a fallback when @SABELA_LOCAL_PACKAGES@
is unset (see @app\/Main.hs@).
-}
buildTimeSupportDir :: FilePath
buildTimeSupportDir = $(makeRelativeToProject "sabela-notebook" >>= \p -> [|p|])

{- | The notebook support library. A real notebook\/scratchpad session depends on
it so a cell can @import Sabela.Notebook.*@; it resolves from Hackage, or from a
local @packages:@ entry supplied via @SABELA_LOCAL_PACKAGES@.
-}
supportPackageName :: String
supportPackageName = "sabela-notebook"

{- | Whether a repl project gets the notebook support library on its
@build-depends@. Real sessions use 'WithNotebookSupport'; a bare session that
only exercises the repl machinery (no @import Sabela.Notebook.*@) uses 'BareRepl'
so it neither requires nor builds the support package.
-}
data ReplSupport = WithNotebookSupport | BareRepl
    deriving (Eq, Show)

{- | Write the throwaway repl project into @dir@. Local packages resolve through
the @packages:@ stanza in the generated @cabal.project@; a package only joins
@build-depends@ (so its modules are in scope) when its dir is in @localPkgs@. The
'supportPackageName' dependency is added for 'WithNotebookSupport' so a cell can
@import Sabela.Notebook.*@ regardless of how that package resolves.
-}
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

{- | Build every package in the repl project with @-haddock@, so the interactive
session can answer @:doc@ (and thus @describe_function@) for the support library
and the notebook's declared dependencies, not just show bare type signatures.
-}
haddockStanza :: String
haddockStanza = "\npackage *\n  ghc-options: -haddock\n"

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
