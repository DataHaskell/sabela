{-# LANGUAGE OverloadedStrings #-}

module Sabela.Deps (
    collectMetadata,
    collectMetadataFromContent,
    mergedMeta,
    sabelaDefaultExts,
    ProjectSig (..),
    emptyProjectSig,
    projectSig,
    depsMatch,
) where

import Data.List (sort, sortOn)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Sabela.Model (Cell (..), CellType (..), Notebook (..))
import qualified Sabela.SessionTypes as ST
import ScriptHs.Markdown (Segment (..), parseMarkdown)
import ScriptHs.Parser (
    CabalMeta (..),
    ScriptFile (..),
    SourceRepoPin (..),
    mergeMetas,
    parseScript,
 )

collectMetadata :: Notebook -> CabalMeta
collectMetadata nb =
    let allCode =
            filter (\c -> cellType c == CodeCell && cellLang c == ST.Haskell) (nbCells nb)
     in mergeMetas [(scriptMeta . parseScript) (cellSource c) | c <- allCode]

collectMetadataFromContent :: Text -> CabalMeta
collectMetadataFromContent content =
    let segs = parseMarkdown content
        codeSrcs = [src | CodeBlock _ src _ <- segs]
     in mergeMetas (map (scriptMeta . parseScript) codeSrcs)

{- | Language extensions enabled by default in every notebook, on top of
whatever a cell declares via @-- cabal: default-extensions:@. Injected by
'mergedMeta' so the live GHCi session and both export paths agree.
-}
sabelaDefaultExts :: [Text]
sabelaDefaultExts =
    [ "TemplateHaskell"
    , "GADTs"
    , "DataKinds"
    , "OverloadedStrings"
    , "TypeApplications"
    , "ScopedTypeVariables"
    ]

{- | Fold environment-global deps and the Sabela default extensions into a
notebook's collected metadata. The single chokepoint shared by the live session
('Sabela.Handlers.Lifecycle') and the standalone / reactive exporters, so the
defaults apply uniformly. Extensions are deduped, preserving cell-declared ones.
-}
mergedMeta :: Set Text -> CabalMeta -> CabalMeta
mergedMeta globalDeps meta =
    meta
        { metaDeps = S.toList (S.fromList (metaDeps meta) <> globalDeps)
        , metaExts = S.toList (S.fromList (metaExts meta) <> S.fromList sabelaDefaultExts)
        }

{- | The rebuild-relevant repl-project inputs beyond dep names and extensions.
Compared by the session-staleness check so directive changes (local package
dirs, git pins, ghc-options) trigger a package-env rebuild.
-}
data ProjectSig = ProjectSig
    { psLocalPackages :: [FilePath]
    , psSourceRepos :: [SourceRepoPin]
    , psGhcOptions :: [Text]
    , psExtraLibDirs :: [Text]
    , psExtraIncludeDirs :: [Text]
    }
    deriving (Eq, Show)

emptyProjectSig :: ProjectSig
emptyProjectSig = ProjectSig [] [] [] [] []

{- | Canonical signature from resolved local package dirs + metadata: package
dirs and git pins compare order-insensitively; ghc-options keep their order.
-}
projectSig :: [FilePath] -> CabalMeta -> ProjectSig
projectSig localPkgs meta =
    ProjectSig
        { psLocalPackages = sort localPkgs
        , psSourceRepos = sortOn pinKey (metaSourceRepos meta)
        , psGhcOptions = metaGhcOptions meta
        , psExtraLibDirs = sort (metaExtraLibDirs meta)
        , psExtraIncludeDirs = sort (metaExtraIncludeDirs meta)
        }
  where
    pinKey p = (srpLocation p, srpRef p, srpSubdir p)

{- | Does an installed session state cover the notebook's metadata? Dep names
use subset semantics (a dropped dep keeps the session); extensions and the
project signature must match exactly.
-}
depsMatch ::
    CabalMeta ->
    Set Text ->
    Set Text ->
    Set Text ->
    ProjectSig ->
    ProjectSig ->
    Bool
depsMatch metas installed instExts globalDeps neededSig instSig =
    S.fromList (metaDeps metas) `S.isSubsetOf` (installed `S.union` globalDeps)
        && S.fromList (metaExts metas) == instExts
        && neededSig == instSig
