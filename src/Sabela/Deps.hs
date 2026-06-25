{-# LANGUAGE OverloadedStrings #-}

module Sabela.Deps (
    collectMetadata,
    collectMetadataFromContent,
    mergedMeta,
    repairDeps,
    sabelaDefaultExts,
    ProjectSig (..),
    emptyProjectSig,
    projectSig,
    depsMatch,
) where

import Data.Char (isAlpha, isAlphaNum)
import Data.List (sort, sortOn)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
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
     in repairMeta
            (mergeMetas [(scriptMeta . parseScript) (cellSource c) | c <- allCode])

collectMetadataFromContent :: Text -> CabalMeta
collectMetadataFromContent content =
    let segs = parseMarkdown content
        codeSrcs = [src | CodeBlock _ src _ <- segs]
     in repairMeta (mergeMetas (map (scriptMeta . parseScript) codeSrcs))

{- | Normalise build-depends as metadata is collected, so the live session, the
staleness check, and the exporters all see comma-separated dependencies.
-}
repairMeta :: CabalMeta -> CabalMeta
repairMeta m = m{metaDeps = repairDeps (metaDeps m)}

{- | Repair build-depends a model wrote space-separated rather than comma-
separated (@text granite@ becomes @text@, @granite@), without breaking a version
constraint, whose spaces are not boundaries (@dataframe == 2.3.0.0@,
@text < 4 && < 5@). A space-mangled line otherwise reaches cabal as one bogus
package and hangs the install.

The repair is Cabal's dependency grammar. scripths has already split on commas,
so the only boundary left inside an entry is the start of a new package name:

>  build-depends      ::= dependency (',' dependency)*
>  dependency         ::= package-name [version-constraint]
>  package-name       ::= alphanumeric and hyphens, with at least one letter
>  version-constraint ::= (operator | version)*    -- carries no letter

A token whose leading package-name prefix carries a letter STARTS a new
dependency; every other token (a version literal, or an operator possibly fused
onto a name as in @dataframe==2.3.0.0@) attaches to the dependency in progress.
-}
repairDeps :: [Text] -> [Text]
repairDeps = concatMap (map T.unwords . splitDeps . T.words)

-- | Re-split an entry's tokens into dependencies at each package-name boundary.
splitDeps :: [Text] -> [[Text]]
splitDeps = reverse . map reverse . foldl step []
  where
    step acc tok
        | startsDependency tok = [tok] : acc
        | (d : ds) <- acc = (tok : d) : ds
        | otherwise = [[tok]]

{- | Does a token begin a new dependency? Its leading package-name prefix (the
maximal run of alphanumerics and hyphens) must carry a letter, which is exactly
Cabal's package-name rule and disambiguates a name from a version literal
(@2.3.0.0@) or an operator (@==@, @&&@). The prefix test also catches a fused
constraint: @dataframe==2.3.0.0@ begins with the name @dataframe@.
-}
startsDependency :: Text -> Bool
startsDependency = T.any isAlpha . T.takeWhile (\c -> isAlphaNum c || c == '-')

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
