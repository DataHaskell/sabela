{-# LANGUAGE OverloadedStrings #-}

module Sabela.Deps (
    availablePackages,
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

availablePackages :: Notebook -> S.Set Text -> S.Set Text
availablePackages nb = S.union (S.fromList (metaDeps (collectMetadata nb)))

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

repairMeta :: CabalMeta -> CabalMeta
repairMeta m = m{metaDeps = repairDeps (metaDeps m)}

repairDeps :: [Text] -> [Text]
repairDeps = concatMap (map T.unwords . splitDeps . T.words)

splitDeps :: [Text] -> [[Text]]
splitDeps = reverse . map reverse . foldl step []
  where
    step acc tok
        | startsDependency tok = [tok] : acc
        | (d : ds) <- acc = (tok : d) : ds
        | otherwise = [[tok]]

startsDependency :: Text -> Bool
startsDependency = T.any isAlpha . T.takeWhile (\c -> isAlphaNum c || c == '-')

sabelaDefaultExts :: [Text]
sabelaDefaultExts =
    [ "TemplateHaskell"
    , "GADTs"
    , "DataKinds"
    , "OverloadedStrings"
    , "TypeApplications"
    , "ScopedTypeVariables"
    ]

mergedMeta :: Set Text -> CabalMeta -> CabalMeta
mergedMeta globalDeps meta =
    meta
        { metaDeps = S.toList (S.fromList (metaDeps meta) <> globalDeps)
        , metaExts = S.toList (S.fromList (metaExts meta) <> S.fromList sabelaDefaultExts)
        }

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
