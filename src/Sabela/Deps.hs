{-# LANGUAGE OverloadedStrings #-}

module Sabela.Deps (
    availablePackages,
    blameCells,
    collectMetadata,
    collectMetadataFromContent,
    depDeclaringCells,
    mergedMeta,
    repairDeps,
    sabelaDefaultExts,
    ProjectSig (..),
    emptyProjectSig,
    projectSig,
    EnvSig (..),
    envSig,
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

{- | Each cell paired with the dependencies it alone declares. This is what
'collectMetadata' folds over, kept unmerged so a failure can name its cell.
-}
depDeclaringCells :: Notebook -> [(Int, [Text])]
depDeclaringCells nb =
    [ (cellId c, deps)
    | c <- nbCells nb
    , cellType c == CodeCell && cellLang c == ST.Haskell
    , let deps = sort (repairDeps (metaDeps (scriptMeta (parseScript (cellSource c)))))
    , not (null deps)
    ]

{- | The cells a build failure belongs to: those declaring a package the error
names, or every declaring cell when it names none we recognise.
-}
blameCells :: Notebook -> Text -> [Int]
blameCells nb err
    | null named = map fst declaring
    | otherwise = named
  where
    declaring = depDeclaringCells nb
    tokens = S.fromList (T.split (not . isPackageChar) (T.toLower err))
    named =
        [cid | (cid, deps) <- declaring, any ((`S.member` tokens) . T.toLower) deps]

isPackageChar :: Char -> Bool
isPackageChar ch = isAlphaNum ch || ch == '-'

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

{- | Everything about a notebook that decides which kernel can serve it. Recorded
against the running kernel at spawn, so "has the environment changed?" is a
comparison against a live process rather than a flag someone must remember to set.
-}
data EnvSig = EnvSig
    { esDeps :: Set Text
    , esExts :: Set Text
    , esProject :: ProjectSig
    }
    deriving (Eq, Show)

{- | Globally-provided deps are excluded: no kernel installs them, so they are
never recorded. Equality (not the old subset test) is what lets a *removed*
dependency invalidate the kernel.
-}
envSig :: Set Text -> [FilePath] -> CabalMeta -> EnvSig
envSig globalDeps localPkgs meta =
    EnvSig
        { esDeps = S.fromList (metaDeps meta) `S.difference` globalDeps
        , esExts = S.fromList (metaExts meta)
        , esProject = projectSig localPkgs (mergedMeta globalDeps meta)
        }
