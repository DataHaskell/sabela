{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Reading the work directory as a file tree. Listing shares
'workDirFiles' with the path gate, so what the model is shown and what a
refusal suggests are the same set of files.
-}
module Sabela.AI.Files (
    ReadError (..),
    FileEntry (..),
    listLocal,
    entryOf,
    readLocal,
    readMissCandidates,
    resolveInWorkDir,
    listCap,
) where

import Control.Exception (IOException, try)
import Data.List (isPrefixOf, sort)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (
    canonicalizePath,
    doesDirectoryExist,
    doesFileExist,
    getFileSize,
 )
import System.FilePath (normalise, splitDirectories, takeDirectory, (</>))

import Sabela.AI.Artefact (Artefact, sampleFile)
import Sabela.AI.PathResolve (
    PathLookup (..),
    lookupPath,
    workDirFiles,
 )
import Sabela.Server.Files (isWithinPath)

{- | Why a path yielded no artefact, told apart where it is found out, so a
refusal can state what the read met rather than assuming it met nothing.
-}
data ReadError = OutsideWorkDir | NotFound | IsDirectory | NotReadable
    deriving (Eq, Show)

{- | A listed file and the size it stats to, so one listing answers "is this
worth reading". 'Nothing' when the stat failed and no size was measured.
-}
data FileEntry = FileEntry
    { feName :: Text
    , feBytes :: Maybe Integer
    }
    deriving (Eq, Show)

listCap :: Int
listCap = 200

{- | Files under @prefix@, relative to the work directory. Returns the
entries shown and how many exist, which are not the same number.
-}
listLocal :: FilePath -> Text -> IO ([FileEntry], Int)
listLocal workDir prefix = do
    base <- if climbs wanted then pure "" else listRoot workDir wanted
    files <- map (base </>) <$> workDirFiles (workDir </> base)
    let under = sort [f | f <- files, wanted `isPrefixOf` f]
    entries <- mapM (entryOf workDir) (take listCap under)
    pure (entries, length under)
  where
    cleaned = normalise (T.unpack (T.dropWhile (== '/') (T.strip prefix)))
    wanted
        | cleaned `elem` [".", "./", ""] = ""
        | otherwise = cleaned

{- | A prefix that walks up out of the work directory. Walking from it would
read a tree the caller may not see, and no path the walk yields can start
with it, so it lists nothing.
-}
climbs :: FilePath -> Bool
climbs p = ".." `elem` splitDirectories p

{- | The deepest directory a prefix names. Walking from there answers about
the subtree asked about, rather than filtering a walk of everything else that
may end before it reaches it.
-}
listRoot :: FilePath -> FilePath -> IO FilePath
listRoot workDir = go
  where
    go p
        | p `elem` [".", "/", ""] = pure ""
        | otherwise = do
            isDir <- doesDirectoryExist (workDir </> p)
            if isDir then pure p else go (takeDirectory p)

entryOf :: FilePath -> FilePath -> IO FileEntry
entryOf workDir rel = do
    got <- try (getFileSize (workDir </> rel)) :: IO (Either IOException Integer)
    pure (FileEntry (T.pack rel) (either (const Nothing) Just got))

{- | The absolute path a work-dir-relative path names, or the reason it may
not be read. A path that is there but is not a file is not a miss.
-}
resolveInWorkDir :: FilePath -> Text -> IO (Either ReadError FilePath)
resolveInWorkDir workDir path = do
    rootCanon <- canonicalizePath workDir
    target <- canonicalizePath (workDir </> normalise (T.unpack path))
    if not (isWithinPath rootCanon target)
        then pure (Left OutsideWorkDir)
        else do
            file <- doesFileExist target
            dir <- doesDirectoryExist target
            pure $ case (file, dir) of
                (True, _) -> Right target
                (_, True) -> Left IsDirectory
                _ -> Left NotFound

{- | Describes a file under the work directory. Bytes that do not decode as
text are a view of the artefact; bytes that could not be opened at all are a
file that was not read, never a file that is not there.
-}
readLocal :: FilePath -> Text -> IO (Either ReadError Artefact)
readLocal workDir path = do
    resolved <- resolveInWorkDir workDir path
    case resolved of
        Left e -> pure (Left e)
        Right target -> maybe (Left NotReadable) Right <$> sampleFile target

{- | The paths a missed read most likely meant, ranked the way a path gate
refusal ranks them, and how many paths were searched, so a miss that offers
none can say what it looked through instead of claiming there is nothing.
-}
readMissCandidates :: FilePath -> Text -> IO ([Text], Int)
readMissCandidates workDir path = do
    files <- workDirFiles workDir
    let searchable = files <> ancestorDirs files
    pure (map T.pack (nearest searchable), length searchable)
  where
    nearest searchable = case lookupPath (T.unpack path) searchable of
        Unique one -> [one]
        Nearby cs -> cs
        NoneNearby -> []

{- | The directories the walk passed through, derived from the files it found
so no second walk is needed. A path a caller can name is a place as often as
it is a file, and a search that skips them cannot answer with the right one.
-}
ancestorDirs :: [FilePath] -> [FilePath]
ancestorDirs = Set.toList . Set.fromList . concatMap places
  where
    places = takeWhile named . drop 1 . iterate takeDirectory
    named d = d `notElem` [".", "/", ""]
