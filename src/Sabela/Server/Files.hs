{-# LANGUAGE OverloadedStrings #-}

{- | File-explorer handlers for the @/api/files@ + @/api/file@ subtree:
list, read, create (file or directory), write, delete, rename. Every
path is canonicalized and gated by 'isWithinPath' / 'isPrefixOfPath' so
nothing escapes @envWorkDir@.
-}
module Sabela.Server.Files (
    -- * Handlers
    listFilesH,
    readFileH,
    createFileH,
    writeFileH,
    deleteFileH,
    renameFileH,

    -- * Path-safety helpers (exposed for other server modules)
    isWithinPath,
    isPrefixOfPath,
) where

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.Char (toLower)
import Data.List (isPrefixOf, sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Servant (Handler, NoContent (..))
import System.Directory (
    canonicalizePath,
    createDirectoryIfMissing,
    doesDirectoryExist,
    listDirectory,
    removeDirectoryRecursive,
    removeFile,
    renamePath,
 )
import System.FilePath (
    makeRelative,
    normalise,
    splitDirectories,
    takeDirectory,
    (</>),
 )

import Sabela.Api
import Sabela.State (App (..))
import Sabela.State.Environment (Environment (..))

normForCmp :: FilePath -> FilePath
normForCmp = map toLower . normalise

isWithinPath :: FilePath -> FilePath -> Bool
isWithinPath parent child =
    let p = splitDirectories (normForCmp parent)
        c = splitDirectories (normForCmp child)
     in p == take (length p) c

listFilesH :: App -> Maybe Text -> Handler [FileEntry]
listFilesH app mPath = liftIO $ do
    let workDir = envWorkDir (appEnv app)
        requested = workDir </> maybe "." T.unpack mPath
    rootCanon <- canonicalizePath workDir
    pathCanon <- canonicalizePath requested
    if not (isWithinPath rootCanon pathCanon)
        then pure []
        else do
            entries <- listDirectory pathCanon
            fes <- forM (sort entries) (toFileEntry rootCanon pathCanon)
            pure (sortDirsFirst fes)

toFileEntry :: FilePath -> FilePath -> String -> IO FileEntry
toFileEntry rootCanon dirCanon name = do
    let full = dirCanon </> name
    isDir <- doesDirectoryExist full
    pure
        FileEntry
            { feName = T.pack name
            , fePath = T.pack (makeRelative rootCanon full)
            , feIsDir = isDir
            }

sortDirsFirst :: [FileEntry] -> [FileEntry]
sortDirsFirst fes =
    let (dirs, files) =
            foldr
                (\e (ds, fs) -> if feIsDir e then (e : ds, fs) else (ds, e : fs))
                ([], [])
                fes
     in dirs ++ files

readFileH :: App -> Maybe Text -> Handler Text
readFileH app mPath = liftIO $ do
    let workDir = envWorkDir (appEnv app)
        relPath = maybe "" T.unpack mPath
        absPath = workDir </> relPath
    canon <- canonicalizePath absPath
    if not (workDir `isPrefixOfPath` canon)
        then pure "(access denied)"
        else TIO.readFile canon

createFileH :: App -> CreateFileRequest -> Handler FileEntry
createFileH app req = liftIO $ do
    let (relPath, isDir) = case req of
            CreateDir p -> (p, True)
            CreateFile p _ -> (p, False)
        workDir = envWorkDir (appEnv app)
        absPath = workDir </> T.unpack relPath
    canon <- canonicalizePath (takeDirectory absPath)
    if not (workDir `isPrefixOfPath` canon)
        then pure (FileEntry relPath relPath False)
        else do
            createFileOrDir absPath req
            pure (mkFileEntry relPath isDir)

createFileOrDir :: FilePath -> CreateFileRequest -> IO ()
createFileOrDir absPath (CreateDir _) = do
    createDirectoryIfMissing True absPath
    putStrLn $ "[sabela] Created: " ++ absPath
createFileOrDir absPath (CreateFile _ content) = do
    createDirectoryIfMissing True (takeDirectory absPath)
    TIO.writeFile absPath content
    putStrLn $ "[sabela] Created: " ++ absPath

mkFileEntry :: Text -> Bool -> FileEntry
mkFileEntry relPath isDir =
    FileEntry
        { feName = T.pack (last (splitPath' (T.unpack relPath)))
        , fePath = relPath
        , feIsDir = isDir
        }
  where
    splitPath' p = case break (== '/') p of
        (a, []) -> [a]
        (a, _ : bs) -> a : splitPath' bs

writeFileH :: App -> WriteFileRequest -> Handler Text
writeFileH app (WriteFileRequest relPath content) = liftIO $ do
    let workDir = envWorkDir (appEnv app)
        absPath = workDir </> T.unpack relPath
    canon <- canonicalizePath (takeDirectory absPath)
    if not (workDir `isPrefixOfPath` canon)
        then pure "access denied"
        else do TIO.writeFile absPath content; pure "ok"

deleteFileH :: App -> DeleteFileRequest -> Handler NoContent
deleteFileH app (DeleteFileRequest relPath) = liftIO $ do
    let workDir = envWorkDir (appEnv app)
        absPath = workDir </> T.unpack relPath
    canon <- canonicalizePath absPath
    if not (workDir `isPrefixOfPath` canon) || canon == workDir
        then pure NoContent
        else do
            isDir <- doesDirectoryExist canon
            if isDir
                then removeDirectoryRecursive canon
                else removeFile canon
            pure NoContent

renameFileH :: App -> RenameFileRequest -> Handler NoContent
renameFileH app (RenameFileRequest oldRelPath newRelPath) = liftIO $ do
    let workDir = envWorkDir (appEnv app)
        oldAbs = workDir </> T.unpack oldRelPath
        newAbs = workDir </> T.unpack newRelPath
    oldCanon <- canonicalizePath oldAbs
    let newParent = takeDirectory newAbs
    newParentCanon <- canonicalizePath newParent
    if not (workDir `isPrefixOfPath` oldCanon)
        || not (workDir `isPrefixOfPath` newParentCanon)
        then pure NoContent
        else do renamePath oldAbs newAbs; pure NoContent

{- | Check whether @path@ is within @prefix@ using path component comparison.
  This avoids false positives like @/home/alice@ matching @/home/alice-secret@.
-}
isPrefixOfPath :: FilePath -> FilePath -> Bool
isPrefixOfPath prefix path =
    let prefixParts = splitDirectories (normalise prefix)
        pathParts = splitDirectories (normalise path)
     in prefixParts `isPrefixOf` pathParts
