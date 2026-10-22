{-# LANGUAGE OverloadedStrings #-}

{- | @POST /api/import-path@: bring a file the browser could only name, not
read, into the workspace. On ChromeOS a drag from the Files app hands a web
page the file's path while refusing it the bytes; the server, which runs in
the Linux container, can read that path once it is mapped from the host's
view of the share to the container's. A file already in the workspace is
used where it is; any other is copied into the data folder.
-}
module Sabela.Server.ImportPath (
    ImportPathRequest (..),
    ImportPathResult (..),
    importPathH,
    containerPath,
) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T
import Servant (Handler)
import System.Directory (
    canonicalizePath,
    copyFile,
    createDirectoryIfMissing,
    doesFileExist,
    getHomeDirectory,
    makeRelativeToCurrentDirectory,
 )
import System.FilePath (makeRelative, splitDirectories, takeFileName, (</>))

import Sabela.Server.Files (isWithinPath)
import Sabela.State (App (..))
import Sabela.State.Environment (Environment (..))

newtype ImportPathRequest = ImportPathRequest {ipPath :: Text}

instance FromJSON ImportPathRequest where
    parseJSON = withObject "ImportPathRequest" $ \o -> ImportPathRequest <$> o .: "ipPath"

data ImportPathResult = ImportPathResult
    { iprPath :: Maybe Text
    -- ^ The workspace-relative path to read, when the file could be brought in.
    , iprError :: Maybe Text
    }

instance ToJSON ImportPathResult where
    toJSON r = object ["iprPath" .= iprPath r, "iprError" .= iprError r]

importPathH :: App -> ImportPathRequest -> Handler ImportPathResult
importPathH app (ImportPathRequest raw) = liftIO $ do
    home <- getHomeDirectory
    let workDir = envWorkDir (appEnv app)
        candidates = containerPath home (T.unpack (T.strip raw))
    found <- firstExisting candidates
    case found of
        Nothing ->
            pure
                ( ImportPathResult
                    Nothing
                    ( Just
                        ( "ChromeOS keeps "
                            <> raw
                            <> " on the host, where Linux cannot see it. In the Files app, "
                            <> "right-click the folder it is in and choose \"Share with Linux\", "
                            <> "or move the file into \"Linux files\", then drop it again."
                        )
                    )
                )
        Just path -> do
            rootCanon <- canonicalizePath workDir
            pathCanon <- canonicalizePath path
            if isWithinPath rootCanon pathCanon
                then
                    pure
                        (ImportPathResult (Just (T.pack (makeRelative rootCanon pathCanon))) Nothing)
                else do
                    let destDir = workDir </> "data"
                        dest = destDir </> takeFileName pathCanon
                    createDirectoryIfMissing True destDir
                    copyFile pathCanon dest
                    rel <- makeRelativeToCurrentDirectory dest
                    pure (ImportPathResult (Just (T.pack (makeRelative workDir rel))) Nothing)
  where
    firstExisting [] = pure Nothing
    firstExisting (p : ps) = do
        ok <- doesFileExist p
        if ok then pure (Just p) else firstExisting ps

{- | Where a path the host browser names may live inside the container: the
path itself; the Crostini share of the home directory
(@/media/fuse/crostini_…_termina_penguin/x@ is @~/x@); and ChromeOS folders
shared with Linux (@/home/chronos/u-…/MyFiles/x@ is @/mnt/chromeos/MyFiles/x@, Drive files
under @/mnt/chromeos/GoogleDrive@), which exist only once shared with Linux.
-}
containerPath :: FilePath -> FilePath -> [FilePath]
containerPath home p = p : crostini ++ shared
  where
    parts = splitDirectories p
    crostini = case parts of
        ("/" : "media" : "fuse" : share : rest)
            | "crostini_" `isPrefixOf` share -> [foldl (</>) home rest]
        _ -> []
    shared = case parts of
        ("/" : "home" : "chronos" : _ : "MyFiles" : rest) ->
            [foldl (</>) "/mnt/chromeos/MyFiles" rest]
        ("/" : "media" : "removable" : rest) -> [foldl (</>) "/mnt/chromeos/removable" rest]
        ("/" : "media" : "fuse" : drive : "root" : rest)
            | "drivefs-" `isPrefixOf` drive ->
                [foldl (</>) "/mnt/chromeos/GoogleDrive/MyDrive" rest]
        ("/" : "media" : "fuse" : drive : "team_drives" : rest)
            | "drivefs-" `isPrefixOf` drive ->
                [foldl (</>) "/mnt/chromeos/GoogleDrive/SharedDrives" rest]
        ("/" : "media" : "fuse" : drive : "Computers" : rest)
            | "drivefs-" `isPrefixOf` drive ->
                [foldl (</>) "/mnt/chromeos/GoogleDrive/Computers" rest]
        ("/" : "media" : "archive" : rest) -> [foldl (</>) "/mnt/chromeos/archive" rest]
        _ -> []
