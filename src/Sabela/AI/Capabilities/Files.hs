{-# LANGUAGE OverloadedStrings #-}

{- | @list_files@ and @read_file@ over two backends: the work directory
when no @repo@ is given, a GitHub tree when one is.
-}
module Sabela.AI.Capabilities.Files (
    execListFiles,
    execReadFile,
    readErrorOutcome,
    entryJson,
    localEntryJson,
) where

import Data.Aeson (Value, object, toJSON, (.=))
import Data.Aeson.Types (Pair)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client (Manager)

import Sabela.AI.Artefact (Artefact (..), ArtefactView (..), artefactPairs)
import Sabela.AI.Capabilities.Util (fieldText)
import Sabela.AI.Files (
    FileEntry (..),
    ReadError (..),
    listCap,
    listLocal,
    readLocal,
    readMissCandidates,
 )
import Sabela.AI.GitHub (
    GhEntry (..),
    fetchBlob,
    fetchTree,
    repoSlug,
 )
import Sabela.AI.Types (ToolOutcome, errOutcome, okOutcome)
import Sabela.Api (errorJson, errorJsonWith)
import Sabela.State (App (..))
import Sabela.State.Environment (Environment (..))

execListFiles :: App -> Value -> IO ToolOutcome
execListFiles app input
    | T.null rawRepo = listWorkDir app (fieldText "path" input)
    | otherwise = withRepo app rawRepo (listRepo app mRef (fieldText "path" input))
  where
    rawRepo = fieldText "repo" input
    mRef = optional (fieldText "ref" input)

execReadFile :: App -> Value -> IO ToolOutcome
execReadFile app input
    | T.null path = pure (errOutcome (errorJson "path required"))
    | T.null rawRepo = readWorkDir app path
    | otherwise = withRepo app rawRepo (readRepo mRef path)
  where
    path = fieldText "path" input
    rawRepo = fieldText "repo" input
    mRef = optional (fieldText "ref" input)

optional :: Text -> Maybe Text
optional t = if T.null (T.strip t) then Nothing else Just (T.strip t)

{- | Resolves the repo slug and the HTTP manager once, so each backend call
sees only a valid slug and a live manager.
-}
withRepo ::
    App -> Text -> (Manager -> Text -> IO ToolOutcome) -> IO ToolOutcome
withRepo app rawRepo k = case (repoSlug rawRepo, appHttpMgr app) of
    (Left msg, _) -> pure (errOutcome (errorJson msg))
    (_, Nothing) ->
        pure (errOutcome (errorJson "GitHub access is unavailable (no HTTP manager)"))
    (Right slug, Just mgr) -> k mgr slug

listWorkDir :: App -> Text -> IO ToolOutcome
listWorkDir app prefix = okOutcome . object . snd <$> listingPairs app prefix

{- | What a listing of @prefix@ says, and how many files it found, so a read
that turns out to be aimed at a place answers in the shape list_files already
speaks and says no more about it than the listing did.
-}
listingPairs :: App -> Text -> IO (Int, [Pair])
listingPairs app prefix = do
    (files, total) <- listLocal workDir prefix
    emptyHint <-
        if null files && not (T.null (T.strip prefix))
            then nothingUnderPairs workDir prefix
            else pure []
    pure . (,) total $
        [ "source" .= ("work-dir" :: Text)
        , "path" .= prefix
        , "files" .= toJSON (map localEntryJson files)
        , "truncated" .= (total > length files)
        , "count" .= total
        , "shown" .= length files
        ]
            <> emptyHint
  where
    workDir = envWorkDir (appEnv app)

{- | The one entry shape a listing speaks, whichever backend answered it, so
a caller reads one schema.
-}
entryPairs :: Text -> Maybe Integer -> [Pair]
entryPairs path mSize = ("path" .= path) : ["size" .= s | Just s <- [mSize]]

localEntryJson :: FileEntry -> Value
localEntryJson e = object (entryPairs (feName e) (feBytes e))

{- | An empty listing is a question in itself; answer it here rather than
leaving the caller to guess whether the tree or the prefix was wrong.
-}
nothingUnderPairs :: FilePath -> Text -> IO [Pair]
nothingUnderPairs workDir prefix = do
    (candidates, searched) <- readMissCandidates workDir prefix
    pure
        [ "note" .= note candidates searched
        , "candidates" .= candidates
        ]
  where
    note cs searched
        | not (null cs) =
            "Nothing under "
                <> tick prefix
                <> ". Did you mean "
                <> T.intercalate " or " [tick c | c <- cs]
                <> "?"
        | searched == 0 =
            "Nothing under "
                <> tick prefix
                <> ", and no file is under the \
                   \work directory at all."
        | otherwise =
            "Nothing under "
                <> tick prefix
                <> ". Call list_files with no path to see the whole work \
                   \directory."

{- | Reads what it can and describes the rest: a file whose bytes are not
text still comes back with its size, its shape verdict and the printable runs
sampled from it, rather than as an error with nothing in it.
-}
readWorkDir :: App -> Text -> IO ToolOutcome
readWorkDir app path = do
    got <- readLocal (envWorkDir (appEnv app)) path
    case got of
        Left e -> readErrorOutcome app path e
        Right a ->
            pure . okOutcome . object $
                [ "source" .= ("work-dir" :: Text)
                , "path" .= path
                ]
                    <> artefactPairs a
                    <> contentPairs a

{- | A binary artefact returns no @content@: there is no text to return, and
a lenient decoding of container bytes would be characters the file does not
contain.
-}
contentPairs :: Artefact -> [Pair]
contentPairs a
    | arView a == BinaryView = []
    | otherwise = ["content" .= arText a]

{- | The one answer given when a read produced no artefact, so read_file and
peek_data never tell a caller different things about the same path.
-}
readErrorOutcome :: App -> Text -> ReadError -> IO ToolOutcome
readErrorOutcome app path err = case err of
    OutsideWorkDir ->
        pure (errOutcome (errorJson "that path is outside the work directory"))
    IsDirectory -> placeOutcome app path
    NotReadable ->
        pure . errOutcome $
            errorJsonWith
                (tick path <> " is there, and could not be opened for reading.")
                ["path" .= path]
    NotFound -> missOutcome (envWorkDir (appEnv app)) path

{- | A read aimed at a place answers with the place: the listing is what the
read was asking for, and it comes back without a second call.
-}
placeOutcome :: App -> Text -> IO ToolOutcome
placeOutcome app path = do
    (found, listing) <- listingPairs app path
    pure (errOutcome (errorJsonWith (message found) listing))
  where
    message 0 =
        tick path <> " is a directory, not a file, and no file was found under it."
    message _ =
        tick path
            <> " is a directory, not a file. `files` holds what was found \
               \under it, `count` how much of it there is; read one of those."

{- | A missed read carries the nearest real paths, so finding the right one
does not cost a second round trip through list_files.
-}
missOutcome :: FilePath -> Text -> IO ToolOutcome
missOutcome workDir path = do
    (candidates, searched) <- readMissCandidates workDir path
    pure . errOutcome $
        errorJsonWith
            (message candidates searched)
            ["missingPath" .= path, "candidates" .= candidates]
  where
    message [] 0 =
        "No such file: " <> tick path <> ". The work directory is empty."
    message [] searched =
        "No such file: "
            <> tick path
            <> ". None of the "
            <> T.pack (show searched)
            <> " paths under the work directory resembles it; call list_files \
               \to see them."
    message cs _ =
        "No such file: "
            <> tick path
            <> ". Did you mean "
            <> T.intercalate " or " (map tick cs)
            <> "?"

tick :: Text -> Text
tick t = "`" <> t <> "`"

listRepo :: App -> Maybe Text -> Text -> Manager -> Text -> IO ToolOutcome
listRepo _ mRef prefix mgr slug = do
    got <- fetchTree mgr slug mRef
    pure $ case got of
        Left msg -> errOutcome (errorJson msg)
        Right (entries, ghTruncated) ->
            let under = filter (T.isPrefixOf (T.dropWhile (== '/') prefix) . ghPath) entries
                shown = take listCap under
             in okOutcome $
                    object
                        [ "source" .= ("github" :: Text)
                        , "repo" .= slug
                        , "ref" .= fromMaybe "HEAD" mRef
                        , "path" .= prefix
                        , "files" .= toJSON (map entryJson shown)
                        , "truncated" .= (ghTruncated || length under > listCap)
                        , "count" .= length under
                        , "shown" .= length shown
                        ]

entryJson :: GhEntry -> Value
entryJson e = object (entryPairs (ghPath e) (fmap fromIntegral (ghSize e)))

readRepo :: Maybe Text -> Text -> Manager -> Text -> IO ToolOutcome
readRepo mRef path mgr slug = do
    got <- fetchBlob mgr slug mRef path
    pure $ case got of
        Left msg -> errOutcome (errorJson msg)
        Right body ->
            okOutcome $
                object
                    [ "source" .= ("github" :: Text)
                    , "repo" .= slug
                    , "ref" .= fromMaybe "HEAD" mRef
                    , "path" .= path
                    , "content" .= body
                    ]
