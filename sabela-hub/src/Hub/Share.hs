{-# LANGUAGE OverloadedStrings #-}

module Hub.Share (
    Share (..),
    ShareStore,
    newShareStore,
    publishShare,
    writeShareSource,
    lookupShareHtml,
    lookupShareSource,
    listShares,
    listAllShares,
    deleteShare,
    validSlug,
    sanitizeTitle,
    scrubSecrets,
    shareHeaders,
) where

import Control.Concurrent.STM
import Control.Monad (forM)
import qualified Data.ByteString as BS
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (Header)
import System.Directory (
    createDirectoryIfMissing,
    doesFileExist,
    listDirectory,
    removeDirectoryRecursive,
 )
import System.FilePath ((</>))

import Hub.Banner (spliceBanner)
import Hub.Meta (parseMeta, sanitizeLine, writeMetaLine)
import Hub.Runner (spliceRunner)
import Hub.Types (ExportMode, exportModeText, isLowerHex, parseExportMode)

data Share = Share
    { shareSlug :: Text
    , shareOwner :: Text
    , shareMode :: ExportMode
    , shareCreatedAt :: Text
    , shareTitle :: Text
    }
    deriving (Eq, Show)

data ShareStore = ShareStore
    { ssBaseDir :: FilePath
    , ssCache :: TVar (Map Text Share)
    }

newShareStore :: FilePath -> IO ShareStore
newShareStore dir = do
    createDirectoryIfMissing True dir
    entries <- listDirectory dir
    shares <- catMaybes <$> forM entries (loadShare dir . T.pack)
    cache <- newTVarIO (Map.fromList [(shareSlug s, s) | s <- shares])
    pure ShareStore{ssBaseDir = dir, ssCache = cache}

publishShare :: ShareStore -> Share -> Text -> Maybe Text -> IO ()
publishShare store share html mSrc = do
    let dir = ssBaseDir store </> T.unpack (shareSlug share)
        slug = shareSlug share
        src = fromMaybe "" mSrc
    createDirectoryIfMissing True dir
    BS.writeFile
        (dir </> "index.html")
        ( spliceRunner
            slug
            src
            (spliceBanner slug (TE.encodeUtf8 html))
        )
    BS.writeFile (dir </> "meta") (TE.encodeUtf8 (metaText share))
    maybe (pure ()) (writeShareSource store slug) mSrc
    atomically $ modifyTVar' (ssCache store) (Map.insert slug share)

lookupShareHtml :: ShareStore -> Text -> IO (Maybe BS.ByteString)
lookupShareHtml store slug
    | not (validSlug slug) = pure Nothing
    | otherwise = do
        let f = ssBaseDir store </> T.unpack slug </> "index.html"
        e <- doesFileExist f
        if e then Just <$> BS.readFile f else pure Nothing

writeShareSource :: ShareStore -> Text -> Text -> IO ()
writeShareSource store slug src
    | not (validSlug slug) = pure ()
    | otherwise = do
        let dir = ssBaseDir store </> T.unpack slug
        createDirectoryIfMissing True dir
        BS.writeFile (dir </> "source.md") (TE.encodeUtf8 src)

lookupShareSource :: ShareStore -> Text -> IO (Maybe BS.ByteString)
lookupShareSource store slug
    | not (validSlug slug) = pure Nothing
    | otherwise = do
        let f = ssBaseDir store </> T.unpack slug </> "source.md"
        e <- doesFileExist f
        if e then Just <$> BS.readFile f else pure Nothing

listShares :: ShareStore -> Text -> IO [Share]
listShares store owner = do
    m <- readTVarIO (ssCache store)
    pure [s | s <- Map.elems m, shareOwner s == owner]

listAllShares :: ShareStore -> IO [Share]
listAllShares store = Map.elems <$> readTVarIO (ssCache store)

deleteShare :: ShareStore -> Text -> Text -> IO Bool
deleteShare store owner slug
    | not (validSlug slug) = pure False
    | otherwise = do
        m <- readTVarIO (ssCache store)
        case Map.lookup slug m of
            Just s | shareOwner s == owner -> do
                removeDirectoryRecursive (ssBaseDir store </> T.unpack slug)
                atomically $ modifyTVar' (ssCache store) (Map.delete slug)
                pure True
            _ -> pure False

validSlug :: Text -> Bool
validSlug = isLowerHex

sanitizeTitle :: Text -> Text
sanitizeTitle t =
    let cleaned = T.take 200 (sanitizeLine t)
     in if T.null (T.strip cleaned) then "Untitled" else cleaned

scrubSecrets :: Text -> Maybe Text
scrubSecrets html = snd <$> find (\(p, _) -> p `T.isInfixOf` html) secretPatterns

secretPatterns :: [(Text, Text)]
secretPatterns =
    [ ("sk-ant-", "an Anthropic API key")
    , ("GOCSPX-", "a Google client secret")
    , ("AKIA", "an AWS access key")
    , ("ghp_", "a GitHub token")
    , ("github_pat_", "a GitHub fine-grained token")
    , ("xoxb-", "a Slack token")
    , ("xoxp-", "a Slack token")
    , ("sk_live_", "a Stripe secret key")
    , ("-----BEGIN ", "a private key")
    ]

shareHeaders :: [Header]
shareHeaders =
    [ ("Content-Type", "text/html; charset=utf-8")
    , ("X-Content-Type-Options", "nosniff")
    , ("X-Frame-Options", "SAMEORIGIN")
    , ("Content-Security-Policy", "frame-ancestors 'self'")
    ]

metaText :: Share -> Text
metaText s =
    T.unlines
        [ writeMetaLine "owner" (shareOwner s)
        , writeMetaLine "mode" (exportModeText (shareMode s))
        , writeMetaLine "createdAt" (shareCreatedAt s)
        , writeMetaLine "title" (sanitizeTitle (shareTitle s))
        ]

loadShare :: FilePath -> Text -> IO (Maybe Share)
loadShare baseDir slug = do
    let metaF = baseDir </> T.unpack slug </> "meta"
    e <- doesFileExist metaF
    if not e
        then pure Nothing
        else do
            txt <- TE.decodeUtf8Lenient <$> BS.readFile metaF
            let get k = lookup k (parseMeta txt)
                title = maybe "Untitled" sanitizeTitle (get "title")
            pure $
                Share slug
                    <$> get "owner"
                    <*> (get "mode" >>= parseExportMode)
                    <*> get "createdAt"
                    <*> pure title
