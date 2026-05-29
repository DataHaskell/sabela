{-# LANGUAGE OverloadedStrings #-}

{- | Public, static notebook shares (Phase 3a).

A share is a self-contained HTML snapshot (the dashboard/slideshow export)
stored on EFS under an unguessable slug and served at @\/s\/<slug>@ with no
auth and no backend - so there is no anonymous compute, only a file serve.

Layout: @<baseDir>\/<slug>\/{index.html, meta}@. @meta@ is simple @key=value@
lines (owner, mode, createdAt) so listing/ownership work without parsing the
HTML. An in-memory cache mirrors the directory and is reloaded at startup.
-}
module Hub.Share (
    Share (..),
    ShareStore,
    newShareStore,
    publishShare,
    lookupShareHtml,
    listShares,
    deleteShare,

    -- * Pure helpers (exported for testing)
    validSlug,
    scrubSecrets,
    shareHeaders,
) where

import Control.Concurrent.STM
import Control.Monad (forM)
import qualified Data.ByteString as BS
import Data.Char (isDigit)
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
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

import Hub.Types (ExportMode, exportModeText, parseExportMode)

data Share = Share
    { shareSlug :: Text
    , shareOwner :: Text
    , shareMode :: ExportMode
    , shareCreatedAt :: Text
    }
    deriving (Eq, Show)

data ShareStore = ShareStore
    { ssBaseDir :: FilePath
    , ssCache :: TVar (Map Text Share)
    }

-- | Open a share store rooted at @dir@, loading any shares already on disk.
newShareStore :: FilePath -> IO ShareStore
newShareStore dir = do
    createDirectoryIfMissing True dir
    entries <- listDirectory dir
    shares <- catMaybes <$> forM entries (loadShare dir . T.pack)
    cache <- newTVarIO (Map.fromList [(shareSlug s, s) | s <- shares])
    pure ShareStore{ssBaseDir = dir, ssCache = cache}

-- | Store the snapshot HTML + metadata and cache the share.
publishShare :: ShareStore -> Share -> Text -> IO ()
publishShare store share html = do
    let dir = ssBaseDir store </> T.unpack (shareSlug share)
    createDirectoryIfMissing True dir
    BS.writeFile (dir </> "index.html") (TE.encodeUtf8 html)
    BS.writeFile (dir </> "meta") (TE.encodeUtf8 (metaText share))
    atomically $ modifyTVar' (ssCache store) (Map.insert (shareSlug share) share)

{- | The stored HTML for a slug, or 'Nothing'. Rejects non-slug input so a
crafted @\/s\/<slug>@ cannot traverse out of the base dir.
-}
lookupShareHtml :: ShareStore -> Text -> IO (Maybe BS.ByteString)
lookupShareHtml store slug
    | not (validSlug slug) = pure Nothing
    | otherwise = do
        let f = ssBaseDir store </> T.unpack slug </> "index.html"
        e <- doesFileExist f
        if e then Just <$> BS.readFile f else pure Nothing

-- | Shares owned by the given user.
listShares :: ShareStore -> Text -> IO [Share]
listShares store owner = do
    m <- readTVarIO (ssCache store)
    pure [s | s <- Map.elems m, shareOwner s == owner]

{- | Delete a share if it belongs to @owner@. Returns 'True' if removed,
'False' if missing or owned by someone else.
-}
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

-- ---------------------------------------------------------------------------
-- Pure helpers
-- ---------------------------------------------------------------------------

{- | A slug must be non-empty lowercase hex (as produced by
'Hub.OAuth.generateRandomToken'). This is the path-traversal guard for
@\/s\/<slug>@ - no @\/@, @.@, or @..@ can pass.
-}
validSlug :: Text -> Bool
validSlug s = not (T.null s) && T.all isHex s
  where
    isHex c = isDigit c || (c >= 'a' && c <= 'f')

{- | Best-effort guard: refuse to publish a snapshot that appears to contain a
credential, so a notebook output can't leak a key into a public page. 'Just' a
reason blocks the publish. This is a denylist of distinctive token shapes, not a
guarantee — the author is ultimately responsible for a public share's contents.
-}
scrubSecrets :: Text -> Maybe Text
scrubSecrets html = snd <$> find (\(p, _) -> p `T.isInfixOf` html) secretPatterns

{- | @(substring, human label)@. Only high-signal prefixes with a negligible
false-positive rate, so a legitimate notebook isn't wrongly blocked.
-}
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

{- | Response headers for a served share. The export carries its own strict
in-page CSP; these add framing/sniffing protection at the HTTP layer.
-}
shareHeaders :: [Header]
shareHeaders =
    [ ("Content-Type", "text/html; charset=utf-8")
    , ("X-Content-Type-Options", "nosniff")
    , ("X-Frame-Options", "SAMEORIGIN")
    , ("Content-Security-Policy", "frame-ancestors 'self'")
    ]

-- ---------------------------------------------------------------------------
-- Internal
-- ---------------------------------------------------------------------------

metaText :: Share -> Text
metaText s =
    T.unlines
        [ "owner=" <> shareOwner s
        , "mode=" <> exportModeText (shareMode s)
        , "createdAt=" <> shareCreatedAt s
        ]

loadShare :: FilePath -> Text -> IO (Maybe Share)
loadShare baseDir slug = do
    let metaF = baseDir </> T.unpack slug </> "meta"
    e <- doesFileExist metaF
    if not e
        then pure Nothing
        else do
            txt <- TE.decodeUtf8 <$> BS.readFile metaF
            let kv =
                    [ (k, T.drop 1 v)
                    | line <- T.lines txt
                    , let (k, v) = T.breakOn "=" line
                    , not (T.null v)
                    ]
                get k = lookup k kv
            pure $
                Share slug
                    <$> get "owner"
                    <*> (get "mode" >>= parseExportMode)
                    <*> get "createdAt"
