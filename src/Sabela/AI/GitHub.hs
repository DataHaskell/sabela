{-# LANGUAGE OverloadedStrings #-}

{- | Reads a GitHub repository as a file tree: one recursive listing call
against the trees API, and raw fetches for blob contents.
-}
module Sabela.AI.GitHub (
    GhEntry (..),
    repoSlug,
    treeUrl,
    rawUrl,
    parseTree,
    fetchTree,
    fetchBlob,
    githubStatus,
) where

import Data.Aeson (Value (..), decode, (.:), (.:?))
import Data.Aeson.Types (parseEither, withObject)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import Network.HTTP.Client (Manager)
import System.Environment (lookupEnv)

import Sabela.AI.Fetch (
    FetchSpec (..),
    OverCap (..),
    fetchBounded,
    statusError,
 )

data GhEntry = GhEntry
    { ghPath :: Text
    , ghSize :: Maybe Int
    }
    deriving (Eq, Show)

-- | Rejects anything that is not exactly one owner and one repository name.
repoSlug :: Text -> Either Text Text
repoSlug raw = case T.splitOn "/" (T.strip raw) of
    [owner, name]
        | segmentOk owner && segmentOk name -> Right (owner <> "/" <> name)
    _ -> Left "repo must be \"owner/name\", e.g. \"haskell/containers\""
  where
    segmentOk s =
        not (T.null s)
            && T.all (\c -> isAlphaNum c || c `elem` ("-._" :: String)) s
            && s /= ".."
            && s /= "."

treeUrl :: Text -> Maybe Text -> Text
treeUrl slug mRef =
    "https://api.github.com/repos/"
        <> slug
        <> "/git/trees/"
        <> refOr mRef
        <> "?recursive=1"

rawUrl :: Text -> Maybe Text -> Text -> Text
rawUrl slug mRef path =
    "https://raw.githubusercontent.com/"
        <> slug
        <> "/"
        <> refOr mRef
        <> "/"
        <> T.dropWhile (== '/') path

refOr :: Maybe Text -> Text
refOr = maybe "HEAD" (T.dropWhile (== '/') . T.strip)

-- | Blobs only; a directory is implied by the paths beneath it.
parseTree :: Value -> Either Text ([GhEntry], Bool)
parseTree v = either (const (Left "unexpected GitHub tree response")) Right parsed
  where
    parsed = parseEither (withObject "tree" entries) v
    entries o = do
        raw <- o .: "tree"
        truncated <- o .:? "truncated"
        items <- mapM (withObject "entry" one) raw
        pure ([e | (e, ty) <- items, ty == ("blob" :: Text)], truncated == Just True)
    one o = do
        path <- o .: "path"
        size <- o .:? "size"
        ty <- o .: "type"
        pure (GhEntry path size, ty)

fetchTree ::
    Manager -> Text -> Maybe Text -> IO (Either Text ([GhEntry], Bool))
fetchTree mgr slug mRef = do
    body <- fetchText mgr (treeUrl slug mRef)
    pure $ do
        raw <- body
        v <-
            maybe (Left "GitHub returned invalid JSON") Right (decode (LBS.fromStrict raw))
        parseTree v

fetchBlob :: Manager -> Text -> Maybe Text -> Text -> IO (Either Text Text)
fetchBlob mgr slug mRef path =
    fmap (TE.decodeUtf8With lenientDecode) <$> fetchText mgr (rawUrl slug mRef path)

maxFetchBytes :: Int
maxFetchBytes = 512 * 1024

fetchText :: Manager -> Text -> IO (Either Text BS.ByteString)
fetchText mgr url = do
    token <- lookupEnv "SABELA_GITHUB_TOKEN"
    fmap LBS.toStrict <$> fetchBounded (fs token) mgr url
  where
    fs token =
        FetchSpec
            { fsService = "GitHub"
            , fsHeaders = headers token
            , fsCap = maxFetchBytes
            , fsOverCap = TruncateAtCap
            , fsStatus = githubStatus
            }
    headers token =
        [ ("User-Agent", "sabela")
        , ("Accept", "application/vnd.github+json")
        ]
            <> [ ("Authorization", "Bearer " <> TE.encodeUtf8 (T.pack t))
               | Just t <- [token]
               ]

-- | The GitHub status ladder, pure so its branches test.
githubStatus :: Int -> Maybe Text
githubStatus =
    statusError
        "GitHub"
        [ (404, "GitHub has no such repository, ref, or path")
        , (403, rateLimited)
        , (429, rateLimited)
        ]
  where
    rateLimited =
        "GitHub rate-limited this request. Unauthenticated access allows 60 \
        \requests an hour; set SABELA_GITHUB_TOKEN to raise it."
