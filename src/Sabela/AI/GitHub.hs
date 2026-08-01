{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
) where

import Control.Exception (SomeException, try)
import Data.Aeson (Value (..), decode, (.:), (.:?))
import Data.Aeson.Types (parseEither, withObject)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import Network.HTTP.Client (
    Manager,
    Request,
    parseRequest,
    requestHeaders,
    responseBody,
    responseStatus,
    withResponse,
 )
import Network.HTTP.Types (statusCode)
import System.Environment (lookupEnv)

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
        not (T.null s) && T.all (\c -> isAlphaNum c || c `elem` ("-._" :: String)) s && s /= ".." && s /= "."

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
        v <- maybe (Left "GitHub returned invalid JSON") Right (decode (LBS.fromStrict raw))
        parseTree v

fetchBlob :: Manager -> Text -> Maybe Text -> Text -> IO (Either Text Text)
fetchBlob mgr slug mRef path =
    fmap (TE.decodeUtf8With lenientDecode) <$> fetchText mgr (rawUrl slug mRef path)

maxFetchBytes :: Int
maxFetchBytes = 512 * 1024

fetchText :: Manager -> Text -> IO (Either Text BS.ByteString)
fetchText mgr url = do
    token <- lookupEnv "SABELA_GITHUB_TOKEN"
    eReq <- try (parseRequest (T.unpack url)) :: IO (Either SomeException Request)
    case eReq of
        Left _ -> pure (Left "could not parse the GitHub URL")
        Right req0 -> do
            let req = req0{requestHeaders = headers token}
            eRes <-
                try (withResponse req mgr readCapped) ::
                    IO (Either SomeException (Either Text BS.ByteString))
            pure (either (Left . transportError) id eRes)
  where
    headers token =
        [ ("User-Agent", "sabela")
        , ("Accept", "application/vnd.github+json")
        ]
            <> [ ("Authorization", "Bearer " <> TE.encodeUtf8 (T.pack t))
               | Just t <- [token]
               ]
    readCapped resp = case statusCode (responseStatus resp) of
        404 -> pure (Left "GitHub has no such repository, ref, or path")
        403 -> pure (Left rateLimited)
        429 -> pure (Left rateLimited)
        sc
            | sc < 200 || sc >= 300 ->
                pure (Left ("GitHub returned HTTP " <> T.pack (show sc)))
            | otherwise -> Right <$> drainCapped (responseBody resp)
    rateLimited =
        "GitHub rate-limited this request. Unauthenticated access allows 60 \
        \requests an hour; set SABELA_GITHUB_TOKEN to raise it."
    transportError e = "GitHub request failed: " <> firstLine (T.pack (show e))

drainCapped :: IO BS.ByteString -> IO BS.ByteString
drainCapped readChunk = go [] 0
  where
    go acc n = do
        chunk <- readChunk
        if BS.null chunk || n >= maxFetchBytes
            then pure (BS.concat (reverse acc))
            else go (chunk : acc) (n + BS.length chunk)

firstLine :: Text -> Text
firstLine t = case T.lines t of
    (l : _) -> l
    [] -> t
