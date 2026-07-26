{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hub.Shares.Api (
    serveShare,
    serveAsset,
    handlePublish,
    handleListShares,
    handleDeleteShare,
    fetchExport,
    publishMode,
    parsePublishTitle,
    shareToJSON,
) where

import Control.Exception (SomeException, try)
import Data.Aeson (Value, decode, object, toJSON, withObject, (.:), (.=))
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlphaNum)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Hub.OAuth (generateRandomToken)
import Hub.Pages (jsonError, jsonResponse, shareNotFoundHtml)
import Hub.Session (SessionManager (..), logHub)
import Hub.Share (
    Share (..),
    ShareStore,
    deleteShare,
    listShares,
    lookupShareHtml,
    publishShare,
    sanitizeTitle,
    scrubSecrets,
    shareHeaders,
 )
import Hub.Types
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types
import Network.Wai
import System.Directory (doesFileExist)
import System.FilePath (takeExtension, (</>))

serveShare ::
    ShareStore -> Text -> (Response -> IO ResponseReceived) -> IO ResponseReceived
serveShare store slug respond = do
    mHtml <- lookupShareHtml store slug
    case mHtml of
        Just html ->
            respond $ responseLBS status200 shareHeaders (BL.fromStrict html)
        Nothing ->
            respond $
                responseLBS
                    status404
                    [(hContentType, "text/html; charset=utf-8")]
                    (BL.fromStrict (TE.encodeUtf8 shareNotFoundHtml))

serveAsset ::
    FilePath -> Text -> (Response -> IO ResponseReceived) -> IO ResponseReceived
serveAsset assetsDir name respond
    | not (safeAssetName name) = respond assetNotFound
    | otherwise = do
        let f = assetsDir </> T.unpack name
        e <- doesFileExist f
        if not e
            then respond assetNotFound
            else do
                bytes <- BS.readFile f
                respond $
                    responseLBS status200 (assetHeaders f) (BL.fromStrict bytes)

assetNotFound :: Response
assetNotFound =
    responseLBS status404 [(hContentType, "text/plain; charset=utf-8")] "Not found"

safeAssetName :: Text -> Bool
safeAssetName name =
    not (T.null name)
        && T.all ok name
        && not (T.isPrefixOf "." name)
        && not (T.isInfixOf ".." name)
  where
    ok c = isAlphaNum c || c == '-' || c == '_' || c == '.'

assetHeaders :: FilePath -> [Header]
assetHeaders f =
    [ (hContentType, ctype)
    , ("Cache-Control", "public, max-age=31536000, immutable")
    , ("X-Content-Type-Options", "nosniff")
    ]
  where
    ctype = case takeExtension f of
        ".html" -> "text/html; charset=utf-8"
        ".js" -> "text/javascript; charset=utf-8"
        ".mjs" -> "text/javascript; charset=utf-8"
        ".wasm" -> "application/wasm"
        ".css" -> "text/css; charset=utf-8"
        ".json" -> "application/json; charset=utf-8"
        ".map" -> "application/json; charset=utf-8"
        _ -> "application/octet-stream"

publishMode :: Query -> ExportMode
publishMode q =
    case lookup "mode" q of
        Just (Just bs) -> fromMaybe ExpDashboard (parseExportMode (TE.decodeUtf8 bs))
        _ -> ExpDashboard

parsePublishTitle :: BL.ByteString -> Text
parsePublishTitle body =
    maybe
        "Untitled"
        sanitizeTitle
        (decode body >>= parseMaybe (withObject "publish" (.: "title")))

handlePublish ::
    SessionManager ->
    ShareStore ->
    HC.Manager ->
    Session ->
    Request ->
    (Response -> IO ResponseReceived) ->
    IO ResponseReceived
handlePublish sm store mgr sess req respond = do
    body <- strictRequestBody req
    let cfg = smConfig sm
        UserId email = sessionUserId sess
        mode = publishMode (queryString req)
        title = parsePublishTitle body
    case sessionState sess of
        SReady ip -> do
            eFetched <-
                try (fetchExport mgr (hcBackendPort cfg) ip mode) ::
                    IO (Either SomeException (Either Text Text))
            case eFetched of
                Left e ->
                    respond . jsonError status502 $
                        "Could not reach your notebook: " <> T.pack (show e)
                Right (Left err) -> respond $ jsonError status502 err
                Right (Right html) -> case scrubSecrets html of
                    Just reason ->
                        respond . jsonError status400 $
                            "Refusing to publish: the export appears to contain "
                                <> reason
                                <> "."
                    Nothing -> do
                        eSrc <-
                            try (fetchSource mgr (hcBackendPort cfg) ip) ::
                                IO (Either SomeException (Either Text Text))
                        let mSrc = case eSrc of
                                Right (Right s) -> Just s
                                _ -> Nothing
                        case mSrc >>= scrubSecrets of
                            Just reason ->
                                respond . jsonError status400 $
                                    "Refusing to publish: the notebook source appears to contain "
                                        <> reason
                                        <> "."
                            Nothing -> do
                                slug <- generateRandomToken
                                now <- getCurrentTime
                                publishShare
                                    store
                                    Share
                                        { shareSlug = slug
                                        , shareOwner = email
                                        , shareMode = mode
                                        , shareCreatedAt = T.pack (iso8601Show now)
                                        , shareTitle = title
                                        }
                                    html
                                    mSrc
                                logHub $ email <> " published " <> exportModeText mode <> " /s/" <> slug
                                respond . jsonResponse status200 $
                                    object ["slug" .= slug, "url" .= ("/s/" <> slug)]
        _ ->
            respond . jsonError status409 $
                "Your notebook is still starting; try again in a moment."

handleListShares ::
    ShareStore ->
    Session ->
    (Response -> IO ResponseReceived) ->
    IO ResponseReceived
handleListShares store sess respond = do
    let UserId email = sessionUserId sess
    shares <- listShares store email
    respond . jsonResponse status200 $ toJSON (map shareToJSON shares)

handleDeleteShare ::
    ShareStore ->
    Session ->
    Text ->
    (Response -> IO ResponseReceived) ->
    IO ResponseReceived
handleDeleteShare store sess slug respond = do
    let UserId email = sessionUserId sess
    ok <- deleteShare store email slug
    if ok
        then respond . jsonResponse status200 $ object ["deleted" .= slug]
        else respond $ jsonError status404 "No such share, or it is not yours."

fetchExport ::
    HC.Manager -> Int -> TaskIp -> ExportMode -> IO (Either Text Text)
fetchExport mgr port (TaskIp ip) mode =
    fetchBackend mgr port ip ("/api/export/" <> exportModeText mode)

fetchSource :: HC.Manager -> Int -> TaskIp -> IO (Either Text Text)
fetchSource mgr port (TaskIp ip) = fetchBackend mgr port ip "/api/export/markdown"

fetchBackend :: HC.Manager -> Int -> Text -> Text -> IO (Either Text Text)
fetchBackend mgr port ip path = do
    initReq <-
        HC.parseRequest $
            "http://" <> T.unpack ip <> ":" <> show port <> T.unpack path
    let req = initReq{HC.responseTimeout = HC.responseTimeoutMicro 30000000}
    resp <- HC.httpLbs req mgr
    let st = HC.responseStatus resp
    if statusIsSuccessful st
        then pure . Right . TE.decodeUtf8 . BL.toStrict $ HC.responseBody resp
        else
            pure . Left $
                "the notebook export returned HTTP " <> T.pack (show (statusCode st))

shareToJSON :: Share -> Value
shareToJSON s =
    object
        [ "slug" .= shareSlug s
        , "mode" .= exportModeText (shareMode s)
        , "createdAt" .= shareCreatedAt s
        , "url" .= ("/s/" <> shareSlug s)
        ]
