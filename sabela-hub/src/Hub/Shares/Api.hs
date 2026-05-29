{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | HTTP layer for published shares: the public @\/s\/<slug>@ static-serve and
the authed @\/_hub\/{publish,shares,shares\/<slug>}@ JSON endpoints.
-}
module Hub.Shares.Api (
    serveShare,
    handlePublish,
    handleListShares,
    handleDeleteShare,
    fetchExport,
    publishMode,
    shareToJSON,
) where

import Control.Exception (SomeException, try)
import Data.Aeson (Value, object, toJSON, (.=))
import qualified Data.ByteString.Lazy as BL
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
    scrubSecrets,
    shareHeaders,
 )
import Hub.Types
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types
import Network.Wai

{- | Serve a published share at @\/s\/<slug>@ with no auth and no backend.
'lookupShareHtml' rejects non-slug input, so a crafted slug cannot traverse out
of the shares directory.
-}
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

{- | Validate the publish @?mode=@ against the allowlist; an absent or unknown
mode falls back to 'ExpDashboard'. The mode is passed straight through to the
backend's @\/api\/export\/<mode>@ as a typed value, so this is the gate on
which export modes are publishable.
-}
publishMode :: Query -> ExportMode
publishMode q =
    case lookup "mode" q of
        Just (Just bs) -> maybe ExpDashboard id (parseExportMode (TE.decodeUtf8 bs))
        _ -> ExpDashboard

{- | @POST \/_hub\/publish?mode=dashboard|slideshow|notebook@: fetch the owner
container's self-contained HTML export, refuse it if it looks like it embeds a
credential, then store it under a fresh slug. Responds @{slug,url}@.
-}
handlePublish ::
    SessionManager ->
    ShareStore ->
    HC.Manager ->
    Session ->
    Request ->
    (Response -> IO ResponseReceived) ->
    IO ResponseReceived
handlePublish sm store mgr sess req respond = do
    let cfg = smConfig sm
        UserId email = sessionUserId sess
        mode = publishMode (queryString req)
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
                        slug <- generateRandomToken
                        now <- getCurrentTime
                        publishShare
                            store
                            Share
                                { shareSlug = slug
                                , shareOwner = email
                                , shareMode = mode
                                , shareCreatedAt = T.pack (iso8601Show now)
                                }
                            html
                        logHub $ email <> " published " <> exportModeText mode <> " /s/" <> slug
                        respond . jsonResponse status200 $
                            object ["slug" .= slug, "url" .= ("/s/" <> slug)]
        _ ->
            respond . jsonError status409 $
                "Your notebook is still starting; try again in a moment."

-- | @GET \/_hub\/shares@: the caller's published shares as JSON.
handleListShares ::
    ShareStore ->
    Session ->
    (Response -> IO ResponseReceived) ->
    IO ResponseReceived
handleListShares store sess respond = do
    let UserId email = sessionUserId sess
    shares <- listShares store email
    respond . jsonResponse status200 $ toJSON (map shareToJSON shares)

{- | @DELETE \/_hub\/shares\/<slug>@: unpublish one of the caller's shares.
'deleteShare' is owner-checked, so another user's slug yields 404.
-}
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

-- | Fetch a notebook's static export over HTTP from its backend container.
fetchExport :: HC.Manager -> Int -> TaskIp -> ExportMode -> IO (Either Text Text)
fetchExport mgr port (TaskIp ip) mode = do
    initReq <-
        HC.parseRequest $
            "http://"
                <> T.unpack ip
                <> ":"
                <> show port
                <> "/api/export/"
                <> T.unpack (exportModeText mode)
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
