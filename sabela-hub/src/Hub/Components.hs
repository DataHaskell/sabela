{-# LANGUAGE OverloadedStrings #-}

{- | Components on the hub: a published component is a share whose page
shows the module and whose source is the bundle; "Use in my workspace"
installs that bundle into the signed-in user's own notebook server.
-}
module Hub.Components (
    handlePublishComponent,
    serveUseComponent,
    componentPage,
) where

import Control.Exception (SomeException, try)
import Data.Aeson (Value (..), decode, encode, object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types
import Network.Wai

import Hub.Brand (brandMark, faviconTag)
import Hub.OAuth (generateRandomToken)
import Hub.Pages (jsonError, jsonResponse)
import Hub.Session (SessionManager (..), logHub)
import Hub.Share (
    Share (..),
    ShareStore,
    lookupShareSource,
    publishShareRaw,
    scrubSecrets,
    validSlug,
 )
import Hub.Types

-- | @POST /_hub/publish?mode=component&module=Components.X@.
handlePublishComponent ::
    SessionManager ->
    ShareStore ->
    HC.Manager ->
    Session ->
    Request ->
    (Response -> IO ResponseReceived) ->
    IO ResponseReceived
handlePublishComponent sm store mgr sess req respond =
    case (sessionState sess, moduleParam) of
        (_, Nothing) -> respond (jsonError status400 "module is required")
        (SReady ip, Just m) -> do
            fetched <-
                try (getBackend mgr port ip ("/api/components/bundle?module=" <> m)) ::
                    IO (Either SomeException (Either Text BL.ByteString))
            case fetched of
                Left e ->
                    respond
                        (jsonError status502 ("Could not reach your notebook: " <> T.pack (show e)))
                Right (Left err) -> respond (jsonError status502 err)
                Right (Right body) -> case decode body of
                    Nothing -> respond (jsonError status502 "Your notebook returned no component bundle.")
                    Just bundle -> case scrubSecrets (TE.decodeUtf8 (BL.toStrict body)) of
                        Just reason ->
                            respond
                                ( jsonError
                                    status400
                                    ("Refusing to publish: the component appears to contain " <> reason <> ".")
                                )
                        Nothing -> do
                            slug <- generateRandomToken
                            now <- getCurrentTime
                            let share =
                                    Share
                                        { shareSlug = slug
                                        , shareOwner = email
                                        , shareMode = ExpComponent
                                        , shareCreatedAt = T.pack (iso8601Show now)
                                        , shareTitle = fromMaybe m (field "cbModule" bundle)
                                        }
                            publishShareRaw
                                store
                                share
                                (componentPage share bundle)
                                (Just (TE.decodeUtf8 (BL.toStrict (encode bundle))))
                            logHub (email <> " published component /s/" <> slug)
                            respond
                                (jsonResponse status200 (object ["slug" .= slug, "url" .= ("/s/" <> slug)]))
        _ ->
            respond
                (jsonError status409 "Your notebook is still starting; try again in a moment.")
  where
    UserId email = sessionUserId sess
    port = hcBackendPort (smConfig sm)
    moduleParam = case lookup "module" (queryString req) of
        Just (Just bs) -> Just (TE.decodeUtf8 bs)
        _ -> Nothing

-- | @POST /_hub/use/<slug>@: install the bundle into the signed-in user's server.
serveUseComponent ::
    SessionManager ->
    ShareStore ->
    HC.Manager ->
    Session ->
    Text ->
    Request ->
    (Response -> IO ResponseReceived) ->
    IO ResponseReceived
serveUseComponent sm store mgr sess slug req respond
    | not (validSlug slug) = respond notUsable
    | otherwise = do
        mBundle <- lookupShareSource store slug
        case (mBundle, sessionState sess) of
            (Nothing, _) -> respond notUsable
            (_, SStarting) ->
                respond
                    (jsonError status409 "Your notebook is still starting; try again in a moment.")
            (Just raw, SReady ip) -> case decode (BL.fromStrict raw) :: Maybe Value of
                Nothing -> respond notUsable
                Just bundle -> do
                    let body = encode (object ["irBundle" .= bundle, "irOverwrite" .= overwrite])
                    posted <-
                        try (postBackend mgr port ip "/api/components/install" body) ::
                            IO (Either SomeException (Either Text BL.ByteString))
                    case posted of
                        Left e ->
                            respond
                                (jsonError status502 ("Could not reach your notebook: " <> T.pack (show e)))
                        Right (Left err) -> respond (jsonError status409 err)
                        Right (Right _) ->
                            respond $
                                if wantsHtml
                                    then
                                        responseLBS status303 [("Location", "/?installed=" <> TE.encodeUtf8 slug)] ""
                                    else jsonResponse status200 (object ["installed" .= slug])
            _ -> respond notUsable
  where
    port = hcBackendPort (smConfig sm)
    notUsable = jsonError status404 "That share is not a component."
    overwrite = lookup "overwrite" (queryString req) == Just (Just "1")
    wantsHtml =
        maybe False ("text/html" `BS.isInfixOf`) (lookup hAccept (requestHeaders req))

getBackend ::
    HC.Manager -> Int -> TaskIp -> Text -> IO (Either Text BL.ByteString)
getBackend mgr port (TaskIp ip) path = do
    initReq <-
        HC.parseRequest ("http://" <> T.unpack ip <> ":" <> show port <> T.unpack path)
    run mgr initReq{HC.responseTimeout = HC.responseTimeoutMicro 30000000}

postBackend ::
    HC.Manager ->
    Int ->
    TaskIp ->
    Text ->
    BL.ByteString ->
    IO (Either Text BL.ByteString)
postBackend mgr port (TaskIp ip) path body = do
    initReq <-
        HC.parseRequest ("http://" <> T.unpack ip <> ":" <> show port <> T.unpack path)
    run
        mgr
        initReq
            { HC.method = "POST"
            , HC.requestHeaders = [(hContentType, "application/json")]
            , HC.requestBody = HC.RequestBodyLBS body
            , HC.responseTimeout = HC.responseTimeoutMicro 30000000
            }

run :: HC.Manager -> HC.Request -> IO (Either Text BL.ByteString)
run mgr req = do
    resp <- HC.httpLbs req mgr
    let st = HC.responseStatus resp
    pure $
        if statusIsSuccessful st
            then Right (HC.responseBody resp)
            else
                Left
                    ( "your notebook answered HTTP "
                        <> T.pack (show (statusCode st))
                        <> ": "
                        <> T.take 300 (TE.decodeUtf8 (BL.toStrict (HC.responseBody resp)))
                    )

field :: Text -> Value -> Maybe Text
field k (Object o) = case KM.lookup (Key.fromText k) o of
    Just (String s) -> Just s
    _ -> Nothing
field _ _ = Nothing

fields :: Text -> Value -> [Text]
fields k (Object o) = case KM.lookup (Key.fromText k) o of
    Just (Array xs) -> [s | String s <- foldr (:) [] xs]
    _ -> []
fields _ _ = []

{- | The share page: what the component is, what it exports, its source, and
a button that installs it into the reader's own workspace.
-}
componentPage :: Share -> Value -> Text
componentPage share bundle =
    T.concat
        [ "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\">"
        , "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">"
        , "<title>" <> esc m <> " · Sabela component</title>"
        , faviconTag
        , "<style>body{margin:0;background:#f9fbfc;color:#4c5773;font:14px/1.55 'Lato',system-ui,sans-serif}"
        , "main{max-width:820px;margin:0 auto;padding:32px 20px 80px}h1{font-size:22px;font-weight:700;margin:0 0 4px}"
        , ".dim{color:#949aab;font-size:12px}pre{background:#ffffff;border:1px solid #eeecec;border-radius:8px;padding:14px 16px;overflow:auto;font:12.5px/1.5 'Nordwand Mono',ui-monospace,monospace}"
        , "code{font-family:'Nordwand Mono',ui-monospace,monospace}.exports span{display:inline-block;margin:4px 6px 0 0;padding:2px 8px;border:1px solid #eeecec;border-radius:6px;background:#ffffff}"
        , "button{margin-top:18px;font:700 13px 'Lato',system-ui;padding:8px 16px;border:1px solid #509ee3;border-radius:6px;background:#509ee3;color:#ffffff;cursor:pointer}"
        , "a{color:#509ee3}</style></head><body><main>"
        , "<div class=\"dim\">" <> brandMark <> "Sabela · component</div>"
        , "<h1>" <> esc m <> "</h1>"
        , "<p>" <> esc purpose <> "</p>"
        , "<div class=\"exports\">"
            <> T.concat ["<span><code>" <> esc e <> "</code></span>" | e <- exports]
            <> "</div>"
        , if null deps
            then ""
            else "<p class=\"dim\">depends on " <> esc (T.intercalate ", " deps) <> "</p>"
        , "<form method=\"post\" action=\"/_hub/use/"
            <> shareSlug share
            <> "\"><button type=\"submit\">Use in my workspace</button></form>"
        , "<p class=\"dim\">Installs <code>components/"
            <> esc (T.replace "." "/" m)
            <> ".hs</code> into your workspace; then <code>import "
            <> esc m
            <> "</code> in any notebook.</p>"
        , "<pre><code>" <> esc source <> "</code></pre>"
        , "<p class=\"dim\">published by "
            <> esc (shareOwner share)
            <> " · <a href=\"/_hub/source/"
            <> shareSlug share
            <> "\">bundle.json</a></p>"
        , "</main></body></html>"
        ]
  where
    m = fromMaybe (shareTitle share) (field "cbModule" bundle)
    purpose = fromMaybe "" (field "cbPurpose" bundle)
    source = fromMaybe "" (field "cbSource" bundle)
    exports = fields "cbExports" bundle
    deps = fields "cbDeps" bundle
    esc =
        T.concatMap
            ( \c -> case c of
                '<' -> "&lt;"
                '>' -> "&gt;"
                '&' -> "&amp;"
                '"' -> "&quot;"
                _ -> T.singleton c
            )
