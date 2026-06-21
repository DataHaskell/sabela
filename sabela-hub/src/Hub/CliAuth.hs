{-# LANGUAGE OverloadedStrings #-}

{- | Device-authorization flow for the @siza@ CLI (RFC 8628 shape), so a
hub user can drive their notebook from the terminal without copying the
@HttpOnly@ session cookie out of DevTools.

Flow: the CLI POSTs @start@ (no auth) and gets a secret @deviceCode@ plus a
short @userCode@; it opens the browser at the authorize page carrying the
@userCode@; the logged-in user clicks Approve, which mints a short-lived token
bound to *their* session; the CLI polls @poll@ with the @deviceCode@ until the
token appears. 'resolveCliToken' then accepts that token as a @Bearer@ at the
proxy boundary — and since 'Hub.Proxy.Forward' strips @Authorization@, it never
reaches the backend.

The token resolves to the approver's 'SessionId', so it drives the SAME
session/backend the browser is on and dies when that session ends;
'hcCliTokenTtl' caps it independently.
-}
module Hub.CliAuth (
    CliAuth,
    newCliAuth,
    resolveCliToken,
    handleCliStart,
    handleCliPoll,
    handleCliApprove,
    cliAuthPage,
) where

import Control.Concurrent.STM (
    TVar,
    atomically,
    modifyTVar',
    newTVarIO,
    readTVar,
    readTVarIO,
 )
import Data.Aeson (Value (..), decode, object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (
    NominalDiffTime,
    UTCTime,
    addUTCTime,
    diffUTCTime,
    getCurrentTime,
 )
import Hub.Auth (extractSessionId)
import Hub.OAuth (generateRandomToken)
import Hub.Pages (jsonError, jsonResponse, loginPage)
import Hub.Types (HubConfig (..), SessionId (..))
import Network.HTTP.Types
import Network.Wai

{- | A pending authorization request, keyed by its secret @deviceCode@. Holds
the user-facing @userCode@, a CSRF nonce minted when the authorize page is
rendered, the creation time (for TTL), and — once approved — the minted token.
-}
data Pending = Pending
    { pUserCode :: Text
    , pCsrf :: Maybe Text
    , pCreated :: UTCTime
    , pToken :: Maybe Text
    }

-- | The CLI-auth stores plus the configured token lifetime.
data CliAuth = CliAuth
    { caPending :: TVar (Map.Map Text Pending)
    , caTokens :: TVar (Map.Map Text (SessionId, UTCTime))
    , caTtl :: NominalDiffTime
    }

newCliAuth :: HubConfig -> IO CliAuth
newCliAuth cfg =
    CliAuth
        <$> newTVarIO Map.empty
        <*> newTVarIO Map.empty
        <*> pure (hcCliTokenTtl cfg)

-- | How long a pending (unapproved) request stays valid (5 min).
requestTtl :: NominalDiffTime
requestTtl = 300

-- | Poll interval the CLI is told to wait between polls, in seconds.
pollInterval :: Int
pollInterval = 2

-- ---------------------------------------------------------------------------
-- Resolve a token at the proxy boundary
-- ---------------------------------------------------------------------------

{- | Resolve a request's @Authorization: Bearer <token>@ to the session the
token was minted against, dropping it if expired. 'Nothing' for a missing,
unknown, or expired token — the caller then falls back to the cookie path.
-}
resolveCliToken :: CliAuth -> Request -> IO (Maybe SessionId)
resolveCliToken ca req =
    case bearerToken req of
        Nothing -> pure Nothing
        Just tok -> do
            now <- getCurrentTime
            atomically $ do
                toks <- readTVar (caTokens ca)
                modifyTVar' (caTokens ca) (Map.filter ((> now) . snd))
                pure $ case Map.lookup tok toks of
                    Just (sid, expiry) | expiry > now -> Just sid
                    _ -> Nothing

-- | The token from an @Authorization: Bearer <token>@ header, if present.
bearerToken :: Request -> Maybe Text
bearerToken req = do
    raw <- lookup hAuthorization (requestHeaders req)
    let (scheme, rest) = B8.break (== ' ') raw
    if T.toLower (TE.decodeUtf8 scheme) == "bearer"
        then nonEmpty (TE.decodeUtf8 (B8.dropWhile (== ' ') rest))
        else Nothing
  where
    nonEmpty t = if T.null t then Nothing else Just t

-- ---------------------------------------------------------------------------
-- start
-- ---------------------------------------------------------------------------

{- | @POST /_hub/cli-auth/start@ (no auth): mint a @deviceCode@ + @userCode@,
record the pending request, and return the codes plus the authorize URL. Prunes
expired pending entries on the way in so the map stays bounded.
-}
handleCliStart :: CliAuth -> Application
handleCliStart ca req respond = do
    deviceCode <- generateRandomToken
    userCode <- T.toUpper . T.take 8 <$> generateRandomToken
    now <- getCurrentTime
    let pending = Pending userCode Nothing now Nothing
    atomically $
        modifyTVar' (caPending ca) $
            Map.insert deviceCode pending . Map.filter (notExpired now)
    respond $
        jsonResponse status200 $
            object
                [ "deviceCode" .= deviceCode
                , "userCode" .= userCode
                , "verificationUri" .= (originOf req <> "/_hub/cli-auth?code=" <> userCode)
                , "interval" .= pollInterval
                , "expiresIn" .= (round requestTtl :: Int)
                ]

-- ---------------------------------------------------------------------------
-- poll
-- ---------------------------------------------------------------------------

{- | @POST /_hub/cli-auth/poll@ (no auth) with @{deviceCode}@: returns
@{status:"pending"}@ until approved, then @{status:"approved", token,
expiresIn}@ once (the entry is consumed), and @{status:"expired"}@ for an
unknown or timed-out request.
-}
handleCliPoll :: CliAuth -> Application
handleCliPoll ca req respond = do
    body <- strictRequestBody req
    now <- getCurrentTime
    case decode body >>= strField "deviceCode" of
        Nothing -> respond $ jsonError status400 "Expected {deviceCode}."
        Just deviceCode -> do
            pend <- readTVarIO (caPending ca)
            case Map.lookup deviceCode pend of
                Just p
                    | notExpired now p -> case pToken p of
                        Just tok -> do
                            atomically $
                                modifyTVar' (caPending ca) (Map.delete deviceCode)
                            respond $
                                jsonResponse status200 $
                                    object
                                        [ "status" .= ("approved" :: Text)
                                        , "token" .= tok
                                        , "expiresIn" .= (round (caTtl ca) :: Int)
                                        ]
                        Nothing ->
                            respond $
                                jsonResponse status200 (object ["status" .= ("pending" :: Text)])
                _ -> respond $ jsonResponse status200 (object ["status" .= ("expired" :: Text)])

-- ---------------------------------------------------------------------------
-- authorize page + approve
-- ---------------------------------------------------------------------------

{- | @GET /_hub/cli-auth?code=<userCode>@: the authorize page. Cookie-gated —
an unauthenticated visitor gets the login page (then re-opens the CLI link).
Mints a CSRF nonce into the matching pending request and embeds it in the form.
-}
cliAuthPage :: CliAuth -> Application
cliAuthPage ca req respond =
    case extractSessionId req of
        Nothing -> respond loginPage
        Just _ -> do
            now <- getCurrentTime
            case userCodeParam req of
                Nothing -> respond $ htmlPage status400 (noticeHtml "Missing authorization code.")
                Just userCode -> do
                    csrf <- generateRandomToken
                    found <- atomically $ do
                        pend <- readTVar (caPending ca)
                        case findByUserCode userCode pend of
                            Just (dc, p) | notExpired now p -> do
                                modifyTVar' (caPending ca) $
                                    Map.insert dc p{pCsrf = Just csrf}
                                pure True
                            _ -> pure False
                    respond $
                        if found
                            then htmlPage status200 (authorizeHtml userCode csrf)
                            else
                                htmlPage
                                    status410
                                    (noticeHtml "This authorization request has expired. Re-run siza login.")

{- | @POST /_hub/cli-auth/approve@ with @{userCode, csrf}@. Cookie-gated by the
router; binds a fresh short-lived token to the approver's 'SessionId'. The CSRF
nonce must match the one the page minted, so a blind cross-site POST can't
approve a pending request.
-}
handleCliApprove :: CliAuth -> Application
handleCliApprove ca req respond = do
    body <- strictRequestBody req
    now <- getCurrentTime
    case (extractSessionId req, decode body) of
        (Just sid, Just v)
            | Just userCode <- strField "userCode" v
            , Just csrf <- strField "csrf" v -> do
                token <- generateRandomToken
                ok <- atomically $ do
                    pend <- readTVar (caPending ca)
                    case findByUserCode userCode pend of
                        Just (dc, p)
                            | notExpired now p
                            , pCsrf p == Just csrf -> do
                                modifyTVar' (caPending ca) $
                                    Map.insert dc p{pToken = Just token}
                                modifyTVar' (caTokens ca) $
                                    Map.insert token (sid, addUTCTime (caTtl ca) now)
                                pure True
                        _ -> pure False
                respond $
                    if ok
                        then jsonResponse status200 (object ["status" .= ("approved" :: Text)])
                        else jsonError status410 "This authorization request has expired."
        _ -> respond $ jsonError status400 "Expected {userCode, csrf}."

-- ---------------------------------------------------------------------------
-- helpers
-- ---------------------------------------------------------------------------

notExpired :: UTCTime -> Pending -> Bool
notExpired now p = diffUTCTime now (pCreated p) < requestTtl

findByUserCode :: Text -> Map.Map Text Pending -> Maybe (Text, Pending)
findByUserCode uc = find ((== uc) . pUserCode . snd) . Map.toList

strField :: Text -> Value -> Maybe Text
strField k v = case v of
    Object o -> case KM.lookup (Key.fromText k) o of
        Just (String s) -> Just s
        _ -> Nothing
    _ -> Nothing

-- | The @scheme://host@ origin, from @Host@ and @X-Forwarded-Proto@.
originOf :: Request -> Text
originOf req =
    scheme <> "://" <> maybe "localhost" TE.decodeUtf8 (requestHeaderHost req)
  where
    scheme
        | lookup "X-Forwarded-Proto" (requestHeaders req) == Just "https" = "https"
        | otherwise = "http"

userCodeParam :: Request -> Maybe Text
userCodeParam req = do
    mv <- lookup "code" (queryString req)
    raw <- mv
    if BS.null raw then Nothing else Just (TE.decodeUtf8 raw)

htmlPage :: Status -> Text -> Response
htmlPage st body =
    responseLBS
        st
        [(hContentType, "text/html; charset=utf-8")]
        (BL.fromStrict (TE.encodeUtf8 body))

{- | The authorize page: shows the user code and an Approve button that POSTs
@{userCode, csrf}@ to @approve@ (the session cookie rides along automatically),
then confirms in place so the user knows to return to the terminal.
-}
authorizeHtml :: Text -> Text -> Text
authorizeHtml userCode csrf =
    T.unlines
        [ "<!doctype html><html lang=en><head><meta charset=utf-8>"
        , "<meta name=viewport content='width=device-width,initial-scale=1'>"
        , "<title>Authorize siza CLI</title>"
        , "<style>body{font-family:system-ui,sans-serif;max-width:34rem;margin:4rem auto;padding:0 1.5rem;color:#2e3440;background:#fdfaf5}"
        , "h1{font-size:1.4rem}code{background:#eceff4;padding:.15rem .4rem;border-radius:.3rem;font-size:1.1rem;letter-spacing:.1em}"
        , "button{font-size:1rem;padding:.7rem 1.4rem;border:0;border-radius:.5rem;background:#5e81ac;color:#fff;cursor:pointer}"
        , "button:disabled{background:#9aa5b1;cursor:default}#ok{color:#3b7a57;font-weight:600}</style></head><body>"
        , "<h1>Authorize the siza CLI</h1>"
        , "<p>A terminal is requesting access to drive your notebook. Confirm the code matches the one shown in your terminal:</p>"
        , "<p><code>" <> userCode <> "</code></p>"
        , "<p><button id=go onclick=approve()>Approve access</button></p>"
        , "<p id=ok hidden>Approved \x2014 return to your terminal.</p>"
        , "<script>"
        , "async function approve(){"
        , "  document.getElementById('go').disabled=true;"
        , "  const r=await fetch('/_hub/cli-auth/approve',{method:'POST',headers:{'content-type':'application/json'},"
        , "    body:JSON.stringify({userCode:'"
            <> userCode
            <> "',csrf:'"
            <> csrf
            <> "'})});"
        , "  if(r.ok){document.getElementById('ok').hidden=false;document.getElementById('go').hidden=true;}"
        , "  else{document.getElementById('go').disabled=false;alert('Authorization failed or expired. Re-run siza login.');}"
        , "}"
        , "</script></body></html>"
        ]

-- | A minimal notice page for the error/expiry cases.
noticeHtml :: Text -> Text
noticeHtml msg =
    T.unlines
        [ "<!doctype html><html lang=en><head><meta charset=utf-8>"
        , "<title>siza CLI</title>"
        , "<style>body{font-family:system-ui,sans-serif;max-width:34rem;margin:4rem auto;padding:0 1.5rem;color:#2e3440;background:#fdfaf5}</style>"
        , "</head><body><p>" <> msg <> "</p></body></html>"
        ]
