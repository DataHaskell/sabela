{-# LANGUAGE OverloadedStrings #-}

{- | Device-authorization flow for the @siza@ CLI (RFC 8628 shape), so a
hub user can drive their notebook from the terminal without copying the
@HttpOnly@ session cookie out of DevTools.

Flow: the CLI POSTs @start@ (no auth) and gets a secret @deviceCode@ plus a
short @userCode@; it opens the browser at the *bare* authorize page (the code is
never put in a URL, so it can't leak into history or access logs); the logged-in
user types the code shown in their terminal and approves, which mints a
short-lived token bound to *their* session; the CLI polls @poll@ with the
@deviceCode@ until the token appears. 'resolveCliToken' accepts that token as a
@Bearer@ at the proxy boundary — and since 'Hub.Proxy.Forward' strips
@Authorization@, it never reaches the backend.

The token resolves to the approver's 'SessionId', so it drives the SAME
session/backend and is only valid while that session is live ('lookupBySessionId'
fails once it ends). It is also revoked by @siza logout@ ('handleCliRevoke'), by
the user signing out ('revokeSessionTokens'), and by its own 'hcCliTokenTtl'.

The @start@ endpoint is unauthenticated, so the pending table is capped
('maxPending') and pruned on every call to bound memory; user codes are unique
and high-entropy.
-}
module Hub.CliAuth (
    CliAuth,
    newCliAuth,
    resolveCliToken,
    revokeSessionTokens,
    handleCliStart,
    handleCliPoll,
    handleCliApprove,
    handleCliRevoke,
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
import qualified Data.ByteString.Char8 as B8
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
import Hub.CliAuth.Page (authorizePage, noticePage)
import Hub.OAuth (generateRandomToken)
import Hub.Pages (jsonError, jsonResponse)
import Hub.Types (HubConfig (..), SessionId (..))
import Network.HTTP.Types
import Network.Wai

{- | A pending authorization request, keyed by its secret @deviceCode@. Holds
the user-facing @userCode@, the creation time (for TTL), and — once approved —
the minted token.
-}
data Pending = Pending
    { pUserCode :: Text
    , pCreated :: UTCTime
    , pToken :: Maybe Text
    }

{- | The CLI-auth stores, the token lifetime, and the hub's public origin.
@caCsrf@ maps each authorize-page CSRF nonce to the session it was minted for
and when, so 'handleCliApprove' can verify the POST came from that session's own
page — decoupled from the user code (which never appears in any URL).
-}
data CliAuth = CliAuth
    { caPending :: TVar (Map.Map Text Pending)
    , caTokens :: TVar (Map.Map Text (SessionId, UTCTime))
    , caCsrf :: TVar (Map.Map Text (SessionId, UTCTime))
    , caTtl :: NominalDiffTime
    , caOrigin :: Text
    }

newCliAuth :: HubConfig -> IO CliAuth
newCliAuth cfg =
    CliAuth
        <$> newTVarIO Map.empty
        <*> newTVarIO Map.empty
        <*> newTVarIO Map.empty
        <*> pure (hcCliTokenTtl cfg)
        <*> pure (originFromRedirect (hcGoogleRedirectUri cfg))

-- | How long a pending (unapproved) request stays valid (5 min).
requestTtl :: NominalDiffTime
requestTtl = 300

-- | Poll interval the CLI is told to wait between polls, in seconds.
pollInterval :: Int
pollInterval = 2

{- | Upper bound on concurrently-pending requests. @start@ is unauthenticated,
so this caps the memory an attacker can pin by spraying it; expired entries are
pruned on every @start@, so legitimate traffic never approaches it.
-}
maxPending :: Int
maxPending = 1000

-- ---------------------------------------------------------------------------
-- Resolve / revoke tokens
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

-- | Revoke every CLI token bound to a session (used when the user signs out).
revokeSessionTokens :: CliAuth -> SessionId -> IO ()
revokeSessionTokens ca sid =
    atomically $ modifyTVar' (caTokens ca) (Map.filter ((/= sid) . fst))

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

{- | @POST /_hub/cli-auth/start@ (no auth): prune expired entries, refuse with
429 when the pending table is full, else mint a unique @userCode@ + secret
@deviceCode@ and return them plus the authorize URL (built from the hub's
configured origin, not the request @Host@).
-}
handleCliStart :: CliAuth -> Application
handleCliStart ca req respond = do
    now <- getCurrentTime
    full <- atomically $ do
        modifyTVar' (caPending ca) (Map.filter (notExpired now))
        (>= maxPending) . Map.size <$> readTVar (caPending ca)
    if full
        then
            respond $ jsonError status429 "Too many pending authorizations; retry shortly."
        else do
            deviceCode <- generateRandomToken
            userCode <- freshUserCode ca
            atomically $
                modifyTVar'
                    (caPending ca)
                    (Map.insert deviceCode (Pending userCode now Nothing))
            let origin = if T.null (caOrigin ca) then originOf req else caOrigin ca
            respond $
                jsonResponse status200 $
                    object
                        [ "deviceCode" .= deviceCode
                        , "userCode" .= userCode
                        , "verificationUri" .= (origin <> "/_hub/cli-auth")
                        , "interval" .= pollInterval
                        , "expiresIn" .= (round requestTtl :: Int)
                        ]

-- | A 12-hex-char (48-bit) upper-cased user code, unique among live pendings.
freshUserCode :: CliAuth -> IO Text
freshUserCode ca = do
    c <- T.toUpper . T.take 12 <$> generateRandomToken
    pend <- readTVarIO (caPending ca)
    if any ((== c) . pUserCode) (Map.elems pend) then freshUserCode ca else pure c

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
-- authorize page + approve + revoke
-- ---------------------------------------------------------------------------

{- | @GET /_hub/cli-auth@: the authorize page. The router gates this on a *live*
session (redirecting an unauthenticated visitor through Google login and back),
so here we mint a CSRF nonce bound to that session and embed it in the form.
Neither the page nor its URL carries the user code — the approver types it from
their terminal, and 'handleCliApprove' validates the typed value.
-}
cliAuthPage :: CliAuth -> Application
cliAuthPage ca req respond =
    case extractSessionId req of
        Nothing -> respond $ noticePage status401 "Not signed in."
        Just sid -> do
            now <- getCurrentTime
            csrf <- generateRandomToken
            atomically $
                modifyTVar' (caCsrf ca) $
                    Map.insert csrf (sid, now) . Map.filter (fresh now)
            respond (authorizePage csrf)

{- | @POST /_hub/cli-auth/approve@ with @{userCode, csrf}@. Cookie-gated by the
router; binds a fresh short-lived token to the approver's 'SessionId'. The CSRF
nonce must be one this session's authorize page minted (so a blind cross-site
POST can't approve), and the @userCode@ must match a live pending request (the
approver supplies it from their terminal — it is never in any URL or page). A
matched nonce is consumed; a wrong code leaves it so the user can retype.
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
                    csrfs <- readTVar (caCsrf ca)
                    pend <- readTVar (caPending ca)
                    let csrfOk = case Map.lookup csrf csrfs of
                            Just (s, t) -> s == sid && fresh now (s, t)
                            Nothing -> False
                    case findByUserCode userCode pend of
                        Just (dc, p)
                            | csrfOk
                            , notExpired now p -> do
                                modifyTVar' (caCsrf ca) (Map.delete csrf)
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

{- | @POST /_hub/cli-auth/revoke@ with @Authorization: Bearer <token>@: delete
that token server-side. No cookie needed — presenting the token is proof enough
to revoke it. Backs @siza logout@.
-}
handleCliRevoke :: CliAuth -> Application
handleCliRevoke ca req respond =
    case bearerToken req of
        Nothing -> respond $ jsonError status400 "Expected a bearer token."
        Just tok -> do
            atomically $ modifyTVar' (caTokens ca) (Map.delete tok)
            respond $ jsonResponse status200 (object ["revoked" .= True])

-- ---------------------------------------------------------------------------
-- helpers
-- ---------------------------------------------------------------------------

notExpired :: UTCTime -> Pending -> Bool
notExpired now p = diffUTCTime now (pCreated p) < requestTtl

-- | A timestamped entry (CSRF nonce or pending) is still within 'requestTtl'.
fresh :: UTCTime -> (a, UTCTime) -> Bool
fresh now (_, t) = diffUTCTime now t < requestTtl

findByUserCode :: Text -> Map.Map Text Pending -> Maybe (Text, Pending)
findByUserCode uc = find ((== uc) . pUserCode . snd) . Map.toList

strField :: Text -> Value -> Maybe Text
strField k v = case v of
    Object o -> case KM.lookup (Key.fromText k) o of
        Just (String s) -> Just s
        _ -> Nothing
    _ -> Nothing

{- | The hub's public @scheme://host@ origin, derived from the configured OAuth
redirect URI (trusted config) rather than the request @Host@ header. Empty when
the redirect URI is unset (dev), where the caller falls back to 'originOf'.
-}
originFromRedirect :: Text -> Text
originFromRedirect uri = case T.splitOn "/" uri of
    (scheme : "" : host : _) | not (T.null host) -> scheme <> "//" <> host
    _ -> ""

-- | Fallback origin from @Host@ + @X-Forwarded-Proto@ (dev only).
originOf :: Request -> Text
originOf req =
    scheme <> "://" <> maybe "localhost" TE.decodeUtf8 (requestHeaderHost req)
  where
    scheme
        | lookup "X-Forwarded-Proto" (requestHeaders req) == Just "https" = "https"
        | otherwise = "http"
