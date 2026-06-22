{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Session, cookie, and CSRF handling for the hub: extracting the session
cookie, gating share endpoints behind a valid session, and the Google OAuth
callback with its CSRF @state@ map.
-}
module Hub.Auth (
    PendingStates,
    csrfStateTtl,
    secureAttr,
    dropExpiredStates,
    extractSessionId,
    parseCookies,
    requireSession,
    requireSessionOrLogin,
    handleLogin,
    handleOAuthCallback,
    logoutResponse,
) where

import Control.Concurrent.STM (
    TVar,
    atomically,
    modifyTVar',
    readTVarIO,
 )
import Control.Exception (SomeException, try)
import Control.Monad (join)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Hub.Allowlist (checkAllowed)
import Hub.OAuth (exchangeCodeForEmail, generateRandomToken, googleAuthUrl)
import Hub.Pages (jsonError, textResponse)
import Hub.Session (
    SessionManager (..),
    lookupBySessionId,
    startSessionAsync,
 )
import Hub.Types
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types
import Network.Wai

{- | Pending OAuth states (CSRF protection). Maps each state token to its
creation time and an optional local return path to redirect to after login
(used by the @siza@ CLI flow to come back to the authorize page).
-}
type PendingStates = TVar (Map.Map Text (UTCTime, Maybe Text))

{- | The CSRF @state@ time-to-live (10 min); shared by the login insert (which
prunes expired entries) and the callback validity check.
-}
csrfStateTtl :: NominalDiffTime
csrfStateTtl = 600

{- | @"; Secure"@ when the request arrived over HTTPS (the ALB sets
@X-Forwarded-Proto: https@), else empty — so production session cookies are
HTTPS-only while local plain-HTTP development still logs in.
-}
secureAttr :: Request -> BS.ByteString
secureAttr req
    | lookup "X-Forwarded-Proto" (requestHeaders req) == Just "https" = "; Secure"
    | otherwise = ""

{- | Drop CSRF @state@ entries older than 'csrfStateTtl' relative to @now@; the
login handler applies this on each insert so the map stays bounded.
-}
dropExpiredStates ::
    UTCTime ->
    Map.Map Text (UTCTime, Maybe Text) ->
    Map.Map Text (UTCTime, Maybe Text)
dropExpiredStates now = Map.filter ((< csrfStateTtl) . diffUTCTime now . fst)

{- | Run the continuation with the caller's session, or reply 401 JSON when the
request carries no valid @_sabela_session@. Gates the @\/_hub\/*@ share
endpoints, which answer JSON (unlike the HTML login page).
-}
requireSession ::
    SessionManager ->
    Request ->
    (Response -> IO ResponseReceived) ->
    (Session -> IO ResponseReceived) ->
    IO ResponseReceived
requireSession sm req respond k =
    case extractSessionId req of
        Nothing -> respond $ jsonError status401 "Not signed in."
        Just sid -> do
            mSess <- lookupBySessionId sm sid
            maybe (respond $ jsonError status401 "Not signed in.") k mSess

{- | Like 'requireSession' but for HTML browser pages: when the request carries
no *live* session, 302 to @\/_hub\/login@ carrying this URL as @next@, so the
user is returned here after Google login (rather than landing on a dead-end
page). A stale cookie that no longer resolves is treated as signed-out.
-}
requireSessionOrLogin ::
    SessionManager ->
    Request ->
    (Response -> IO ResponseReceived) ->
    (Session -> IO ResponseReceived) ->
    IO ResponseReceived
requireSessionOrLogin sm req respond k =
    case extractSessionId req of
        Nothing -> respond (loginRedirect req)
        Just sid -> do
            mSess <- lookupBySessionId sm sid
            maybe (respond (loginRedirect req)) k mSess

-- | A 302 to the login flow that remembers the current path+query as @next@.
loginRedirect :: Request -> Response
loginRedirect req =
    responseLBS
        status302
        [("Location", "/_hub/login?next=" <> urlEncode True dest)]
        ""
  where
    dest = rawPathInfo req <> rawQueryString req

-- | Extract session ID from the _sabela_session cookie.
extractSessionId :: Request -> Maybe SessionId
extractSessionId req = do
    cookieHeader <- lookup "Cookie" (requestHeaders req)
    let cookies = parseCookies cookieHeader
    val <- lookup "_sabela_session" cookies
    if BS.null val then Nothing else Just $ SessionId (TE.decodeUtf8 val)

parseCookies :: BS.ByteString -> [(BS.ByteString, BS.ByteString)]
parseCookies = map parsePair . B8.split ';'
  where
    parsePair s =
        let stripped = B8.dropWhile (== ' ') s
         in case B8.break (== '=') stripped of
                (name, rest)
                    | BS.null rest -> (name, "")
                    | otherwise -> (name, B8.drop 1 rest)

{- | Handle the OAuth callback from Google. The session cookie is set
'SameSite=Lax' so it survives the post-redirect navigation back to @/@.
The verified email is normalized at this identity boundary and gated on the
signup allowlist before any 'UserId' is minted.
-}
handleOAuthCallback ::
    SessionManager -> HC.Manager -> PendingStates -> HubConfig -> Application
handleOAuthCallback sm mgr states cfg req respond = do
    let params = queryString req
        mCode = join $ lookup "code" params
        mState = join $ lookup "state" params
    case (mCode, mState) of
        (Just code, Just state) -> do
            -- Validate CSRF state
            pending <- readTVarIO states
            now <- getCurrentTime
            let stateText = TE.decodeUtf8 state
                mEntry = Map.lookup stateText pending
                dest = case mEntry of
                    Just (_, Just n) -> n
                    _ -> "/"
                valid = case mEntry of
                    Just (t, _) -> diffUTCTime now t < csrfStateTtl
                    Nothing -> False
            if not valid
                then respond $ textResponse status400 "Invalid or expired state parameter"
                else do
                    atomically $ modifyTVar' states $ Map.delete stateText
                    result <-
                        try (exchangeCodeForEmail mgr cfg (TE.decodeUtf8 code)) ::
                            IO (Either SomeException (Either Text Text))
                    case result of
                        Right (Right email) -> do
                            let norm = normalizeEmail email
                            allowed <- checkAllowed (hcAllowlistFile cfg) norm
                            if not allowed
                                then
                                    respond $
                                        textResponse
                                            status403
                                            "This Sabela hub is invite-only. Ask the operator for access."
                                else do
                                    sid <- SessionId <$> generateRandomToken
                                    let uid = UserId norm
                                    -- Insert session placeholder and spawn task in background
                                    startSessionAsync sm sid uid
                                    let SessionId sidText = sid
                                    respond $
                                        responseLBS
                                            status302
                                            [ ("Location", TE.encodeUtf8 dest)
                                            ,
                                                ( "Set-Cookie"
                                                , "_sabela_session="
                                                    <> TE.encodeUtf8 sidText
                                                    <> "; Path=/; HttpOnly; SameSite=Lax"
                                                    <> secureAttr req
                                                    <> "; Max-Age=2592000"
                                                )
                                            ]
                                            ""
                        Right (Left err) ->
                            respond $ textResponse status500 ("OAuth error: " <> err)
                        Left e ->
                            respond $ textResponse status500 ("OAuth error: " <> T.pack (show e))
        _ ->
            respond $ textResponse status400 "Missing code or state parameter"

{- | Start the OAuth flow: mint a fresh CSRF @state@, prune expired entries,
record the new state (bound to a validated @next@ return path, if any), and 302
to Google's authorization endpoint. The matching 'handleOAuthCallback' validates
the state on return and redirects to that @next@.
-}
handleLogin :: PendingStates -> HubConfig -> Application
handleLogin states cfg req respond = do
    state <- generateRandomToken
    now <- getCurrentTime
    let mNext = safeNext (join (lookup "next" (queryString req)))
    atomically $
        modifyTVar' states $
            Map.insert state (now, mNext) . dropExpiredStates now
    let url = googleAuthUrl cfg state
    respond $
        responseLBS
            status302
            [("Location", TE.encodeUtf8 url)]
            ""

{- | Accept a @next@ redirect target only when it is a *local* path: it must
begin with a single @/@ and not @//@ or @/\\@, so it can never be coerced into
an open redirect to another origin.
-}
safeNext :: Maybe BS.ByteString -> Maybe Text
safeNext mbs = do
    bs <- mbs
    let t = TE.decodeUtf8 bs
    if "/" `T.isPrefixOf` t
        && not ("//" `T.isPrefixOf` t)
        && not ("/\\" `T.isPrefixOf` t)
        then Just t
        else Nothing

{- | A 302 to @/@ that clears the @_sabela_session@ cookie. The @Secure@
attribute is gated on @X-Forwarded-Proto@ so local plain-HTTP dev still works.
-}
logoutResponse :: Request -> Response
logoutResponse req =
    responseLBS
        status302
        [ ("Location", "/")
        ,
            ( "Set-Cookie"
            , "_sabela_session=; Path=/; HttpOnly; SameSite=Lax"
                <> secureAttr req
                <> "; Max-Age=0"
            )
        ]
        ""
