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

-- | Pending OAuth states (CSRF protection). Maps state token → creation time.
type PendingStates = TVar (Map.Map Text UTCTime)

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
dropExpiredStates :: UTCTime -> Map.Map Text UTCTime -> Map.Map Text UTCTime
dropExpiredStates now = Map.filter ((< csrfStateTtl) . diffUTCTime now)

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
'SameSite=Lax' so it survives the post-redirect navigation back to @/@;
'Strict' would withhold it and bounce the user to login.
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
                valid = case Map.lookup stateText pending of
                    Just t -> diffUTCTime now t < csrfStateTtl
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
                            sid <- SessionId <$> generateRandomToken
                            let uid = UserId email
                            -- Insert session placeholder and spawn task in background
                            startSessionAsync sm sid uid
                            let SessionId sidText = sid
                            respond $
                                responseLBS
                                    status302
                                    [ ("Location", "/")
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
record the new state, and 302 to Google's authorization endpoint. The matching
'handleOAuthCallback' validates the state on return.
-}
handleLogin :: PendingStates -> HubConfig -> Application
handleLogin states cfg _req respond = do
    state <- generateRandomToken
    now <- getCurrentTime
    atomically $
        modifyTVar' states $
            Map.insert state now . dropExpiredStates now
    let url = googleAuthUrl cfg state
    respond $
        responseLBS
            status302
            [("Location", TE.encodeUtf8 url)]
            ""

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
