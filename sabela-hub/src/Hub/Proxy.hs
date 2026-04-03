{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hub.Proxy (
    hubApp,
    extractSessionId,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (
    TVar,
    atomically,
    modifyTVar',
    newTVarIO,
    readTVarIO,
 )
import Control.Exception (SomeException, try)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (byteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Hub.OAuth (exchangeCodeForEmail, generateRandomToken, googleAuthUrl)
import Hub.Session (
    SessionManager (..),
    getOrCreateSession,
    insertSession,
    lookupBySessionId,
    startSessionAsync,
 )
import Hub.Types
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types
import Network.Wai

-- | Pending OAuth states (CSRF protection). Maps state token → creation time.
type PendingStates = TVar (Map.Map Text UTCTime)

-- | Create the WAI application. Call once at startup.
hubApp :: SessionManager -> HC.Manager -> IO Application
hubApp sm mgr = do
    states <- newTVarIO Map.empty
    pure $ hubApp' sm mgr states

hubApp' :: SessionManager -> HC.Manager -> PendingStates -> Application
hubApp' sm mgr states req respond =
    let path = rawPathInfo req
        cfg = smConfig sm
     in case path of
            "/_hub/health" ->
                respond $ textResponse status200 "ok"
            "/_hub/login" -> do
                state <- generateRandomToken
                now <- getCurrentTime
                atomically $ modifyTVar' states $ Map.insert state now
                let url = googleAuthUrl cfg state
                respond $
                    responseLBS
                        status302
                        [("Location", TE.encodeUtf8 url)]
                        ""
            "/_hub/oauth/callback" ->
                handleOAuthCallback sm mgr states cfg req respond
            "/_hub/logout" ->
                respond $
                    responseLBS
                        status302
                        [ ("Location", "/")
                        , ("Set-Cookie", "_sabela_session=; Path=/; HttpOnly; SameSite=Lax; Max-Age=0")
                        ]
                        ""
            _ ->
                case extractSessionId req of
                    Nothing ->
                        respond loginPage
                    Just sid -> do
                        mSess <- lookupBySessionId sm sid
                        case mSess of
                            Nothing ->
                                respond loginPage
                            Just sess ->
                                case sessionState sess of
                                    SReady ip ->
                                        proxyWithRetry mgr (hcBackendPort cfg) ip req respond
                                    SStarting ->
                                        respond startingPage
                                    SStopping ->
                                        respond loginPage

-- | Handle the OAuth callback from Google.
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
                    Just t -> diffUTCTime now t < 600 -- 10 min expiry
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
                                            <> "; Path=/; HttpOnly; SameSite=Lax; Max-Age=2592000"
                                        )
                                    ]
                                    ""
                        Right (Left err) ->
                            respond $ textResponse status500 ("OAuth error: " <> err)
                        Left e ->
                            respond $ textResponse status500 ("OAuth error: " <> T.pack (show e))
        _ ->
            respond $ textResponse status400 "Missing code or state parameter"
  where
    join (Just (Just x)) = Just x
    join _ = Nothing

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

{- | Proxy with retry — handles the case where the Sabela task is RUNNING
but the server hasn't started accepting connections yet.
-}
proxyWithRetry :: HC.Manager -> Int -> Text -> Application
proxyWithRetry mgr port ip req respond = go (0 :: Int)
  where
    maxRetries = 12 -- 60 seconds at 5s intervals
    go n = do
        result <- try $ proxyRequest mgr port ip req respond
        case result of
            Right val -> pure val
            Left (e :: SomeException)
                | n < maxRetries && isConnectionError e -> do
                    threadDelay 5000000
                    go (n + 1)
                | otherwise ->
                    respond $
                        textResponse status502 "Notebook is starting up. Please refresh in a moment."

isConnectionError :: SomeException -> Bool
isConnectionError e = "ConnectionFailure" `T.isInfixOf` T.pack (show e)

-- | Forward a request to the backend Sabela task.
proxyRequest :: HC.Manager -> Int -> Text -> Application
proxyRequest mgr port ip req respond = do
    backendReq <- buildBackendRequest port ip req
    HC.withResponse backendReq mgr $ \backendResp -> do
        let status' = HC.responseStatus backendResp
            headers' = filterResponseHeaders (HC.responseHeaders backendResp)
            body = HC.responseBody backendResp
        respond $ responseStream status' headers' $ \write flush -> do
            let loop = do
                    chunk <- body
                    if BS.null chunk
                        then flush
                        else do
                            write (byteString chunk)
                            flush
                            loop
            loop

buildBackendRequest :: Int -> Text -> Request -> IO HC.Request
buildBackendRequest port ip req = do
    let url =
            "http://"
                ++ T.unpack ip
                ++ ":"
                ++ show port
                ++ B8.unpack (rawPathInfo req)
                ++ B8.unpack (rawQueryString req)
    initReq <- HC.parseRequest url
    bodyChunks <- consumeRequestBody req
    let bodyBs = BS.concat bodyChunks
    pure
        initReq
            { HC.method = requestMethod req
            , HC.requestHeaders = filterRequestHeaders (requestHeaders req)
            , HC.requestBody = HC.RequestBodyBS bodyBs
            , HC.responseTimeout = HC.responseTimeoutNone
            }

consumeRequestBody :: Request -> IO [BS.ByteString]
consumeRequestBody req = go []
  where
    go acc = do
        chunk <- getRequestBodyChunk req
        if BS.null chunk
            then pure (reverse acc)
            else go (chunk : acc)

filterRequestHeaders :: [Header] -> [Header]
filterRequestHeaders = filter (not . isHopByHop . fst)

filterResponseHeaders :: [Header] -> [Header]
filterResponseHeaders = filter (not . isHopByHop . fst)

isHopByHop :: HeaderName -> Bool
isHopByHop h =
    h
        `elem` [ hConnection
               , "Transfer-Encoding"
               , "Keep-Alive"
               , "Proxy-Authenticate"
               , "Proxy-Authorization"
               , "TE"
               , "Trailer"
               , "Upgrade"
               ]

textResponse :: Status -> Text -> Response
textResponse s msg =
    responseLBS
        s
        [(hContentType, "text/plain")]
        (BL.fromStrict (TE.encodeUtf8 msg))

loginPage :: Response
loginPage =
    responseLBS
        status200
        [(hContentType, "text/html; charset=utf-8")]
        (BL.fromStrict (TE.encodeUtf8 loginHtml))

startingPage :: Response
startingPage =
    responseLBS
        status200
        [(hContentType, "text/html; charset=utf-8")]
        (BL.fromStrict (TE.encodeUtf8 startingHtml))

startingHtml :: Text
startingHtml =
    T.unlines
        [ "<!DOCTYPE html>"
        , "<html lang=\"en\">"
        , "<head>"
        , "<meta charset=\"UTF-8\">"
        , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">"
        , ""
        , "<title>Sabela - Starting</title>"
        , "<style>"
        , "  * { margin: 0; padding: 0; box-sizing: border-box; }"
        , "  body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;"
        , "    background: #0f1117; color: #e1e4e8; display: flex; align-items: center;"
        , "    justify-content: center; min-height: 100vh; }"
        , "  .card { background: #1c1f26; border: 1px solid #2d3139; border-radius: 12px;"
        , "    padding: 40px; width: 380px; text-align: center; }"
        , "  h1 { font-size: 24px; margin-bottom: 8px; }"
        , "  p { color: #8b949e; font-size: 14px; margin-bottom: 16px; }"
        , "  .spinner { display: inline-block; width: 24px; height: 24px;"
        , "    border: 3px solid #2d3139; border-top-color: #58a6ff;"
        , "    border-radius: 50%; animation: spin 1s linear infinite; }"
        , "  @keyframes spin { to { transform: rotate(360deg); } }"
        , "</style>"
        , "</head>"
        , "<body>"
        , "<div class=\"card\">"
        , "  <h1>Sabela</h1>"
        , "  <p>Starting your notebook environment...</p>"
        , "  <div class=\"spinner\"></div>"
        , "  <p id=\"status\" style=\"margin-top:16px;font-size:12px;\">This may take a couple of minutes.</p>"
        , "  <script>"
        , "    async function poll() {"
        , "      try {"
        , "        const r = await fetch('/_hub/health');"
        , "        if (r.ok) {"
        , "          const r2 = await fetch('/api/notebook');"
        , "          if (r2.ok) { window.location.reload(); return; }"
        , "        }"
        , "      } catch(e) {}"
        , "      setTimeout(poll, 5000);"
        , "    }"
        , "    setTimeout(poll, 5000);"
        , "  </script>"
        , "</div>"
        , "</body>"
        , "</html>"
        ]

loginHtml :: Text
loginHtml =
    T.unlines
        [ "<!DOCTYPE html>"
        , "<html lang=\"en\">"
        , "<head>"
        , "<meta charset=\"UTF-8\">"
        , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">"
        , "<title>Sabela</title>"
        , "<style>"
        , "  * { margin: 0; padding: 0; box-sizing: border-box; }"
        , "  body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;"
        , "    background: #0f1117; color: #e1e4e8; display: flex; align-items: center;"
        , "    justify-content: center; min-height: 100vh; }"
        , "  .card { background: #1c1f26; border: 1px solid #2d3139; border-radius: 12px;"
        , "    padding: 40px; width: 380px; text-align: center; }"
        , "  h1 { font-size: 24px; margin-bottom: 8px; }"
        , "  p { color: #8b949e; font-size: 14px; margin-bottom: 24px; }"
        , "  .google-btn { display: inline-flex; align-items: center; gap: 10px;"
        , "    padding: 10px 24px; background: #fff; color: #3c4043; border: 1px solid #dadce0;"
        , "    border-radius: 6px; font-size: 14px; font-weight: 500; text-decoration: none;"
        , "    cursor: pointer; transition: box-shadow 0.2s; }"
        , "  .google-btn:hover { box-shadow: 0 1px 3px rgba(0,0,0,0.3); }"
        , "  .google-btn svg { width: 18px; height: 18px; }"
        , "</style>"
        , "</head>"
        , "<body>"
        , "<div class=\"card\">"
        , "  <h1>Sabela</h1>"
        , "  <p>Sign in to start a notebook session.</p>"
        , "  <a href=\"/_hub/login\" class=\"google-btn\">"
        , "    <svg viewBox=\"0 0 24 24\"><path fill=\"#4285F4\" d=\"M22.56 12.25c0-.78-.07-1.53-.2-2.25H12v4.26h5.92a5.06 5.06 0 0 1-2.2 3.32v2.77h3.57c2.08-1.92 3.28-4.74 3.28-8.1z\"/><path fill=\"#34A853\" d=\"M12 23c2.97 0 5.46-.98 7.28-2.66l-3.57-2.77c-.98.66-2.23 1.06-3.71 1.06-2.86 0-5.29-1.93-6.16-4.53H2.18v2.84C3.99 20.53 7.7 23 12 23z\"/><path fill=\"#FBBC05\" d=\"M5.84 14.09c-.22-.66-.35-1.36-.35-2.09s.13-1.43.35-2.09V7.07H2.18C1.43 8.55 1 10.22 1 12s.43 3.45 1.18 4.93l2.85-2.22.81-.62z\"/><path fill=\"#EA4335\" d=\"M12 5.38c1.62 0 3.06.56 4.21 1.64l3.15-3.15C17.45 2.09 14.97 1 12 1 7.7 1 3.99 3.47 2.18 7.07l3.66 2.84c.87-2.6 3.3-4.53 6.16-4.53z\"/></svg>"
        , "    Sign in with Google"
        , "  </a>"
        , "</div>"
        , "</body>"
        , "</html>"
        ]
