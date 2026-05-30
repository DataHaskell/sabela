{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | The reverse-proxy engine: forward a WAI request to a per-user backend
container, retrying while it boots, and harden the response on the way back.
-}
module Hub.Proxy.Forward (
    proxyWithRetry,
    proxyRequest,
    buildBackendRequest,
    consumeRequestBody,
    filterRequestHeaders,
    filterResponseHeaders,
    hardenResponseHeaders,
    stripCookieDomain,
    isHopByHop,
    isConnectionError,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (byteString)
import qualified Data.ByteString.Char8 as B8
import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text as T
import Hub.Pages (textResponse)
import Hub.Types (TaskIp (..))
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types
import Network.Wai

{- | Proxy with retry — handles the case where the Sabela task is RUNNING
but the server hasn't started accepting connections yet.
-}
proxyWithRetry :: HC.Manager -> Int -> TaskIp -> Application
proxyWithRetry mgr port ip req respond = go (0 :: Int)
  where
    maxRetries = 12 :: Int
    retryIntervalMicros = 5_000_000 -- 5s × maxRetries ≈ 60s budget
    go n = do
        result <- try $ proxyRequest mgr port ip req respond
        case result of
            Right val -> pure val
            Left (e :: SomeException)
                | n < maxRetries && isConnectionError e -> do
                    threadDelay retryIntervalMicros
                    go (n + 1)
                | otherwise ->
                    respond $
                        textResponse status502 "Notebook is starting up. Please refresh in a moment."

isConnectionError :: SomeException -> Bool
isConnectionError e = "ConnectionFailure" `T.isInfixOf` T.pack (show e)

-- | Forward a request to the backend Sabela task.
proxyRequest :: HC.Manager -> Int -> TaskIp -> Application
proxyRequest mgr port (TaskIp ip) req respond = do
    backendReq <- buildBackendRequest port ip req
    HC.withResponse backendReq mgr $ \backendResp -> do
        let status' = HC.responseStatus backendResp
            headers' = hardenResponseHeaders (HC.responseHeaders backendResp)
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

{- | Build the upstream request without buffering the body. The body is
streamed chunk-by-chunk from the WAI 'Request' via 'HC.RequestBodyStreamChunked',
so a large upload doesn't allocate the whole payload twice (once into a
chunk list, once into a 'BS.concat').
-}
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
    pure
        initReq
            { HC.method = requestMethod req
            , HC.requestHeaders = filterRequestHeaders (requestHeaders req)
            , HC.requestBody =
                HC.RequestBodyStreamChunked (\writeChunk -> writeChunk (popper req))
            , HC.responseTimeout = HC.responseTimeoutNone
            }

{- | Adapter from WAI's 'getRequestBodyChunk' (returns 'BS.empty' at EOF)
to http-client's 'GivesPopper' (returns 'BS.empty' at EOF). The two
share the same convention, so the popper is just the chunk-getter.
-}
popper :: Request -> IO BS.ByteString
popper = getRequestBodyChunk

{- | Drain the request body into a chunk list. Retained because the
test suite uses it; production now streams directly via 'popper'.
-}
consumeRequestBody :: Request -> IO [BS.ByteString]
consumeRequestBody req = go []
  where
    go acc = do
        chunk <- getRequestBodyChunk req
        if BS.null chunk
            then pure (reverse acc)
            else go (chunk : acc)

{- | Strip hop-by-hop headers AND the request's credential headers (@Cookie@,
@Authorization@) before forwarding to the per-user backend container. The
container runs untrusted notebook code and the backend has no auth of its own,
so it must never receive the hub session cookie — otherwise a cell could harvest
the caller's hub credential and hijack their session.
-}
filterRequestHeaders :: [Header] -> [Header]
filterRequestHeaders =
    filter (\(n, _) -> not (isHopByHop n || n == hCookie || n == hAuthorization))

filterResponseHeaders :: [Header] -> [Header]
filterResponseHeaders = filter (not . isHopByHop . fst)

{- | Filter hop-by-hop headers, drop cookie @Domain@ attributes (host-only, so
one sandbox's cookies can't leak across @/s/<slug>@ paths), and add
clickjacking/sniffing protection. Never weakens an existing app CSP: the
@frame-ancestors@ default is added only when the backend sends no CSP, and
@nosniff@ only when absent. Required once public shares (Phase 3) are iframed;
harmless for authed proxying.
-}
hardenResponseHeaders :: [Header] -> [Header]
hardenResponseHeaders hs =
    let base = map stripCookieDomain (filterResponseHeaders hs)
        hasCsp = any ((== "Content-Security-Policy") . fst) base
        hasNosniff = any ((== "X-Content-Type-Options") . fst) base
        framed
            | hasCsp = base -- the app controls its own framing/CSP
            | otherwise =
                filter ((/= "X-Frame-Options") . fst) base
                    ++ [("Content-Security-Policy", "frame-ancestors 'self'")]
     in if hasNosniff
            then framed
            else framed ++ [("X-Content-Type-Options", "nosniff")]

-- | Remove the @Domain=@ attribute from a @Set-Cookie@ so it stays host-only.
stripCookieDomain :: Header -> Header
stripCookieDomain (name, val)
    | name == "Set-Cookie" =
        (name, BS.intercalate "; " (filter (not . isDomain) segments))
    | otherwise = (name, val)
  where
    segments = map (B8.dropWhile (== ' ')) (B8.split ';' val)
    isDomain seg = "domain=" `BS.isPrefixOf` B8.map toLower seg

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
