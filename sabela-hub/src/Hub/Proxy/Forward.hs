{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

proxyWithRetry :: HC.Manager -> Int -> TaskIp -> Application
proxyWithRetry mgr port ip req respond = go (0 :: Int)
  where
    maxRetries = 12 :: Int
    retryIntervalMicros = 5_000_000
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

popper :: Request -> IO BS.ByteString
popper = getRequestBodyChunk

consumeRequestBody :: Request -> IO [BS.ByteString]
consumeRequestBody req = go []
  where
    go acc = do
        chunk <- getRequestBodyChunk req
        if BS.null chunk
            then pure (reverse acc)
            else go (chunk : acc)

filterRequestHeaders :: [Header] -> [Header]
filterRequestHeaders =
    filter (\(n, _) -> not (isHopByHop n || n == hCookie || n == hAuthorization))

filterResponseHeaders :: [Header] -> [Header]
filterResponseHeaders = filter (not . isHopByHop . fst)

hardenResponseHeaders :: [Header] -> [Header]
hardenResponseHeaders hs =
    let base = map stripCookieDomain (filterResponseHeaders hs)
        hasCsp = any ((== "Content-Security-Policy") . fst) base
        hasNosniff = any ((== "X-Content-Type-Options") . fst) base
        framed
            | hasCsp = base
            | otherwise =
                filter ((/= "X-Frame-Options") . fst) base
                    ++ [("Content-Security-Policy", "frame-ancestors 'self'")]
     in if hasNosniff
            then framed
            else framed ++ [("X-Content-Type-Options", "nosniff")]

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
