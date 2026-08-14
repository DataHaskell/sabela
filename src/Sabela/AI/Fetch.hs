{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | One bounded HTTP GET shared by the Hackage and GitHub clients: parse
the URL, send the caller's headers, map the status through a per-caller
ladder, and drain the body under a byte cap.
-}
module Sabela.AI.Fetch (
    FetchSpec (..),
    OverCap (..),
    fetchBounded,
    statusError,
    transportError,
    drainBounded,
    firstLine,
) where

import Control.Exception (SomeException, fromException, try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client (
    HttpException (..),
    Manager,
    Request,
    parseRequest,
    requestHeaders,
    responseBody,
    responseStatus,
    withResponse,
 )
import Network.HTTP.Types (Header, statusCode)

-- | Whether a body past the cap is an error or is cut at the cap.
data OverCap = FailOverCap Text | TruncateAtCap

-- | What varies between callers of one bounded GET.
data FetchSpec = FetchSpec
    { fsService :: Text
    , fsHeaders :: [Header]
    , fsCap :: Int
    , fsOverCap :: OverCap
    , fsStatus :: Int -> Maybe Text
    }

fetchBounded ::
    FetchSpec -> Manager -> Text -> IO (Either Text BL.ByteString)
fetchBounded fs mgr url = do
    eReq <-
        try (parseRequest (T.unpack url)) ::
            IO (Either SomeException Request)
    case eReq of
        Left _ ->
            pure (Left ("could not parse the " <> fsService fs <> " URL"))
        Right req0 -> do
            let req = req0{requestHeaders = fsHeaders fs}
            eRes <-
                try (withResponse req mgr readBody) ::
                    IO (Either SomeException (Either Text BL.ByteString))
            pure (either (Left . transportError (fsService fs)) id eRes)
  where
    readBody resp = case fsStatus fs (statusCode (responseStatus resp)) of
        Just e -> pure (Left e)
        Nothing -> drainBounded (fsCap fs) (fsOverCap fs) (responseBody resp)

-- | Statuses outside the caller's ladder and outside 2xx are a plain error.
statusError :: Text -> [(Int, Text)] -> Int -> Maybe Text
statusError service ladder sc
    | Just msg <- lookup sc ladder = Just msg
    | sc < 200 || sc >= 300 =
        Just (service <> " returned HTTP " <> T.pack (show sc))
    | otherwise = Nothing

-- | One line of the underlying failure, prefixed with the service's name.
transportError :: Text -> SomeException -> Text
transportError service e = service <> " request failed: " <> reason
  where
    reason = case fromException e of
        Just (HttpExceptionRequest _ content) ->
            firstLine (T.pack (show content))
        Just (InvalidUrlException _ why) -> T.pack why
        Nothing -> firstLine (T.pack (show e))

-- | Reads chunks until the end or the cap, honouring the over-cap policy.
drainBounded ::
    Int -> OverCap -> IO BS.ByteString -> IO (Either Text BL.ByteString)
drainBounded cap overCap readChunk = go [] 0
  where
    go acc n = do
        chunk <- readChunk
        step acc n chunk
    step acc n chunk
        | BS.null chunk = pure (Right (BL.fromChunks (reverse acc)))
        | n + BS.length chunk > cap = pure (past acc n chunk)
        | otherwise = go (chunk : acc) (n + BS.length chunk)
    past acc n chunk = case overCap of
        FailOverCap e -> Left e
        TruncateAtCap ->
            Right (BL.fromChunks (reverse (BS.take (cap - n) chunk : acc)))

firstLine :: Text -> Text
firstLine t = case T.lines t of
    (l : _) -> l
    [] -> t
