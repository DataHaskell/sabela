{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sabela.LLM.Anthropic.Client (
    streamMessages,
    defaultConfig,
    module Sabela.Anthropic.Types,
) where

import Control.Exception (SomeException, try)
import Control.Monad (unless, when)
import Data.Aeson (Value (String), eitherDecodeStrict, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client (
    Manager,
    Request (..),
    RequestBody (..),
    Response (..),
    parseRequest,
    withResponse,
 )
import Network.HTTP.Types (statusCode)
import Sabela.Anthropic.Types

defaultConfig :: Text -> AnthropicConfig
defaultConfig apiKey =
    AnthropicConfig
        { acApiKey = apiKey
        , acModel = "claude-sonnet-4-20250514"
        , acBaseUrl = "https://api.anthropic.com"
        }

streamMessages ::
    Manager ->
    AnthropicConfig ->
    MessagesRequest ->
    CancelToken ->
    (StreamEvent -> IO ()) ->
    IO (Either Text MessageResponse)
streamMessages mgr cfg req cancelTok onEvent = do
    eResult <- try $ do
        initReq <- parseRequest (T.unpack (acBaseUrl cfg) ++ "/v1/messages")
        let httpReq =
                initReq
                    { method = "POST"
                    , requestBody = RequestBodyLBS (encode req)
                    , requestHeaders =
                        [ ("x-api-key", TE.encodeUtf8 (acApiKey cfg))
                        , ("anthropic-version", "2023-06-01")
                        , ("content-type", "application/json")
                        , ("accept", "text/event-stream")
                        ]
                    }
        responseRef <- newIORef Nothing
        withResponse httpReq mgr $ \resp -> do
            let sc = statusCode (responseStatus resp)
            if sc /= 200
                then do
                    body <- drainBody (responseBody resp)
                    pure $
                        Left $
                            "Anthropic API error (HTTP "
                                <> T.pack (show sc)
                                <> "): "
                                <> TE.decodeUtf8 body
                else do
                    parseSSEStream (responseBody resp) cancelTok onEvent responseRef
                    mResp <- readIORef responseRef
                    case mResp of
                        Just r -> pure (Right r)
                        Nothing -> pure (Left "Stream ended without message_start")
    case eResult of
        Left (e :: SomeException) -> pure (Left (T.pack (show e)))
        Right r -> pure r

drainBody :: IO BS.ByteString -> IO BS.ByteString
drainBody readChunk = do
    chunks <- go []
    pure (BS.concat (reverse chunks))
  where
    go acc = do
        chunk <- readChunk
        if BS.null chunk
            then pure acc
            else go (chunk : acc)

parseSSEStream ::
    IO BS.ByteString ->
    CancelToken ->
    (StreamEvent -> IO ()) ->
    IORef (Maybe MessageResponse) ->
    IO ()
parseSSEStream readChunk cancelTok onEvent responseRef = do
    bufRef <- newIORef BS.empty
    go bufRef
  where
    go bufRef = do
        cancelled <- isCancelled cancelTok
        when cancelled $ pure ()
        unless cancelled $ do
            chunk <- readChunk
            if BS.null chunk
                then do
                    buf <- readIORef bufRef
                    unless (BS.null (BS8.strip buf)) $
                        processFrame buf onEvent responseRef
                else do
                    buf <- readIORef bufRef
                    let combined = buf <> chunk
                    remaining <- processBuffer combined onEvent responseRef
                    modifyIORef' bufRef (const remaining)
                    go bufRef

processBuffer ::
    BS.ByteString ->
    (StreamEvent -> IO ()) ->
    IORef (Maybe MessageResponse) ->
    IO BS.ByteString
processBuffer buf onEvent responseRef = do
    case BS8.breakSubstring "\n\n" buf of
        (_, rest) | BS.null rest -> pure buf
        (frame, rest) -> do
            processFrame frame onEvent responseRef
            let remaining = BS.drop 2 rest
            processBuffer remaining onEvent responseRef

processFrame ::
    BS.ByteString ->
    (StreamEvent -> IO ()) ->
    IORef (Maybe MessageResponse) ->
    IO ()
processFrame frame onEvent responseRef = do
    let ls = BS8.lines frame
        eventType = extractField "event:" ls
        dataLines = [BS.drop 6 l | l <- ls, "data: " `BS.isPrefixOf` l]
    unless (null dataLines) $ do
        let jsonBs = BS.intercalate "\n" dataLines
        case eitherDecodeStrict jsonBs of
            Left _ -> pure ()
            Right ev -> do
                case ev of
                    SEMessageStart resp ->
                        modifyIORef' responseRef (const (Just resp))
                    SEMessageDelta md -> do
                        modifyIORef' responseRef $ fmap $ \r ->
                            r
                                { mrsStopReason =
                                    mdStopReason md
                                        `orElse` mrsStopReason r
                                , mrsUsage =
                                    mdUsage md `orElse` mrsUsage r
                                }
                    SEContentBlockStart idx cb -> do
                        modifyIORef' responseRef $ fmap $ \r ->
                            r{mrsContent = setAt idx cb (mrsContent r)}
                    SEContentBlockDelta idx delta -> do
                        modifyIORef' responseRef $ fmap $ \r ->
                            r{mrsContent = applyDelta idx delta (mrsContent r)}
                    _ -> pure ()
                handleEvent eventType ev onEvent
  where
    orElse (Just x) _ = Just x
    orElse Nothing y = y

extractField :: BS.ByteString -> [BS.ByteString] -> Maybe BS.ByteString
extractField prefix ls =
    case [BS.drop (BS.length prefix + 1) l | l <- ls, prefix `BS.isPrefixOf` l] of
        (x : _) -> Just (BS8.strip x)
        [] -> Nothing

handleEvent ::
    Maybe BS.ByteString -> StreamEvent -> (StreamEvent -> IO ()) -> IO ()
handleEvent _ ev onEvent = onEvent ev

setAt :: Int -> ContentBlock -> [ContentBlock] -> [ContentBlock]
setAt idx cb xs
    | idx < length xs = take idx xs ++ [cb] ++ drop (idx + 1) xs
    | otherwise = xs ++ [cb]

applyDelta :: Int -> ContentDelta -> [ContentBlock] -> [ContentBlock]
applyDelta idx delta blocks =
    [ if i == idx then merge b delta else b
    | (i, b) <- zip [0 ..] blocks
    ]
  where
    merge (TextBlock t) (TextDelta dt) =
        let !t' = t <> dt in TextBlock t'
    merge (ToolUseBlock tid name (String jsonSoFar)) (InputJsonDelta dj) =
        let !merged = jsonSoFar <> dj in ToolUseBlock tid name (String merged)
    merge (ToolUseBlock tid name _) (InputJsonDelta dj) =
        ToolUseBlock tid name (String dj)
    merge block _ = block
