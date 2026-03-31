{-# LANGUAGE OverloadedStrings #-}

module Sabela.LeanLsp (
    -- * Message framing
    sendLspMessage,
    readLspMessage,

    -- * JSON-RPC helpers
    makeRequest,
    makeNotification,
    responseResult,
    isNotification,
    notificationMethod,
    notificationParams,

    -- * LSP method constants
    mInitialize,
    mInitialized,
    mShutdown,
    mExit,
    mDidOpen,
    mDidChange,
    mPublishDiagnostics,
    mFileProgress,

    -- * Diagnostic types
    Diagnostic (..),
    DiagnosticSeverity (..),
    Position (..),
    Range (..),
) where

import Control.Monad (void)
import Data.Aeson (
    FromJSON (..),
    Value (..),
    eitherDecodeStrict',
    encode,
    object,
    withObject,
    (.:),
    (.:?),
    (.=),
 )
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import System.IO (Handle, hFlush)

sendLspMessage :: Handle -> Value -> IO ()
sendLspMessage h val = do
    let body = LBS.toStrict (encode val)
        header = "Content-Length: " <> BS8.pack (show (BS.length body)) <> "\r\n\r\n"
    BS.hPut h header
    BS.hPut h body
    hFlush h

readLspMessage :: Handle -> IO Value
readLspMessage h = do
    headerLine <- readLine h
    let contentLen = parseContentLength headerLine
    -- Skip remaining headers until empty line
    skipUntilEmpty h
    body <- BS.hGet h contentLen
    case eitherDecodeStrict' body of
        Left err -> error $ "LeanLsp: failed to decode JSON: " ++ err
        Right val -> pure val

readLine :: Handle -> IO BS.ByteString
readLine h = go []
  where
    go acc = do
        b <- BS.hGet h 1
        if b == "\n"
            then pure (BS.concat (reverse acc))
            else go (b : acc)

skipUntilEmpty :: Handle -> IO ()
skipUntilEmpty h = do
    line <- readLine h
    let stripped = BS8.dropWhileEnd (\c -> c == '\r' || c == '\n') line
    if BS.null stripped
        then pure ()
        else void $ skipUntilEmpty h

parseContentLength :: BS.ByteString -> Int
parseContentLength bs =
    case BS8.stripPrefix "Content-Length: " (BS8.dropWhileEnd (== '\r') bs) of
        Just rest -> read (BS8.unpack (BS8.takeWhile (/= '\r') rest))
        Nothing -> error $ "LeanLsp: expected Content-Length header, got: " ++ show bs

makeRequest :: Int -> Text -> Value -> Value
makeRequest reqId method params =
    object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "id" .= reqId
        , "method" .= method
        , "params" .= params
        ]

makeNotification :: Text -> Value -> Value
makeNotification method params =
    object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "method" .= method
        , "params" .= params
        ]

responseResult :: Value -> Maybe Value
responseResult (Object o) = KM.lookup "result" o
responseResult _ = Nothing

isNotification :: Value -> Bool
isNotification (Object o) =
    KM.member "method" o && not (KM.member "id" o)
isNotification _ = False

notificationMethod :: Value -> Maybe Text
notificationMethod (Object o) = case KM.lookup "method" o of
    Just (String m) -> Just m
    _ -> Nothing
notificationMethod _ = Nothing

notificationParams :: Value -> Maybe Value
notificationParams (Object o) = KM.lookup "params" o
notificationParams _ = Nothing

mInitialize :: Text
mInitialize = "initialize"

mInitialized :: Text
mInitialized = "initialized"

mShutdown :: Text
mShutdown = "shutdown"

mExit :: Text
mExit = "exit"

mDidOpen :: Text
mDidOpen = "textDocument/didOpen"

mDidChange :: Text
mDidChange = "textDocument/didChange"

mPublishDiagnostics :: Text
mPublishDiagnostics = "textDocument/publishDiagnostics"

mFileProgress :: Text
mFileProgress = "$/lean/fileProgress"

data Position = Position
    { posLine :: Int
    , posCharacter :: Int
    }
    deriving (Show, Eq)

instance FromJSON Position where
    parseJSON = withObject "Position" $ \o ->
        Position <$> o .: "line" <*> o .: "character"

data Range = Range
    { rangeStart :: Position
    , rangeEnd :: Position
    }
    deriving (Show, Eq)

instance FromJSON Range where
    parseJSON = withObject "Range" $ \o ->
        Range <$> o .: "start" <*> o .: "end"

data DiagnosticSeverity
    = DsError
    | DsWarning
    | DsInformation
    | DsHint
    deriving (Show, Eq)

instance FromJSON DiagnosticSeverity where
    parseJSON v = case v of
        Number 1 -> pure DsError
        Number 2 -> pure DsWarning
        Number 3 -> pure DsInformation
        Number 4 -> pure DsHint
        _ -> fail "unknown severity"

data Diagnostic = Diagnostic
    { diagRange :: Range
    , diagSeverity :: Maybe DiagnosticSeverity
    , diagMessage :: Text
    }
    deriving (Show, Eq)

instance FromJSON Diagnostic where
    parseJSON = withObject "Diagnostic" $ \o ->
        Diagnostic <$> o .: "range" <*> o .:? "severity" <*> o .: "message"
