{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Sabela.Anthropic.Types (
    -- * Configuration
    AnthropicConfig (..),

    -- * Request types
    MessagesRequest (..),
    SystemBlock (..),
    CacheControl (..),
    Message (..),
    Role (..),
    ContentBlock (..),
    ToolDef (..),

    -- * Response types
    MessageResponse (..),
    Usage (..),
    StopReason (..),

    -- * Streaming
    StreamEvent (..),
    ContentDelta (..),
    MessageDelta (..),

    -- * Cancellation
    CancelToken (..),
    newCancelToken,
    cancel,
    isCancelled,
) where

import Data.Aeson (
    FromJSON (..),
    ToJSON (..),
    Value (..),
    object,
    withObject,
    withText,
    (.:),
    (.:?),
    (.=),
 )
import qualified Data.Aeson.Types as Aeson
import Data.IORef (IORef, atomicWriteIORef, newIORef, readIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | Configuration for the Anthropic API client.
data AnthropicConfig = AnthropicConfig
    { acApiKey :: Text
    , acModel :: Text
    , acBaseUrl :: Text
    }

-- | Cancellation token checked between streaming chunks and tool calls.
newtype CancelToken = CancelToken (IORef Bool)

newCancelToken :: IO CancelToken
newCancelToken = CancelToken <$> newIORef False

cancel :: CancelToken -> IO ()
cancel (CancelToken ref) = atomicWriteIORef ref True

isCancelled :: CancelToken -> IO Bool
isCancelled (CancelToken ref) = readIORef ref

------------------------------------------------------------------------
-- Request types
------------------------------------------------------------------------

data MessagesRequest = MessagesRequest
    { mrModel :: Text
    , mrMaxTokens :: Int
    , mrSystem :: [SystemBlock]
    , mrMessages :: [Message]
    , mrTools :: [ToolDef]
    , mrStream :: Bool
    }

instance ToJSON MessagesRequest where
    toJSON r =
        object $
            [ "model" .= mrModel r
            , "max_tokens" .= mrMaxTokens r
            , "messages" .= mrMessages r
            , "stream" .= mrStream r
            ]
                ++ (["system" .= mrSystem r | not (null (mrSystem r))])
                ++ (["tools" .= mrTools r | not (null (mrTools r))])

data SystemBlock = SystemBlock
    { sbkText :: Text
    , sbkCacheControl :: Maybe CacheControl
    }

instance ToJSON SystemBlock where
    toJSON sb =
        object $
            ["type" .= ("text" :: Text), "text" .= sbkText sb]
                ++ maybe [] (\cc -> ["cache_control" .= cc]) (sbkCacheControl sb)

data CacheControl = Ephemeral

instance ToJSON CacheControl where
    toJSON Ephemeral = object ["type" .= ("ephemeral" :: Text)]

data Message = Message
    { msgRole :: Role
    , msgContent :: [ContentBlock]
    }

instance ToJSON Message where
    toJSON m =
        object
            [ "role" .= msgRole m
            , "content" .= msgContent m
            ]

instance FromJSON Message where
    parseJSON = withObject "Message" $ \o ->
        Message <$> o .: "role" <*> o .: "content"

data Role = RoleUser | RoleAssistant
    deriving (Show, Eq, Generic)

instance ToJSON Role where
    toJSON RoleUser = "user"
    toJSON RoleAssistant = "assistant"

instance FromJSON Role where
    parseJSON = withText "Role" $ \case
        "user" -> pure RoleUser
        "assistant" -> pure RoleAssistant
        other -> fail $ "Unknown role: " ++ T.unpack other

data ContentBlock
    = TextBlock Text
    | ToolUseBlock
        { tubId :: Text
        , tubName :: Text
        , tubInput :: Value
        }
    | ToolResultBlock
        { trbToolUseId :: Text
        , trbIsError :: Bool
        , trbContent :: [ContentBlock]
        }

instance ToJSON ContentBlock where
    toJSON (TextBlock t) =
        object ["type" .= ("text" :: Text), "text" .= t]
    toJSON (ToolUseBlock tid name input) =
        object
            [ "type" .= ("tool_use" :: Text)
            , "id" .= tid
            , "name" .= name
            , "input" .= input
            ]
    toJSON (ToolResultBlock tid isErr content) =
        object $
            [ "type" .= ("tool_result" :: Text)
            , "tool_use_id" .= tid
            , "content" .= content
            ]
                ++ ["is_error" .= True | isErr]

instance FromJSON ContentBlock where
    parseJSON = withObject "ContentBlock" $ \o -> do
        typ <- o .: "type" :: Aeson.Parser Text
        case typ of
            "text" -> TextBlock <$> o .: "text"
            "tool_use" ->
                ToolUseBlock
                    <$> o .: "id"
                    <*> o .: "name"
                    <*> o .: "input"
            "tool_result" ->
                ToolResultBlock
                    <$> o .: "tool_use_id"
                    <*> fmap (fromMaybe False) (o .:? "is_error")
                    <*> o .: "content"
            other -> fail $ "Unknown content block type: " ++ T.unpack other

data ToolDef = ToolDef
    { tdName :: Text
    , tdDescription :: Text
    , tdInputSchema :: Value
    }

instance ToJSON ToolDef where
    toJSON td =
        object
            [ "name" .= tdName td
            , "description" .= tdDescription td
            , "input_schema" .= tdInputSchema td
            ]

------------------------------------------------------------------------
-- Response types
------------------------------------------------------------------------

data MessageResponse = MessageResponse
    { mrsId :: Text
    , mrsContent :: [ContentBlock]
    , mrsStopReason :: Maybe StopReason
    , mrsUsage :: Maybe Usage
    }
    deriving (Show)

instance FromJSON MessageResponse where
    parseJSON = withObject "MessageResponse" $ \o ->
        MessageResponse
            <$> o .: "id"
            <*> o .: "content"
            <*> o .:? "stop_reason"
            <*> o .:? "usage"

instance Show ContentBlock where
    show (TextBlock t) = "TextBlock " ++ show t
    show (ToolUseBlock tid name _) =
        "ToolUseBlock " ++ show tid ++ " " ++ show name
    show (ToolResultBlock tid isErr _) =
        "ToolResultBlock " ++ show tid ++ " isError=" ++ show isErr

data StopReason = SREndTurn | SRToolUse | SRMaxTokens
    deriving (Show, Eq)

instance FromJSON StopReason where
    parseJSON = withText "StopReason" $ \case
        "end_turn" -> pure SREndTurn
        "tool_use" -> pure SRToolUse
        "max_tokens" -> pure SRMaxTokens
        other -> fail $ "Unknown stop_reason: " ++ T.unpack other

instance ToJSON StopReason where
    toJSON SREndTurn = "end_turn"
    toJSON SRToolUse = "tool_use"
    toJSON SRMaxTokens = "max_tokens"

data Usage = Usage
    { uInputTokens :: Int
    , uOutputTokens :: Int
    , uCacheCreationInputTokens :: Maybe Int
    , uCacheReadInputTokens :: Maybe Int
    }
    deriving (Show, Generic)

instance FromJSON Usage where
    parseJSON = withObject "Usage" $ \o ->
        Usage
            <$> o .: "input_tokens"
            <*> o .: "output_tokens"
            <*> o .:? "cache_creation_input_tokens"
            <*> o .:? "cache_read_input_tokens"

instance ToJSON Usage where
    toJSON u =
        object
            [ "input_tokens" .= uInputTokens u
            , "output_tokens" .= uOutputTokens u
            ]

------------------------------------------------------------------------
-- Streaming event types
------------------------------------------------------------------------

data StreamEvent
    = SEMessageStart MessageResponse
    | SEContentBlockStart Int ContentBlock
    | SEContentBlockDelta Int ContentDelta
    | SEContentBlockStop Int
    | SEMessageDelta MessageDelta
    | SEMessageStop
    | SEPing
    | SEError Text
    deriving (Show)

data ContentDelta
    = TextDelta Text
    | InputJsonDelta Text
    deriving (Show)

data MessageDelta = MessageDelta
    { mdStopReason :: Maybe StopReason
    , mdUsage :: Maybe Usage
    }
    deriving (Show)

instance FromJSON StreamEvent where
    parseJSON = withObject "StreamEvent" $ \o -> do
        typ <- o .: "type" :: Aeson.Parser Text
        case typ of
            "message_start" -> SEMessageStart <$> o .: "message"
            "content_block_start" ->
                SEContentBlockStart <$> o .: "index" <*> o .: "content_block"
            "content_block_delta" -> do
                idx <- o .: "index"
                delta <- o .: "delta"
                SEContentBlockDelta idx <$> parseDelta delta
            "content_block_stop" -> SEContentBlockStop <$> o .: "index"
            "message_delta" ->
                SEMessageDelta <$> (o .: "delta" >>= parseMessageDelta)
            "message_stop" -> pure SEMessageStop
            "ping" -> pure SEPing
            "error" -> do
                errObj <- o .: "error"
                SEError <$> (errObj .: "message" :: Aeson.Parser Text)
            other -> fail $ "Unknown stream event type: " ++ T.unpack other
      where
        parseDelta = withObject "ContentDelta" $ \d -> do
            dType <- d .: "type" :: Aeson.Parser Text
            case dType of
                "text_delta" -> TextDelta <$> d .: "text"
                "input_json_delta" -> InputJsonDelta <$> d .: "partial_json"
                other -> fail $ "Unknown delta type: " ++ T.unpack other
        parseMessageDelta = withObject "MessageDelta" $ \d ->
            MessageDelta <$> d .:? "stop_reason" <*> d .:? "usage"
