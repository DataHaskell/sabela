{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Types describing what we send to the Anthropic Messages API: the
configuration, the request body, the content blocks that flow inside a
message (also shared by responses and streaming), and the in-flight
cancellation token consulted between streamed chunks and tool calls.
-}
module Sabela.Anthropic.Types.Request (
    -- * Configuration
    AnthropicConfig (..),

    -- * Cache control
    CacheControl (..),

    -- * Conversation roles and content
    Role (..),
    ContentBlock (..),

    -- * Request body
    MessagesRequest (..),
    SystemBlock (..),
    Message (..),
    ToolDef (..),

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

{- | Anthropic @cache_control@. @Ephemeral@ uses the default 5-minute TTL;
@EphemeralHour@ uses the 1-hour TTL (costs 2× on write but 0.1× on read, same
as 5-minute reads). Pick @EphemeralHour@ for prefixes that stay stable across
user idle gaps (system blocks, tool schemas, stable notebook prefix).
-}
data CacheControl = Ephemeral | EphemeralHour
    deriving (Show, Eq)

instance ToJSON CacheControl where
    toJSON Ephemeral = object ["type" .= ("ephemeral" :: Text)]
    toJSON EphemeralHour =
        object
            [ "type" .= ("ephemeral" :: Text)
            , "ttl" .= ("1h" :: Text)
            ]

data Role = RoleUser | RoleAssistant
    deriving (Show, Eq)

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

instance Show ContentBlock where
    show (TextBlock t) = "TextBlock " ++ show t
    show (ToolUseBlock tid name _) =
        "ToolUseBlock " ++ show tid ++ " " ++ show name
    show (ToolResultBlock tid isErr _) =
        "ToolResultBlock " ++ show tid ++ " isError=" ++ show isErr

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

data ToolDef = ToolDef
    { tdName :: Text
    , tdDescription :: Text
    , tdInputSchema :: Value
    , tdCacheControl :: Maybe CacheControl
    }

instance ToJSON ToolDef where
    toJSON td =
        object $
            [ "name" .= tdName td
            , "description" .= tdDescription td
            , "input_schema" .= tdInputSchema td
            ]
                ++ maybe [] (\cc -> ["cache_control" .= cc]) (tdCacheControl td)
