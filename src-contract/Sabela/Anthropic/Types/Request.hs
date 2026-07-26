{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Sabela.Anthropic.Types.Request (
    AnthropicConfig (..),
    CacheControl (..),
    Role (..),
    ContentBlock (..),
    MessagesRequest (..),
    SystemBlock (..),
    Message (..),
    ToolDef (..),
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

data AnthropicConfig = AnthropicConfig
    { acApiKey :: Text
    , acModel :: Text
    , acBaseUrl :: Text
    }

newtype CancelToken = CancelToken (IORef Bool)

newCancelToken :: IO CancelToken
newCancelToken = CancelToken <$> newIORef False

cancel :: CancelToken -> IO ()
cancel (CancelToken ref) = atomicWriteIORef ref True

isCancelled :: CancelToken -> IO Bool
isCancelled (CancelToken ref) = readIORef ref

data CacheControl = Ephemeral | EphemeralHour
    deriving (Eq, Show)

instance ToJSON CacheControl where
    toJSON Ephemeral = object ["type" .= ("ephemeral" :: Text)]
    toJSON EphemeralHour =
        object
            [ "type" .= ("ephemeral" :: Text)
            , "ttl" .= ("1h" :: Text)
            ]

data Role = RoleUser | RoleAssistant
    deriving (Eq, Show)

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
