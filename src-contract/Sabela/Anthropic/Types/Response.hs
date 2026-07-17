{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Types describing what the Anthropic Messages API sends back: the
final 'MessageResponse', why it stopped, and the token-usage breakdown
the hub charges against.
-}
module Sabela.Anthropic.Types.Response (
    MessageResponse (..),
    StopReason (..),
    Usage (..),
) where

import Data.Aeson (
    FromJSON (..),
    ToJSON (..),
    object,
    withObject,
    withText,
    (.:),
    (.:?),
    (.=),
 )
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.Anthropic.Types.Request (ContentBlock)

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

data StopReason = SREndTurn | SRToolUse | SRMaxTokens
    deriving (Eq, Show)

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
    deriving (Show)

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
