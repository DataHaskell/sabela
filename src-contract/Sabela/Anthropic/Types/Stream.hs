{-# LANGUAGE OverloadedStrings #-}

{- | Server-sent event types for streaming Anthropic Messages responses:
the @StreamEvent@ envelope plus the deltas it carries.
-}
module Sabela.Anthropic.Types.Stream (
    StreamEvent (..),
    ContentDelta (..),
    MessageDelta (..),
) where

import Data.Aeson (
    FromJSON (..),
    withObject,
    (.:),
    (.:?),
 )
import qualified Data.Aeson.Types as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.Anthropic.Types.Request (ContentBlock)
import Sabela.Anthropic.Types.Response (MessageResponse, StopReason, Usage)

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
