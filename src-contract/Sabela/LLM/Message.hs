module Sabela.LLM.Message (
    Role (..),
    ContentPart (..),
    Message (..),
    Conversation (..),
    ToolCall (..),
    ToolResult (..),
) where

import Data.Aeson (Value)
import Data.Text (Text)

import Sabela.AI.Types (ToolOutcome)
import Sabela.Ids (ToolCallId)

data Role = User | Assistant | System
    deriving (Eq, Show)

data ToolCall = ToolCall
    { tcId :: ToolCallId
    , tcName :: Text
    , tcInput :: Value
    }
    deriving (Eq, Show)

data ToolResult = ToolResult
    { trFor :: ToolCallId
    , trName :: Text
    , trOutcome :: ToolOutcome
    }
    deriving (Eq, Show)

data ContentPart
    = TextPart Text
    | ToolCallPart ToolCall
    | ToolResultPart ToolResult
    deriving (Eq, Show)

data Message = Message
    { msgRole :: Role
    , msgParts :: [ContentPart]
    }
    deriving (Eq, Show)

newtype Conversation = Conversation {convMessages :: [Message]}
    deriving (Eq, Show)
