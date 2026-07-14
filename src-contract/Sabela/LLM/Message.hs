{- | Provider-neutral conversation vocabulary: the domain's own words for a
message, its parts, and the tool calls/results that flow inside an exchange.
No provider wire detail (Anthropic block names, @tool_use_id@, Ollama channels)
appears here — adapters translate to/from these types at the boundary.
-}
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

-- | Who authored a message.
data Role = User | Assistant | System
    deriving (Show, Eq)

{- | A tool the assistant asked to run. @tcName@ is the wire name as emitted by
the model — kept as 'Text', not a parsed tool enum, because models invent names;
dispatch resolves or rejects it.
-}
data ToolCall = ToolCall
    { tcId :: ToolCallId
    , tcName :: Text
    , tcInput :: Value
    }
    deriving (Show, Eq)

{- | Our answer to one 'ToolCall', carrying the already-clean 'ToolOutcome'.
Keeps both the call id (Anthropic pairs results to calls by id) and the tool
name (Ollama pairs by name) so either adapter can render it.
-}
data ToolResult = ToolResult
    { trFor :: ToolCallId
    , trName :: Text
    , trOutcome :: ToolOutcome
    }
    deriving (Show, Eq)

-- | One part of a message's content.
data ContentPart
    = TextPart Text
    | ToolCallPart ToolCall
    | ToolResultPart ToolResult
    deriving (Show, Eq)

data Message = Message
    { msgRole :: Role
    , msgParts :: [ContentPart]
    }
    deriving (Show, Eq)

-- | An ordered exchange history. The aggregate root of a chat session.
newtype Conversation = Conversation {convMessages :: [Message]}
    deriving (Show, Eq)
