{- | A provider-neutral tool specification: a name, a description, and a raw
JSON Schema. Carries no @cache_control@ (an Anthropic billing concern) and no
provider envelope — each adapter renders this into its own tool shape
(@input_schema@ for Anthropic, @{type:function}@ for Ollama, @inputSchema@ for
MCP).
-}
module Sabela.LLM.Tool (ToolSpec (..)) where

import Data.Aeson (Value)
import Data.Text (Text)

import Sabela.AI.Capabilities.ToolName (ToolName)

data ToolSpec = ToolSpec
    { toolName :: ToolName
    , toolDescription :: Text
    , toolSchema :: Value
    }
    deriving (Eq, Show)
