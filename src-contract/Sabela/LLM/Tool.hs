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
