{-# LANGUAGE OverloadedStrings #-}

module Sabela.LLM.Ollama.Client.Types (
    ToolCall (..),
    Turn (..),
    rawWithCalls,
) where

import Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)

data ToolCall = ToolCall
    { tcName :: Text
    , tcArgs :: Value
    }
    deriving (Eq, Show)

data Turn = Turn
    { turnRaw :: Value
    , turnContent :: Text
    , turnCalls :: [ToolCall]
    }
    deriving (Show)

rawWithCalls :: Value -> [ToolCall] -> Value
rawWithCalls (Object m) calls
    | not (null calls) =
        Object (KM.insert "tool_calls" (toJSON (map callJson calls)) m)
  where
    callJson (ToolCall n a) =
        object ["function" .= object ["name" .= n, "arguments" .= a]]
rawWithCalls v _ = v
