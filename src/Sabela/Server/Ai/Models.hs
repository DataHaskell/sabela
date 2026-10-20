{-# LANGUAGE OverloadedStrings #-}

-- | The Anthropic models the settings picker offers by name.
module Sabela.Server.Ai.Models (knownModels) where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)

knownModels :: [Value]
knownModels =
    [ modelEntry
        "claude-haiku-4-5-20251001"
        "Haiku 4.5"
        "Fast + cheap; best for high-frequency iteration"
    , modelEntry
        "claude-sonnet-4-6"
        "Sonnet 4.6"
        "Recommended balance of speed and capability"
    , modelEntry
        "claude-opus-4-7"
        "Opus 4.7"
        "Most capable; slower; use for hard reasoning"
    , modelEntry "claude-sonnet-4-20250514" "Sonnet 4 (legacy)" "Original default"
    ]
  where
    modelEntry :: Text -> Text -> Text -> Value
    modelEntry mid label desc =
        object ["id" .= mid, "label" .= label, "description" .= desc]
