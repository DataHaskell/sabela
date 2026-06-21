{-# LANGUAGE OverloadedStrings #-}

{- | Safety compaction for tool results before they land in conversation
history: a large structured payload is stashed in the handle store and
replaced by a compact reference the LLM can drill into via @explore_result@,
rather than silently clipped. Split out of 'Sabela.AI.Orchestrator.Loop' so
that module stays within the size cap; the loop is the only caller.
-}
module Sabela.AI.Orchestrator.Compact (
    compactToolResult,
    resultToText,
) where

import Data.Aeson (Value (..), encode, object, (.=))
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import Sabela.AI.Handles (
    HandleRef (..),
    Output (..),
    storeLargeResult,
    summarizeForLLM,
 )
import Sabela.AI.Store

{- | Threshold (in characters of the JSON-encoded form) above which a tool
result is stashed in the handle store instead of being inlined into the
conversation. Tools that know their output is large should stash proactively
(see Capabilities.compactOutputs); this is the safety net for structured
payloads that weren't explicitly pre-compacted.
-}
compactToolResultThreshold :: Int
compactToolResultThreshold = 8000

{- | Safety compaction pass for tool results before they land in conversation
history. If a result is small, pass it through unchanged. If it exceeds
'compactToolResultThreshold' characters once JSON-encoded, stash it in the
handle store and return a compact summary object referencing the handle, so
the LLM can drill in via @explore_result@ instead of losing the tail.

This replaces the old silent-clip behaviour that dropped bytes past 8000.
-}
compactToolResult :: AIStore -> Value -> IO Value
compactToolResult store v =
    -- Size-check via @LBS.length . encode@ first; only on the stash path
    -- do we round-trip through @Text@.
    if smallEnough v
        then pure v
        else do
            let text = resultToText v
            r <- storeLargeResult (aiHandles store) text
            case r of
                -- The handle store's own cleanup (ANSI strip + dedupe) shrank
                -- the payload below its own inline threshold. Use the cleaned
                -- text inline.
                Inline _ cleaned -> pure (String cleaned)
                Stashed (HandleRef hid summary nLines nBytes) ->
                    pure $
                        object
                            [ "_compacted" .= True
                            , "_note"
                                .= ( "Tool result exceeded inline limit; stashed. Drill in via explore_result." ::
                                        Text
                                   )
                            , "_large" .= summarizeForLLM hid summary nLines nBytes
                            ]

-- | Convert a JSON value to text for tool result content.
resultToText :: Value -> Text
resultToText (String s) = s
resultToText v = TL.toStrict (TLE.decodeUtf8 (encode v))

{- | Size precheck against 'compactToolResultThreshold' without the
@lazy-bytes → Text@ round-trip 'resultToText' does. For non-'String'
values the threshold is interpreted as UTF-8 bytes (matches the wire
limit); for ASCII payloads this is identical to a character count.
-}
smallEnough :: Value -> Bool
smallEnough (String s) = T.length s <= compactToolResultThreshold
smallEnough v = LBS.length (encode v) <= fromIntegral compactToolResultThreshold
