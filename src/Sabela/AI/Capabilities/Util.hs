{-# LANGUAGE OverloadedStrings #-}

{- | Shared utilities for the tool implementations: field extraction from
a JSON tool-arg payload, and compaction of large outputs/error text
via the 'AIStore' handle store.
-}
module Sabela.AI.Capabilities.Util (
    -- * JSON field extraction
    field,
    fieldText,
    fieldInt,

    -- * Typed enum parsing
    parseCellLang,
    parseCellType,

    -- * Output compaction
    compactOutputs,
    compactMaybeText,
) where

import Data.Aeson (ToJSON (..), Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)

import Sabela.AI.Handles (storeLargeResult, summarizeForLLM)
import Sabela.AI.Store
import Sabela.Model (CellType (..), OutputItem (..), mimeIndicator)
import qualified Sabela.SessionTypes as ST

field :: Text -> Value -> Maybe Value
field key (Object o) = KM.lookup (Key.fromText key) o
field _ _ = Nothing

fieldText :: Text -> Value -> Text
fieldText key v = case field key v of
    Just (String s) -> s
    _ -> ""

fieldInt :: Text -> Value -> Maybe Int
fieldInt key v = case field key v of
    Just (Number n) -> Just (round n)
    _ -> Nothing

{- | Total parser for the AI tool @language@ field. The schemas declare
@enum: [\"Haskell\", \"Python\"]@; an unrecognised value is a tool-call
bug rather than a Haskell default — return 'Nothing' so the caller can
surface a clear error.
-}
parseCellLang :: Text -> Maybe ST.CellLang
parseCellLang "Haskell" = Just ST.Haskell
parseCellLang "Python" = Just ST.Python
parseCellLang _ = Nothing

{- | Total parser for the AI tool @cell_type@ field. The schemas declare
@enum: [\"CodeCell\", \"ProseCell\"]@; unrecognised values come back as
'Nothing' instead of silently producing a 'CodeCell'.
-}
parseCellType :: Text -> Maybe CellType
parseCellType "CodeCell" = Just CodeCell
parseCellType "ProseCell" = Just ProseCell
parseCellType _ = Nothing

-- | Render outputs compactly: large individual outputs are swapped for a handle.
compactOutputs :: AIStore -> [OutputItem] -> IO Value
compactOutputs store items = do
    compacted <- mapM compactOne items
    pure (toJSON compacted)
  where
    compactOne oi = do
        r <- storeLargeResult (aiHandles store) (oiOutput oi)
        case r of
            Left cleaned ->
                pure $
                    object
                        [ "mime" .= mimeIndicator (oiMime oi)
                        , "output" .= cleaned
                        ]
            Right (hid, summary, nLines, nBytes) ->
                pure $
                    object
                        [ "mime" .= mimeIndicator (oiMime oi)
                        , "large" .= summarizeForLLM hid summary nLines nBytes
                        ]

compactMaybeText :: AIStore -> Maybe Text -> IO Value
compactMaybeText _ Nothing = pure Null
compactMaybeText store (Just t) = do
    r <- storeLargeResult (aiHandles store) t
    pure $ case r of
        Left cleaned -> String cleaned
        Right (hid, summary, nLines, nBytes) -> summarizeForLLM hid summary nLines nBytes
