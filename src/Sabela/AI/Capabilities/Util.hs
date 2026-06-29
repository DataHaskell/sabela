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
    fieldBool,

    -- * Typed enum parsing
    parseCellLang,
    parseCellType,

    -- * Output compaction
    inlineOrStash,
    compactOutputs,
    compactMaybeText,
) where

import Data.Aeson (ToJSON (..), Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)

import Sabela.AI.Handles (Output (..), storeLargeResult)
import Sabela.AI.Store
import Sabela.Model (CellType (..), MimeType (..), OutputItem (..))
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

-- | A boolean tool-arg flag, defaulting to 'False' when absent or non-boolean.
fieldBool :: Text -> Value -> Bool
fieldBool key v = field key v == Just (Bool True)

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

{- | The single inline-or-stash chokepoint. Both 'compactOutputs' and
'compactMaybeText' route through this so their inline shapes stop diverging:
'storeLargeResult' decides inline vs stash, and the supplied 'MimeType'
re-tags the inline placeholder so the wire @{mime,output}@ carries the real
MIME.
-}
inlineOrStash :: AIStore -> MimeType -> Text -> IO Output
inlineOrStash store mime text = do
    out <- storeLargeResult (aiHandles store) text
    pure $ case out of
        Inline _ cleaned -> Inline mime cleaned
        stashed -> stashed

-- | Render outputs compactly: large individual outputs are swapped for a handle.
compactOutputs :: AIStore -> [OutputItem] -> IO Value
compactOutputs store items = do
    compacted <- mapM compactOne items
    pure (toJSON compacted)
  where
    compactOne oi = toJSON <$> inlineOrStash store (oiMime oi) (oiOutput oi)

compactMaybeText :: AIStore -> Maybe Text -> IO Value
compactMaybeText _ Nothing = pure Null
compactMaybeText store (Just t) = toJSON <$> inlineOrStash store MimePlain t
