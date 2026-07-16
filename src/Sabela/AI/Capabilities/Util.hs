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

    -- * Feature flags
    featureEnabled,
) where

import Data.Aeson (ToJSON (..), Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv)

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

{- | Parse the AI tool @language@ field, liberal in what it accepts: the schema
enum plus common lowercase aliases a model reaches for (@haskell@, @hs@, @py@).
'Nothing' only for a truly unknown value.
-}
parseCellLang :: Text -> Maybe ST.CellLang
parseCellLang t = case T.toLower (T.strip t) of
    "haskell" -> Just ST.Haskell
    "hs" -> Just ST.Haskell
    "python" -> Just ST.Python
    "py" -> Just ST.Python
    _ -> Nothing

{- | Parse the AI tool @cell_type@ field, liberal in what it accepts: the schema
enum plus the lowercase aliases a model reaches for (@code@, @prose@, @markdown@,
@md@). 'Nothing' only for a truly unknown value.
-}
parseCellType :: Text -> Maybe CellType
parseCellType t = case T.toLower (T.strip t) of
    "codecell" -> Just CodeCell
    "code" -> Just CodeCell
    "prosecell" -> Just ProseCell
    "prose" -> Just ProseCell
    "markdown" -> Just ProseCell
    "md" -> Just ProseCell
    _ -> Nothing

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

{- | A feature flag that defaults ON: enabled unless the env var is explicitly
set to a falsey value (0/off/false/no).
-}
featureEnabled :: String -> IO Bool
featureEnabled var = do
    v <- lookupEnv var
    pure (maybe True (\s -> map toLower s `notElem` ["0", "off", "false", "no"]) v)
