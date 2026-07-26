{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Capabilities.Util (
    field,
    fieldText,
    fieldInt,
    fieldBool,
    parseCellLang,
    parseCellType,
    inlineOrStash,
    compactOutputs,
    compactMaybeText,
    featureEnabled,
    featureOptIn,
) where

import Data.Aeson (ToJSON (..), Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

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
    Just (String s) -> readMaybe (T.unpack (T.strip s))
    _ -> Nothing

fieldBool :: Text -> Value -> Bool
fieldBool key v = field key v == Just (Bool True)

parseCellLang :: Text -> Maybe ST.CellLang
parseCellLang t = case T.toLower (T.strip t) of
    "haskell" -> Just ST.Haskell
    "hs" -> Just ST.Haskell
    "python" -> Just ST.Python
    "py" -> Just ST.Python
    _ -> Nothing

parseCellType :: Text -> Maybe CellType
parseCellType t = case T.toLower (T.strip t) of
    "codecell" -> Just CodeCell
    "code" -> Just CodeCell
    "prosecell" -> Just ProseCell
    "prose" -> Just ProseCell
    "markdown" -> Just ProseCell
    "md" -> Just ProseCell
    _ -> Nothing

inlineOrStash :: AIStore -> MimeType -> Text -> IO Output
inlineOrStash store mime text = do
    out <- storeLargeResult (aiHandles store) text
    pure $ case out of
        Inline _ cleaned -> Inline mime cleaned
        stashed -> stashed

compactOutputs :: AIStore -> [OutputItem] -> IO Value
compactOutputs store items = do
    compacted <- mapM compactOne items
    pure (toJSON compacted)
  where
    compactOne oi = toJSON <$> inlineOrStash store (oiMime oi) (oiOutput oi)

compactMaybeText :: AIStore -> Maybe Text -> IO Value
compactMaybeText _ Nothing = pure Null
compactMaybeText store (Just t) = toJSON <$> inlineOrStash store MimePlain t

featureEnabled :: String -> IO Bool
featureEnabled var = do
    v <- lookupEnv var
    pure (maybe True (\s -> map toLower s `notElem` ["0", "off", "false", "no"]) v)

featureOptIn :: String -> IO Bool
featureOptIn var = do
    v <- lookupEnv var
    pure
        (maybe False (\s -> map toLower s `notElem` ["", "0", "off", "false", "no"]) v)
