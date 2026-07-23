{-# LANGUAGE OverloadedStrings #-}

module Sabela.LLM.Ollama.Client.Repair (
    recoverFromContent,
    recoverFromError,
    repairJsonEscapes,
) where

import Data.Aeson (Value (..), eitherDecodeStrict', object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Sabela.LLM.Ollama.Client.Types (ToolCall (..), Turn (..), rawWithCalls)

recoverFromError :: Text -> Maybe Turn
recoverFromError err = do
    payload <- extractRawPayload err
    obj <- firstJsonObject payload
    call <- callFromArgsJsonWith inferToolNameWide obj
    pure (synthTurn call)

recoverFromContent :: Text -> Maybe ToolCall
recoverFromContent t = do
    obj <- firstJsonObject t
    callFromArgsJson obj

synthTurn :: ToolCall -> Turn
synthTurn call =
    Turn
        { turnRaw =
            rawWithCalls
                (object ["role" .= ("assistant" :: Text), "content" .= ("" :: Text)])
                [call]
        , turnContent = ""
        , turnCalls = [call]
        }

extractRawPayload :: Text -> Maybe Text
extractRawPayload err = do
    afterRaw <- T.stripPrefix "raw='" (snd (T.breakOn "raw='" err))
    let (payload, rest) = T.breakOn "', err=" afterRaw
    if T.null rest then Nothing else Just payload

callFromArgsJson :: Text -> Maybe ToolCall
callFromArgsJson = callFromArgsJsonWith inferToolName

callFromArgsJsonWith ::
    (KM.KeyMap Value -> Maybe Text) -> Text -> Maybe ToolCall
callFromArgsJsonWith infer payload = do
    Object o <- decodeRepaired payload
    name <- infer o
    pure (ToolCall name (Object o))

inferToolName :: KM.KeyMap Value -> Maybe Text
inferToolName o
    | has "source" = Just "insert_cell"
    | has "new_source" = Just "replace_cell_source"
    | has "code" = Just "try"
    | otherwise = Nothing
  where
    has k = KM.member (K.fromText k) o

inferToolNameWide :: KM.KeyMap Value -> Maybe Text
inferToolNameWide o = case inferToolName o of
    Just n -> Just n
    Nothing
        | KM.member (K.fromText "query") o -> Just "discover"
        | otherwise -> Nothing

decodeRepaired :: Text -> Maybe Value
decodeRepaired payload =
    decode (repairJsonEscapes payload)
        `orElse` decode (repairDanglingComma (repairJsonEscapes payload))
  where
    decode t = case eitherDecodeStrict' (TE.encodeUtf8 t) of
        Right v -> Just v
        Left _ -> Nothing
    orElse (Just v) _ = Just v
    orElse Nothing y = y

repairDanglingComma :: Text -> Text
repairDanglingComma t = case T.stripSuffix ",\"}" stripped of
    Just body -> body <> "}"
    Nothing -> case T.stripSuffix ",}" stripped of
        Just body -> body <> "}"
        Nothing -> t
  where
    stripped = T.stripEnd t

firstJsonObject :: Text -> Maybe Text
firstJsonObject t = case T.breakOn "{" t of
    (_, rest) | T.null rest -> Nothing
    (_, rest) -> Just (T.dropWhileEnd (/= '}') rest)

repairJsonEscapes :: Text -> Text
repairJsonEscapes = T.pack . go False . T.unpack
  where
    go _ [] = []
    go False (c : cs)
        | c == '"' = c : go True cs
        | otherwise = c : go False cs
    go True ('\\' : c : cs)
        | c `elem` validEscapes = '\\' : c : go True cs
        | Just e <- controlEscape c = '\\' : '\\' : '\\' : e : go True cs
        | otherwise = '\\' : '\\' : c : go True cs
    go True ['\\'] = "\\\\"
    go True ('"' : cs) = '"' : go False cs
    go True (c : cs)
        | Just e <- controlEscape c = '\\' : e : go True cs
        | otherwise = c : go True cs
    validEscapes = "\"\\/bfnrtu" :: [Char]
    controlEscape '\n' = Just 'n'
    controlEscape '\t' = Just 't'
    controlEscape '\r' = Just 'r'
    controlEscape _ = Nothing
