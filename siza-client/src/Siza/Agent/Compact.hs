{-# LANGUAGE OverloadedStrings #-}

module Siza.Agent.Compact (
    compactSeed,
    recallResult,
    resultStubFor,
    recallToolName,
    recallToolDef,
) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

recallToolName :: Text
recallToolName = "recall_result"

compactSeed :: [Value] -> ([Value], Map Int Text)
compactSeed = go 1 [] Map.empty
  where
    go _ acc store [] = (reverse acc, store)
    go n acc store (m : ms) = case toolContent m of
        Just full
            | T.length full > stubFloor ->
                go
                    (n + 1)
                    (stubMessage n m full : acc)
                    (Map.insert n full store)
                    ms
        _ -> go n (dropThinking m : acc) store ms

stubFloor :: Int
stubFloor = 200

toolContent :: Value -> Maybe Text
toolContent (Object o)
    | KM.lookup "role" o == Just (String "tool")
    , Just (String c) <- KM.lookup "content" o =
        Just c
toolContent _ = Nothing

stubMessage :: Int -> Value -> Text -> Value
stubMessage n (Object o) full =
    Object (KM.insert "content" (String (resultStubFor n toolName full)) o)
  where
    toolName = case KM.lookup "tool_name" o of
        Just (String t) -> t
        _ -> "tool"
stubMessage _ v _ = v

resultStubFor :: Int -> Text -> Text -> Text
resultStubFor n toolName full =
    "[result #"
        <> T.pack (show n)
        <> " from "
        <> toolName
        <> " ("
        <> T.pack (show (T.length full))
        <> " chars, elided): "
        <> T.takeWhile (/= '\n') (T.take stubPreview full)
        <> "… — read it with "
        <> recallToolName
        <> " {\"index\": "
        <> T.pack (show n)
        <> "}]"

stubPreview :: Int
stubPreview = 90

recallResult :: Map Int Text -> Value -> Text
recallResult store args = case indexArg of
    Nothing -> "recall_result needs an integer `index`, as named in a [result #N] line."
    Just n -> case Map.lookup n store of
        Just full -> full
        Nothing ->
            "No result #"
                <> T.pack (show n)
                <> " in this conversation. Available: "
                <> T.intercalate ", " (map (T.pack . show) (Map.keys store))
                <> "."
  where
    indexArg = case args of
        Object o -> case KM.lookup (K.fromText "index") o of
            Just (Number s) -> Just (round s)
            Just (String t) -> readIndex t
            _ -> Nothing
        _ -> Nothing

readIndex :: Text -> Maybe Int
readIndex t = case reads (T.unpack (T.strip t)) of
    [(n, "")] -> Just n
    _ -> Nothing

recallToolDef :: Value
recallToolDef =
    object
        [ "type" .= ("function" :: Text)
        , "function"
            .= object
                [ "name" .= recallToolName
                , "description"
                    .= ( "Read a tool result from earlier in this conversation, \
                         \by the index named in a [result #N] line. Older results \
                         \are elided from the conversation to leave room; this \
                         \returns one in full." ::
                            Text
                       )
                , "parameters"
                    .= object
                        [ "type" .= ("object" :: Text)
                        , "properties"
                            .= object
                                [ "index"
                                    .= object
                                        [ "type" .= ("integer" :: Text)
                                        , "description"
                                            .= ("The N from a [result #N] line." :: Text)
                                        ]
                                ]
                        , "required" .= (["index"] :: [Text])
                        ]
                ]
        ]

dropThinking :: Value -> Value
dropThinking (Object o) = Object (KM.delete (K.fromText "thinking") o)
dropThinking v = v
