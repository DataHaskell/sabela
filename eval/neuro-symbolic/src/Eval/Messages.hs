{-# LANGUAGE OverloadedStrings #-}

module Eval.Messages (
    toolMsg,
    reenterMsg,
    reenterMessage,
    verifyMsg,
) where

import Data.Aeson (Value, object, (.=))
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T

import Eval.Ollama (ToolCall (..))
import Sabela.AI.CellResult (CellId)

toolMsg :: ToolCall -> Text -> Value
toolMsg tc result =
    object
        [ "role" .= ("tool" :: Text)
        , "tool_name" .= tcName tc
        , "content" .= result
        ]

reenterMsg :: [CellId] -> Value
reenterMsg reds =
    object
        [ "role" .= ("tool" :: Text)
        , "tool_name" .= ("health_gate" :: Text)
        , "content" .= reenterMessage reds
        ]

reenterMessage :: [CellId] -> Text
reenterMessage reds =
    "Not done yet: cell(s) "
        <> T.pack (intercalate ", " (map show reds))
        <> " that you wrote still have errors. Read each cell's error, fix it with \
           \replace_cell_source, and do not stop until every cell you wrote runs \
           \without error."

verifyMsg :: Value
verifyMsg =
    object
        [ "role" .= ("tool" :: Text)
        , "tool_name" .= ("verify" :: Text)
        , "content"
            .= ( "Your cells run, but the task's deliverable does not pass its check \
                 \yet. Keep working until the requested binding or plot is correct, \
                 \then stop." ::
                    Text
               )
        ]
