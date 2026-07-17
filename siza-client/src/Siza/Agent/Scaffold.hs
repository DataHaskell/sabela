{-# LANGUAGE OverloadedStrings #-}

module Siza.Agent.Scaffold (scaffoldCall, scaffoldNote, scaffoldText) where

import Control.Monad (guard)
import Data.Aeson (object, (.=))
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.LLM.Ollama.Client (ToolCall (..))

-- | Pre-load a dataframe when the request names a CSV, keyed off the prompt text.
scaffoldCall :: Text -> Maybe ToolCall
scaffoldCall prompt = do
    guard ("dataframe" `T.isInfixOf` T.toLower prompt)
    file <- csvToken prompt
    pure (ToolCall "insert_cell" (object ["source" .= scaffoldText file]))

-- | The orienting note placed after the scaffold so the model builds on @df@.
scaffoldNote :: Text
scaffoldNote =
    "A DataFrame `df` is already loaded from the CSV and is in scope. Write the \
    \cell that computes the requested result from `df` (read a column with \
    \`D.columnAsList (D.col @Type \"name\") df`)."

scaffoldText :: Text -> Text
scaffoldText file =
    T.intercalate
        "\n"
        [ "-- cabal: build-depends: dataframe, text"
        , "{-# LANGUAGE TypeApplications #-}"
        , "{-# LANGUAGE OverloadedStrings #-}"
        , "import qualified DataFrame as D"
        , ""
        , "df <- D.readCsv \"" <> file <> "\""
        ]

csvToken :: Text -> Maybe Text
csvToken =
    find (".csv" `T.isSuffixOf`)
        . map (T.dropAround (`elem` ("`\"',." :: String)))
        . T.words
