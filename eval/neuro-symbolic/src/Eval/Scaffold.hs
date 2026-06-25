{-# LANGUAGE OverloadedStrings #-}

module Eval.Scaffold (scaffoldCall, scaffoldNote, scaffoldText) where

import Control.Monad (guard)
import Data.Aeson (object, (.=))
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T

import Eval.Ollama (ToolCall (..))
import Eval.Task (Task (..))

scaffoldCall :: Task -> Maybe ToolCall
scaffoldCall task = do
    guard ("dataframe" `T.isInfixOf` T.toLower (taskPrompt task))
    file <- csvToken (taskPrompt task)
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
