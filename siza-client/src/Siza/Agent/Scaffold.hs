{-# LANGUAGE OverloadedStrings #-}

module Siza.Agent.Scaffold (
    runScaffoldStage,
    scaffoldCall,
    scaffoldDisclosure,
    scaffoldFile,
    scaffoldNoteFor,
    scaffoldText,
) where

import Data.Aeson (Value, object, (.=))
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Types (ToolOutcome)
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.Deliverable (requestedNames)
import Siza.Agent.Messages (toolMsg)
import Siza.Agent.Owned (ownedCellOutcome)
import Siza.Agent.Tools (renderOutcome)

runScaffoldStage ::
    (ToolCall -> IO (Either Text ToolOutcome)) -> Text -> IO [Value]
runScaffoldStage disp prompt = case scaffoldFile prompt of
    Nothing -> pure []
    Just file -> do
        let call = ToolCall "insert_cell" (object ["source" .= scaffoldText file])
        outcome <- disp call
        let verified = maybe False snd (ownedCellOutcome call outcome)
            disclosed =
                toolMsg
                    (ToolCall "scaffold" (tcArgs call))
                    (scaffoldDisclosure file (renderOutcome outcome))
            note =
                [ object
                    [ "role" .= ("user" :: Text)
                    , "content" .= scaffoldNoteFor prompt file
                    ]
                | verified
                ]
        pure (disclosed : note)

scaffoldFile :: Text -> Maybe Text
scaffoldFile =
    find (".csv" `T.isSuffixOf`)
        . map trimPathToken
        . T.words

trimPathToken :: Text -> Text
trimPathToken =
    T.dropWhileEnd (`elem` ("`\"',." :: String))
        . T.dropWhile (`elem` ("`\"'," :: String))

scaffoldCall :: Text -> Maybe ToolCall
scaffoldCall prompt = do
    file <- scaffoldFile prompt
    pure (ToolCall "insert_cell" (object ["source" .= scaffoldText file]))

scaffoldNoteFor :: Text -> Text -> Text
scaffoldNoteFor prompt file =
    "Setup: a cell loading `"
        <> file
        <> "` into `df` ran successfully, so `df` is in scope; read a column \
           \with `D.columnAsList (D.col @Type \"name\") df`. The request above \
           \still stands in full: write every cell it asks for"
        <> stillClause
        <> "."
  where
    stillClause = case requestedNames prompt of
        [] -> ""
        ns -> ", including " <> T.intercalate ", " ["`" <> n <> "`" | n <- ns]

scaffoldDisclosure :: Text -> Text -> Text
scaffoldDisclosure file rendered =
    "Setup write: inserted a cell loading `"
        <> file
        <> "` into `df`. Outcome: "
        <> rendered

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
