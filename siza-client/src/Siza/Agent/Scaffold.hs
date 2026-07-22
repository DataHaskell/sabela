{-# LANGUAGE OverloadedStrings #-}

{- | The setup scaffold stage: when a request names a data file, pre-commit the
loading cell as a disclosed write, keyed on prompt structure (never a library
token); the note is gated on the cell verifiably running clean (R7.4, M16).
-}
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

{- | Run the scaffold stage through the injected dispatch: the setup write's
call and real outcome land in the transcript on the @scaffold@ channel, and
the orienting note is emitted only when the cell verifiably ran clean.
-}
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

-- | The data file the request names, if any: the structural scaffold key.
scaffoldFile :: Text -> Maybe Text
scaffoldFile =
    find (".csv" `T.isSuffixOf`)
        . map (T.dropAround (`elem` ("`\"',." :: String)))
        . T.words

-- | Pre-load the named data file, keyed off the prompt's structure alone.
scaffoldCall :: Text -> Maybe ToolCall
scaffoldCall prompt = do
    file <- scaffoldFile prompt
    pure (ToolCall "insert_cell" (object ["source" .= scaffoldText file]))

{- | The orienting note for a VERIFIED scaffold: it states only the observed
outcome and restates the full remaining ask, naming every typed deliverable.
Callers must gate it on the scaffold cell's execution reporting healthy.
-}
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

{- | The transcript disclosure for the scaffold write: names the action and
carries the real rendered outcome, so the note that follows has visible
evidence and the write is never silent (the product-chat requirement).
-}
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
