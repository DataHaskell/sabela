{-# LANGUAGE OverloadedStrings #-}

module Test.ArtifactFixtures (
    deferredInsert,
    deferredReplace,
    proseInsert,
    runPending,
    awaitIdle,
    listCells,
    readCell,
    execute,
    deferred,
    structuredDeferred,
    execution,
    pendingOutcome,
    cellsOutcome,
    cellsOutcomeOf,
    cellsOutcomeWithHash,
    readOutcome,
) where

import Data.Aeson (Value (Null), object, (.=))
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..))

deferredInsert
    , deferredReplace
    , proseInsert
    , runPending
    , awaitIdle
    , listCells
    , readCell ::
        ToolCall
deferredInsert = ToolCall "insert_cell" (object ["source" .= source])
deferredReplace =
    ToolCall
        "replace_cell_source"
        (object ["cell_id" .= (7 :: Int), "new_source" .= source])
proseInsert =
    ToolCall
        "insert_cell"
        (object ["cell_type" .= ("ProseCell" :: Text), "source" .= ("A note." :: Text)])
runPending = ToolCall "run_pending" (object [])
awaitIdle = ToolCall "await_idle" (object [])
listCells = ToolCall "list_cells" (object [])
readCell = ToolCall "read_cell" (object ["cell_id" .= (7 :: Int)])

execute :: Int -> ToolCall
execute cid = ToolCall "execute_cell" (object ["cell_id" .= cid])

deferred :: Int -> Either Text ToolOutcome
deferred cid =
    Right . ToolOk $
        object
            [ "cellId" .= cid
            , "hash" .= hashOf cid
            , "status" .= ("completed" :: Text)
            , "execution" .= Null
            ]

structuredDeferred :: Int -> Either Text ToolOutcome
structuredDeferred cid =
    Right . ToolOk $
        object
            [ "cellId" .= cid
            , "hash" .= hashOf cid
            , "status" .= ("completed" :: Text)
            , "execution"
                .= object
                    [ "ok" .= False
                    , "outcome" .= object ["tag" .= ("Deferred" :: Text)]
                    ]
            ]

execution :: Int -> Bool -> Either Text ToolOutcome
execution cid ok =
    Right . ToolOk $
        object
            [ "cellId" .= cid
            , "ok" .= ok
            , "outcome"
                .= object
                    [ "tag" .= if ok then ("Succeeded" :: Text) else "Raised"
                    ]
            ]

pendingOutcome :: [Int] -> Either Text ToolOutcome
pendingOutcome ids = Right (ToolOk (object ["pending" .= ids]))

cellsOutcome :: [(Int, Bool, Bool)] -> Either Text ToolOutcome
cellsOutcome = cellsOutcomeOf "CodeCell"

cellsOutcomeOf :: Text -> [(Int, Bool, Bool)] -> Either Text ToolOutcome
cellsOutcomeOf cellType =
    cellsOutcomeWithHash cellType
        . map (\(cid, dirty, hasError) -> (cid, dirty, hasError, hashOf cid))

cellsOutcomeWithHash ::
    Text -> [(Int, Bool, Bool, Text)] -> Either Text ToolOutcome
cellsOutcomeWithHash cellType states =
    Right . ToolOk . object $
        [ "cells"
            .= [ object
                    [ "id" .= cid
                    , "dirty" .= dirty
                    , "hasError" .= hasError
                    , "hash" .= hash
                    , "type" .= cellType
                    ]
               | (cid, dirty, hasError, hash) <- states
               ]
        ]

readOutcome :: Int -> Maybe Text -> Either Text ToolOutcome
readOutcome cid err =
    Right . ToolOk $
        object
            [ "id" .= cid
            , "hash" .= hashOf cid
            , "source" .= source
            , "error" .= err
            ]

hashOf :: Int -> Text
hashOf cid = "h" <> T.pack (show cid)

source :: Text
source = "answer = 42"
