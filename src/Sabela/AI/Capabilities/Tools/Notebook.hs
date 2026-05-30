{-# LANGUAGE OverloadedStrings #-}

{- | Tool schemas for notebook reads + mutations + execution. Sibling of
"Sabela.AI.Capabilities.Tools.Query"; both feed the umbrella
"Sabela.AI.Capabilities.Tools".
-}
module Sabela.AI.Capabilities.Tools.Notebook (notebookTools) where

import Data.Aeson (object, (.=))
import Data.Text (Text)
import Sabela.AI.Capabilities.ToolName (ToolName (..), mkTool)
import Sabela.Anthropic.Types (ToolDef)

notebookTools :: [ToolDef]
notebookTools =
    [ mkTool
        ListCells
        "List all cells in the notebook with their IDs, types, languages, first line of source, and whether they have errors. Use this for a quick overview."
        (object ["type" .= ("object" :: Text), "properties" .= object []])
    , mkTool
        ReadCell
        "Read the full source code and outputs of a specific cell."
        ( object
            [ "type" .= ("object" :: Text)
            , "properties"
                .= object
                    [ "cell_id"
                        .= object
                            [ "type" .= ("integer" :: Text)
                            , "description" .= ("The cell ID to read" :: Text)
                            ]
                    ]
            , "required" .= (["cell_id"] :: [Text])
            ]
        )
    , mkTool
        ReadCellOutput
        "Read only the outputs and error of a cell (not the source). Useful for checking execution results."
        ( object
            [ "type" .= ("object" :: Text)
            , "properties"
                .= object
                    [ "cell_id"
                        .= object
                            [ "type" .= ("integer" :: Text)
                            , "description" .= ("The cell ID" :: Text)
                            ]
                    ]
            , "required" .= (["cell_id"] :: [Text])
            ]
        )
    , mkTool
        FindCellsByContent
        "Search cell sources for a pattern (substring match). Returns matching cell IDs with context."
        ( object
            [ "type" .= ("object" :: Text)
            , "properties"
                .= object
                    [ "pattern"
                        .= object
                            [ "type" .= ("string" :: Text)
                            , "description" .= ("Substring to search for in cell sources" :: Text)
                            ]
                    ]
            , "required" .= (["pattern"] :: [Text])
            ]
        )
    , mkTool
        ReplaceCellSource
        "Replace the source of a cell you own and AUTO-RUN it. Use this for iterating on cells you inserted yourself — no user approval round-trip. For Haskell code cells the tool response includes an `execution` field {ran, ok, outputs, error, errors}; read it before moving on. Pass expected_hash to detect drift. For user-authored changes that deserve review, use propose_edit instead."
        ( object
            [ "type" .= ("object" :: Text)
            , "properties"
                .= object
                    [ "cell_id"
                        .= object
                            [ "type" .= ("integer" :: Text)
                            , "description" .= ("The cell ID to replace" :: Text)
                            ]
                    , "new_source"
                        .= object
                            [ "type" .= ("string" :: Text)
                            , "description" .= ("The new source code — full replacement, not a diff" :: Text)
                            ]
                    , "expected_hash"
                        .= object
                            [ "type" .= ("string" :: Text)
                            , "description"
                                .= ( "Optional: the cell hash you last saw. If it does not match, the edit is rejected and you should re-read the cell." ::
                                        Text
                                   )
                            ]
                    ]
            , "required" .= (["cell_id", "new_source"] :: [Text])
            ]
        )
    , mkTool
        ProposeEdit
        "Propose a source code change for a cell. The change is NOT applied immediately — the user must accept it. Use for edits to cells the user authored where review matters; for your own scaffolding cells prefer replace_cell_source which applies + runs in one step. Returns an editId for tracking. Pass expected_hash to detect drift."
        ( object
            [ "type" .= ("object" :: Text)
            , "properties"
                .= object
                    [ "cell_id"
                        .= object
                            [ "type" .= ("integer" :: Text)
                            , "description" .= ("The cell ID to edit" :: Text)
                            ]
                    , "new_source"
                        .= object
                            [ "type" .= ("string" :: Text)
                            , "description" .= ("The new source code" :: Text)
                            ]
                    , "expected_hash"
                        .= object
                            [ "type" .= ("string" :: Text)
                            , "description"
                                .= ( "Optional: the cell hash you last saw. If it does not match the current hash, the edit is rejected and you should re-read the cell." ::
                                        Text
                                   )
                            ]
                    ]
            , "required" .= (["cell_id", "new_source"] :: [Text])
            ]
        )
    , mkTool
        InsertCell
        "Insert a new cell into the notebook. Applied immediately."
        ( object
            [ "type" .= ("object" :: Text)
            , "properties"
                .= object
                    [ "after_cell_id"
                        .= object
                            [ "type" .= ("integer" :: Text)
                            , "description" .= ("Insert after this cell ID. Use -1 for beginning." :: Text)
                            ]
                    , "cell_type"
                        .= object
                            [ "type" .= ("string" :: Text)
                            , "enum" .= (["CodeCell", "ProseCell"] :: [Text])
                            ]
                    , "language"
                        .= object
                            [ "type" .= ("string" :: Text)
                            , "enum" .= (["Haskell", "Python"] :: [Text])
                            ]
                    , "source"
                        .= object
                            [ "type" .= ("string" :: Text)
                            , "description" .= ("Source code or markdown" :: Text)
                            ]
                    ]
            , "required" .= (["after_cell_id", "source"] :: [Text])
            ]
        )
    , mkTool
        DeleteCell
        "Delete a cell from the notebook."
        ( object
            [ "type" .= ("object" :: Text)
            , "properties"
                .= object
                    [ "cell_id"
                        .= object
                            [ "type" .= ("integer" :: Text)
                            , "description" .= ("The cell ID to delete" :: Text)
                            ]
                    ]
            , "required" .= (["cell_id"] :: [Text])
            ]
        )
    , mkTool
        ExecuteCell
        "Execute a cell and wait for its result. Returns the outputs and any errors."
        ( object
            [ "type" .= ("object" :: Text)
            , "properties"
                .= object
                    [ "cell_id"
                        .= object
                            [ "type" .= ("integer" :: Text)
                            , "description" .= ("The cell ID to execute" :: Text)
                            ]
                    ]
            , "required" .= (["cell_id"] :: [Text])
            ]
        )
    , mkTool
        Scratchpad
        "Run code in an isolated scratchpad session. Does NOT modify the notebook. The scratchpad has the same packages available but separate state. Use this for standalone experiments."
        ( object
            [ "type" .= ("object" :: Text)
            , "properties"
                .= object
                    [ "code"
                        .= object
                            [ "type" .= ("string" :: Text)
                            , "description" .= ("Code to execute" :: Text)
                            ]
                    , "language"
                        .= object
                            [ "type" .= ("string" :: Text)
                            , "enum" .= (["Haskell", "Python"] :: [Text])
                            , "description" .= ("Language. Default: Haskell." :: Text)
                            ]
                    ]
            , "required" .= (["code"] :: [Text])
            ]
        )
    ]
