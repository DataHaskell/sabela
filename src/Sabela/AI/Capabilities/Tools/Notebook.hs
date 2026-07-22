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
        "Map of EVERY cell in the notebook (the whole notebook in one call): each cell's ID, position, type, language, the bindings it `defines`, and whether it errored. By default each cell shows only its first line (a compact preview that always lists all cells); pass `full: true` to include each cell's source too. To find which cell defines or uses a name, scan `defines` here or use find_cells_by_content; to read one cell in depth use read_cell."
        ( object
            [ "type" .= ("object" :: Text)
            , "properties"
                .= object
                    [ "full"
                        .= object
                            [ "type" .= ("boolean" :: Text)
                            , "description"
                                .= ("Include each cell's source (default false: first-line preview)." :: Text)
                            ]
                    ]
            ]
        )
    , mkTool
        ReadCell
        "Read one cell's full SOURCE and error by ID. Outputs (which can be large rendered HTML/SVG) are omitted by default — a `hasOutputs` flag signals them; pass `full: true` to include `outputs`."
        ( object
            [ "type" .= ("object" :: Text)
            , "properties"
                .= object
                    [ "cell_id"
                        .= object
                            [ "type" .= ("integer" :: Text)
                            , "description" .= ("The cell ID to read" :: Text)
                            ]
                    , "full"
                        .= object
                            [ "type" .= ("boolean" :: Text)
                            , "description" .= ("Include the cell's outputs (default false)." :: Text)
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
        "Search the NOTEBOOK's own cell sources for a substring (e.g. `model`, `fitDecisionTree`). Returns matching cell IDs with the matching lines. This is how you find which cell defines or uses something so you can edit it — unlike find_function/find_by_type, which search installed LIBRARIES, not your notebook."
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
        "Replace the source of a cell you own and AUTO-RUN it. Use this for iterating on cells you inserted yourself — no user approval round-trip. For Haskell code cells the tool response includes a typed `execution` CellResult {outcome, outputs, warnings, ok}; read `execution.ok` (and `outcome.tag` for the reason) before moving on. Pass expected_hash to detect drift. For user-authored changes that deserve review, use propose_edit instead."
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
        "Append a new cell at the end of the notebook. Applied immediately. New cells always go at the end; execution order follows data dependencies, not position."
        ( object
            [ "type" .= ("object" :: Text)
            , "properties"
                .= object
                    [ "cell_type"
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
            , "required" .= (["source"] :: [Text])
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
        "Execute a cell and wait for its result. Returns a typed CellResult {outcome, outputs, warnings, ok}; read `ok` (and `outcome.tag` for the reason) to act on it."
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
        "Run code in an isolated scratchpad session. Does NOT modify the notebook. The scratchpad has the same packages available but separate state. Use this for standalone experiments. It CANNOT see notebook variables (a cell's `df`, etc.) — to inspect or run a pure function on a live notebook value, use eval_live."
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
