{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The 'ToolName' ADT and the single source of truth for the wire-format
strings the AI emits.

Lives in its own module so both:

* 'Sabela.AI.Capabilities' (the dispatcher) — for total pattern coverage.
* 'Sabela.AI.Capabilities.Tools.*' (the catalogue) — so 'mkTool' takes a
  'ToolName' and the schema sent to Anthropic can only spell tool names
  that the dispatcher actually handles.

Adding a tool: add a constructor here, add it to 'parseToolName' /
'toolWireName' (the compiler enforces this via exhaustive matches once
each is rewritten), add it to the chat catalogue with 'mkTool', and add
an @exec*@ branch in the dispatcher.
-}
module Sabela.AI.Capabilities.ToolName (
    ToolName (..),
    parseToolName,
    toolWireName,
    mkTool,
) where

import Data.Aeson (Value)
import Data.Text (Text)
import Sabela.Anthropic.Types (ToolDef (..))

data ToolName
    = ListCells
    | ReadCell
    | ReadCellOutput
    | FindCellsByContent
    | ProposeEdit
    | ReplaceCellSource
    | InsertCell
    | DeleteCell
    | ExecuteCell
    | Scratchpad
    | GhciQuery
    | ApiReference
    | ExploreResult
    | KernelStatus
    | Interrupt
    | KernelRestart
    | AwaitIdle
    | ExportNotebook
    deriving (Show, Eq)

parseToolName :: Text -> Maybe ToolName
parseToolName = \case
    "list_cells" -> Just ListCells
    "read_cell" -> Just ReadCell
    "read_cell_output" -> Just ReadCellOutput
    "find_cells_by_content" -> Just FindCellsByContent
    "propose_edit" -> Just ProposeEdit
    "replace_cell_source" -> Just ReplaceCellSource
    "insert_cell" -> Just InsertCell
    "delete_cell" -> Just DeleteCell
    "execute_cell" -> Just ExecuteCell
    "scratchpad" -> Just Scratchpad
    "ghci_query" -> Just GhciQuery
    "api_reference" -> Just ApiReference
    "explore_result" -> Just ExploreResult
    "kernel_status" -> Just KernelStatus
    "interrupt" -> Just Interrupt
    "kernel_restart" -> Just KernelRestart
    "await_idle" -> Just AwaitIdle
    "export_notebook" -> Just ExportNotebook
    _ -> Nothing

{- | Wire-format spelling of a 'ToolName'. The inverse of 'parseToolName':
@parseToolName (toolWireName t) == Just t@ for every constructor. The
exhaustive case keeps the two functions in sync at compile time.
-}
toolWireName :: ToolName -> Text
toolWireName = \case
    ListCells -> "list_cells"
    ReadCell -> "read_cell"
    ReadCellOutput -> "read_cell_output"
    FindCellsByContent -> "find_cells_by_content"
    ProposeEdit -> "propose_edit"
    ReplaceCellSource -> "replace_cell_source"
    InsertCell -> "insert_cell"
    DeleteCell -> "delete_cell"
    ExecuteCell -> "execute_cell"
    Scratchpad -> "scratchpad"
    GhciQuery -> "ghci_query"
    ApiReference -> "api_reference"
    ExploreResult -> "explore_result"
    KernelStatus -> "kernel_status"
    Interrupt -> "interrupt"
    KernelRestart -> "kernel_restart"
    AwaitIdle -> "await_idle"
    ExportNotebook -> "export_notebook"

{- | Build a 'ToolDef' from a typed 'ToolName'. The wire-name string sent
to Anthropic comes from 'toolWireName', the single source of truth that
'parseToolName' is the inverse of — so the catalogue can only spell names
the dispatcher actually handles.
-}
mkTool :: ToolName -> Text -> Value -> ToolDef
mkTool name desc schema = ToolDef (toolWireName name) desc schema Nothing
