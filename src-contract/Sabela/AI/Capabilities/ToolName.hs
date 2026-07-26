{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Capabilities.ToolName (
    ToolName (..),
    parseToolName,
    resolveToolCall,
    primaryArgKey,
    toolWireName,
    mkTool,
    actsOnNotebook,
) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
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
    | Try
    | Scratchpad
    | ListBindings
    | CheckType
    | FindByType
    | DescribeFunction
    | ApiReference
    | ExploreResult
    | KernelStatus
    | Interrupt
    | KernelRestart
    | AwaitIdle
    | ExportNotebook
    | PeekData
    | FindExampleCell
    | FindFunction
    | SearchCapability
    | EvalLive
    deriving (Eq, Show)

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
    "try" -> Just Try
    "scratchpad" -> Just Scratchpad
    "list_bindings" -> Just ListBindings
    "check_type" -> Just CheckType
    "find_by_type" -> Just FindByType
    "describe_function" -> Just DescribeFunction
    "api_reference" -> Just ApiReference
    "explore_result" -> Just ExploreResult
    "kernel_status" -> Just KernelStatus
    "interrupt" -> Just Interrupt
    "kernel_restart" -> Just KernelRestart
    "await_idle" -> Just AwaitIdle
    "export_notebook" -> Just ExportNotebook
    "peek_data" -> Just PeekData
    "find_example_cell" -> Just FindExampleCell
    "find_function" -> Just FindFunction
    "search_capability" -> Just SearchCapability
    "eval_live" -> Just EvalLive
    _ -> Nothing

resolveToolCall :: Text -> Value -> Maybe (ToolName, Value)
resolveToolCall name args = case parseToolName name of
    Just tn -> Just (tn, args)
    Nothing -> do
        let (tok, rest) = T.break (== ' ') (T.strip name)
        tn <- parseToolName tok
        pure (tn, foldInlineArg tn (dequote rest) args)
  where
    dequote = T.dropAround (\c -> c == '"' || c == '\'' || c == ' ' || c == '`')

foldInlineArg :: ToolName -> Text -> Value -> Value
foldInlineArg tn inline args
    | T.null inline = args
    | Just key <- primaryArgKey tn = insertMissing (Key.fromText key) inline args
    | otherwise = args
  where
    insertMissing k v (Object o)
        | KM.member k o = Object o
        | otherwise = Object (KM.insert k (String v) o)
    insertMissing k v _ = Object (KM.singleton k (String v))

primaryArgKey :: ToolName -> Maybe Text
primaryArgKey = \case
    FindFunction -> Just "query"
    SearchCapability -> Just "query"
    FindExampleCell -> Just "query"
    FindCellsByContent -> Just "pattern"
    CheckType -> Just "expr"
    FindByType -> Just "goal"
    DescribeFunction -> Just "name"
    ApiReference -> Just "module"
    Try -> Just "code"
    EvalLive -> Just "expression"
    _ -> Nothing

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
    Try -> "try"
    Scratchpad -> "scratchpad"
    ListBindings -> "list_bindings"
    CheckType -> "check_type"
    FindByType -> "find_by_type"
    DescribeFunction -> "describe_function"
    ApiReference -> "api_reference"
    ExploreResult -> "explore_result"
    KernelStatus -> "kernel_status"
    Interrupt -> "interrupt"
    KernelRestart -> "kernel_restart"
    AwaitIdle -> "await_idle"
    ExportNotebook -> "export_notebook"
    PeekData -> "peek_data"
    FindExampleCell -> "find_example_cell"
    FindFunction -> "find_function"
    SearchCapability -> "search_capability"
    EvalLive -> "eval_live"

mkTool :: ToolName -> Text -> Value -> ToolDef
mkTool name desc schema = ToolDef (toolWireName name) desc schema Nothing

actsOnNotebook :: ToolName -> Bool
actsOnNotebook InsertCell = True
actsOnNotebook ReplaceCellSource = True
actsOnNotebook DeleteCell = True
actsOnNotebook ProposeEdit = True
actsOnNotebook ExecuteCell = True
actsOnNotebook _ = False
