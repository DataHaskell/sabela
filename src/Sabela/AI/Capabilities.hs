{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | AI tool surface: tool catalogue ('chatTools'), name-based dispatch
('executeTool'), and the edit-lifecycle hooks the server's HTTP layer
calls ('acceptEdit', 'revertEdit'). The per-tool implementations live in
the sibling modules:

* "Sabela.AI.Capabilities.Notebook" — read-only inspection.
* "Sabela.AI.Capabilities.Edit" — mutating ops + executeCell.
* "Sabela.AI.Capabilities.Scratchpad" — isolated GHCi/Python sandbox.
* "Sabela.AI.Capabilities.Query" — GHCi introspection + api_reference + explore_result.
* "Sabela.AI.Capabilities.Tools" — the static schemas, split into Notebook + Query sub-modules.
-}
module Sabela.AI.Capabilities (
    -- * Tool definitions
    chatTools,

    -- * Tool execution
    executeTool,
    admissionBlocked,

    -- * Edit lifecycle
    acceptEdit,
    revertEdit,
) where

import Control.Exception (SomeException, try)
import Data.Aeson (Value (..), (.=))
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.Edit (
    execDeleteCell,
    execExecuteCell,
    execInsertCell,
    execProposeEdit,
    execReplaceCellSource,
    executeCell,
 )
import Sabela.AI.Capabilities.Kernel (
    execExportNotebook,
    execInterrupt,
    execKernelRestart,
    execKernelStatus,
    haskellKernelBusy,
 )
import Sabela.AI.Capabilities.Notebook (
    execFindCells,
    execListCells,
    execReadCell,
    execReadCellOutput,
 )
import Sabela.AI.Capabilities.Query (
    execApiReference,
    execExploreResult,
    execGhciQuery,
 )
import Sabela.AI.Capabilities.Scratchpad (execScratchpadGuarded)
import Sabela.AI.Capabilities.ToolName (ToolName (..), parseToolName)
import Sabela.AI.Capabilities.Tools (chatTools)
import Sabela.AI.Capabilities.Util (field)
import Sabela.AI.Store
import Sabela.AI.Types
import Sabela.Anthropic.Types (CancelToken, newCancelToken)
import Sabela.Api (errorJson, errorJsonWith)
import Sabela.Handlers (ReactiveNotebook (..))
import Sabela.Model
import Sabela.State

------------------------------------------------------------------------
-- Tool execution dispatch
------------------------------------------------------------------------

-- 'ToolName' / 'parseToolName' moved to "Sabela.AI.Capabilities.ToolName"
-- so the catalogue in @Sabela.AI.Capabilities.Tools.*@ can import them
-- without creating a cycle. Re-imported above.

executeTool ::
    App ->
    AIStore ->
    ReactiveNotebook ->
    CancelToken ->
    Text ->
    Value ->
    IO ToolOutcome
executeTool app store rn cancelTok toolName input =
    case lookupParseError input of
        Just parseErr ->
            pure
                ( errOutcome
                    ( errorJsonWith
                        "Could not parse tool arguments."
                        ["hint" .= parseErr]
                    )
                )
        Nothing -> case parseToolName toolName of
            Nothing -> pure (errOutcome (errorJson ("Unknown tool: " <> toolName)))
            Just tool -> do
                mBlocked <- admissionBlocked app tool
                case mBlocked of
                    Just busy -> pure busy
                    Nothing -> do
                        eResult <- try (runTool tool)
                        case eResult of
                            Left (e :: SomeException) ->
                                pure (errOutcome (errorJson (T.pack (show e))))
                            Right r -> pure r
  where
    runTool :: ToolName -> IO ToolOutcome
    runTool = \case
        ListCells -> execListCells app
        ReadCell -> execReadCell app input
        ReadCellOutput -> execReadCellOutput app input
        FindCellsByContent -> execFindCells app input
        ProposeEdit -> execProposeEdit app store input
        ReplaceCellSource -> execReplaceCellSource app store rn cancelTok input
        InsertCell -> execInsertCell app store rn cancelTok input
        DeleteCell -> execDeleteCell app input
        ExecuteCell -> execExecuteCell app store rn cancelTok input
        Scratchpad -> execScratchpadGuarded app store input
        GhciQuery -> execGhciQuery app input
        ApiReference -> execApiReference input
        ExploreResult -> execExploreResult store input
        KernelStatus -> execKernelStatus app
        Interrupt -> execInterrupt app
        KernelRestart -> execKernelRestart rn
        ExportNotebook -> execExportNotebook app input

{- | Admission control: a tool that needs the GHCi kernel is refused with a
busy outcome while a cell or query is already running, so the agent reads
'kernel_status'/'interrupt'/'kernel_restart' instead of stacking a second
request behind the run-lock. Lock-free tools (status/interrupt/restart) and
notebook-only tools are never blocked.
-}
admissionBlocked :: App -> ToolName -> IO (Maybe ToolOutcome)
admissionBlocked app tool
    | needsKernel tool = do
        busy <- haskellKernelBusy app
        pure $
            if busy
                then Just (errOutcome busyOutcome)
                else Nothing
    | otherwise = pure Nothing
  where
    busyOutcome =
        errorJsonWith
            "The Haskell kernel is busy running another cell."
            [ "busy" .= True
            , "hint"
                .= ( "Call kernel_status to see if it is still running, or \
                     \interrupt / kernel_restart to free it." ::
                        Text
                   )
            ]

-- | Tools that issue work to the GHCi kernel and so must wait on it.
needsKernel :: ToolName -> Bool
needsKernel = \case
    ExecuteCell -> True
    ReplaceCellSource -> True
    InsertCell -> True
    GhciQuery -> True
    _ -> False

{- | Return the parse-error hint planted by the orchestrator when a streamed
tool_use JSON couldn't be decoded. Dispatch fails fast in that case rather
than letting downstream tools report misleading "cell_id required" errors.
-}
lookupParseError :: Value -> Maybe Text
lookupParseError v = case field "_parseError" v of
    Just (String s) -> Just s
    _ -> Nothing

------------------------------------------------------------------------
-- Edit lifecycle (called by server endpoints)
------------------------------------------------------------------------

{- | Accept a pending AI edit: write the new source, run the cell reactively,
and wait for its execution result before returning. The returned 'Cell' is
re-read from the notebook store AFTER execution, so its @cellOutputs@ /
@cellError@ fields reflect the fresh result — no client-side polling needed.

On timeout (cell takes >130s) we return the cell with whatever state the
notebook store currently has; SSE events will catch the eventual result.
-}
acceptEdit :: App -> AIStore -> ReactiveNotebook -> EditId -> IO (Maybe Cell)
acceptEdit app store rn eid = do
    mEdit <- lookupEdit store eid
    case mEdit of
        Nothing -> pure Nothing
        Just edit -> do
            modifyNotebook (appNotebook app) $ \nb ->
                nb
                    { nbCells =
                        map
                            ( \c ->
                                if cellId c == aeCellId edit
                                    then c{cellSource = aeNewSource edit, cellDirty = True}
                                    else c
                            )
                            (nbCells nb)
                    }
            -- 'updateEditStatus' on a terminal state also removes the
            -- entry from the pending-edits map.
            updateEditStatus store (aeEditId edit) Accepted
            broadcastNotebook app
            -- Block on execution so the HTTP response / tool_result carries
            -- the actual outputs instead of forcing the caller to poll.
            ct <- newCancelToken
            _ <- executeCell app rn (aeCellId edit) ct
            nb <- readNotebook (appNotebook app)
            pure (lookupCell (aeCellId edit) nb)

revertEdit :: App -> AIStore -> EditId -> IO ()
revertEdit app store eid = do
    updateEditStatus store eid Reverted
    broadcastNotebook app
