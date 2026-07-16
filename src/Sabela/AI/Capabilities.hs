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
    needsKernel,

    -- * Edit lifecycle
    acceptEdit,
    revertEdit,
) where

import Control.Exception (SomeException, try)
import Control.Monad (void)
import Data.Aeson (Value (..), (.=))
import Data.IORef (readIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.ApiRef (execApiReference)
import Sabela.AI.Capabilities.CapabilitySearch (execSearchCapability)
import Sabela.AI.Capabilities.Discover (execFindExampleCell, execFindPackage)
import Sabela.AI.Capabilities.Edit (
    execDeleteCell,
    execExecuteCell,
    execInsertCell,
    execProposeEdit,
    execReplaceCellSource,
    executeCell,
 )
import Sabela.AI.Capabilities.Kernel (
    execAwaitIdle,
    execExportNotebook,
    execInterrupt,
    execKernelRestart,
    execKernelStatus,
    haskellKernelBusy,
 )
import Sabela.AI.Capabilities.ModuleSearch (execFindFunction)
import Sabela.AI.Capabilities.Notebook (
    execFindCells,
    execListCells,
    execReadCell,
    execReadCellOutput,
 )
import Sabela.AI.Capabilities.Query (
    execCheckType,
    execDescribeFunction,
    execExploreResult,
    execFindByType,
    execListBindings,
    execPeekData,
 )
import Sabela.AI.Capabilities.Scratchpad (execScratchpadGuarded)
import Sabela.AI.Capabilities.ToolName (ToolName (..), resolveToolCall)
import Sabela.AI.Capabilities.Tools (chatTools)
import Sabela.AI.Capabilities.Util (field, fieldInt)
import Sabela.AI.Store
import Sabela.AI.Types
import Sabela.Anthropic.Types (CancelToken, newCancelToken)
import Sabela.Api (errorJson, errorJsonWith)
import Sabela.Handlers (ReactiveNotebook (..), setCellSourceChecked)
import Sabela.Handlers.Lifecycle (ensureSessionAlive)
import Sabela.Model
import Sabela.Session (Admission (..))
import Sabela.State
import ScriptHs.Parser (CabalMeta (..))

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
executeTool app store rn cancelTok toolName rawInput =
    case lookupParseError rawInput of
        Just parseErr ->
            pure
                ( errOutcome
                    ( errorJsonWith
                        "Could not parse tool arguments."
                        ["hint" .= parseErr]
                    )
                )
        Nothing -> case resolveToolCall toolName rawInput of
            Nothing -> pure (errOutcome (errorJson ("Unknown tool: " <> toolName)))
            Just (tool, input) -> dispatch tool input
  where
    {- Kernel-needing tools take the AI admission gate atomically (one
    'tryTakeMVar'): the busy decision and the hold are a single step, so two
    AI callers can't both observe \"not busy\" and stack behind the run-lock
    (§1.4 TOCTOU). Inside the held gate we still bounce if the kernel is busy
    with non-AI work (a browser run holding 'sbBusy') — the legacy bounce,
    preserved. Lock-free tools (status/interrupt/restart) bypass the gate. -}
    dispatch :: ToolName -> Value -> IO ToolOutcome
    dispatch tool input
        | needsKernel tool =
            admitKernel store (admissionCandidate input) (kernelGuarded tool input)
                >>= \case
                    Busy{} -> pure (errOutcome busyOutcome)
                    Ran r -> pure r
        | otherwise = guarded tool input

    {- Within the held AI gate: warm a cold base GHCi once so discovery tools
    work pre-first-cell, then bounce if a non-AI run (browser/widget) holds the
    kernel run-lock rather than block. -}
    kernelGuarded :: ToolName -> Value -> IO ToolOutcome
    kernelGuarded tool input = do
        warmKernel app
        busy <- haskellKernelBusy app
        if busy then pure (errOutcome busyOutcome) else guarded tool input

    guarded :: ToolName -> Value -> IO ToolOutcome
    guarded tool input = do
        eResult <- try (runTool tool input)
        case eResult of
            Left (e :: SomeException) ->
                pure (errOutcome (errorJson (T.pack (show e))))
            Right r -> pure r

    runTool :: ToolName -> Value -> IO ToolOutcome
    runTool tool input = case tool of
        ListCells -> execListCells app input
        ReadCell -> execReadCell app input
        ReadCellOutput -> execReadCellOutput app input
        FindCellsByContent -> execFindCells app input
        ProposeEdit -> execProposeEdit app store input
        ReplaceCellSource -> execReplaceCellSource app store rn cancelTok input
        InsertCell -> execInsertCell app store rn cancelTok input
        DeleteCell -> execDeleteCell app input
        ExecuteCell -> execExecuteCell app store rn cancelTok input
        Scratchpad -> execScratchpadGuarded app store input
        ListBindings -> execListBindings app input
        CheckType -> execCheckType app input
        FindByType -> execFindByType app input
        DescribeFunction -> execDescribeFunction app input
        ApiReference -> execApiReference app input
        ExploreResult -> execExploreResult store input
        KernelStatus -> execKernelStatus app
        Interrupt -> execInterrupt app
        KernelRestart -> execKernelRestart rn
        AwaitIdle -> execAwaitIdle app
        ExportNotebook -> execExportNotebook app input
        PeekData -> execPeekData app input
        FindPackage -> execFindPackage input
        FindExampleCell -> execFindExampleCell input
        FindFunction -> execFindFunction app input
        SearchCapability -> execSearchCapability app input

{- | Spawn a base GHCi if none is live so kernel-needing discovery tools
(check_type, list_bindings, …) work before the first cell is run. Idempotent:
a no-op when a session already exists, so it never disturbs live work. Warms
with empty metadata at the current generation; a later @-- cabal:@ cell rebuilds
through the normal cell-run path.
-}
warmKernel :: App -> IO ()
warmKernel app = do
    mSess <- getHaskellSession (appSessions app)
    case mSess of
        Just _ -> pure ()
        Nothing -> do
            gen <- readIORef (ebGeneration (appEvents app))
            void (ensureSessionAlive app gen emptyMeta)
  where
    emptyMeta =
        CabalMeta
            { metaDeps = []
            , metaExts = []
            , metaGhcOptions = []
            , metaExtraLibDirs = []
            , metaExtraIncludeDirs = []
            , metaPackages = []
            , metaSourceRepos = []
            , metaUnknownKeys = []
            }

-- | The busy wire shape returned by the live atomic admission gate.
busyOutcome :: Value
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

{- | Candidate id recorded as the admission holder: the tool's @cell_id@ when
present (so a bounced caller's @running@ names the cell), else a sentinel.
-}
admissionCandidate :: Value -> Int
admissionCandidate input = fromMaybe 0 (fieldInt "cell_id" input)

-- | Tools that issue work to the GHCi kernel and so must wait on it.
needsKernel :: ToolName -> Bool
needsKernel = \case
    ExecuteCell -> True
    ReplaceCellSource -> True
    InsertCell -> True
    ListBindings -> True
    CheckType -> True
    FindByType -> True
    DescribeFunction -> True
    FindFunction -> True
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
            committed <- atomicEditNotebook (appNotebook app) $ \nb ->
                case lookupCell (aeCellId edit) nb of
                    Nothing -> (nb, False)
                    Just c -> case setCellSourceChecked c (aeNewSource edit) nb of
                        Left _ -> (nb, False)
                        Right (nb', _) -> (nb', True)
            if not committed
                then pure Nothing
                else do
                    -- 'updateEditStatus' on a terminal state also removes the
                    -- entry from the pending-edits map.
                    updateEditStatus store (aeEditId edit) Accepted
                    broadcastNotebook app
                    -- Block on execution so the HTTP response / tool_result
                    -- carries the actual outputs instead of forcing a poll.
                    ct <- newCancelToken
                    _ <- executeCell app rn (aeCellId edit) ct
                    nb <- readNotebook (appNotebook app)
                    pure (lookupCell (aeCellId edit) nb)

revertEdit :: App -> AIStore -> EditId -> IO ()
revertEdit app store eid = do
    updateEditStatus store eid Reverted
    broadcastNotebook app
