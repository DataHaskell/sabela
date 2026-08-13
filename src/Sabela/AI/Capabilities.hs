{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sabela.AI.Capabilities (
    chatTools,
    executeTool,
    needsKernel,
    acceptEdit,
    commitAcceptedEdit,
    revertEdit,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (unless, void)
import Data.Aeson (Value (..), (.=))
import Data.IORef (readIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.ApiRef (execApiReference)
import Sabela.AI.Capabilities.Bindings (execListBindings)
import Sabela.AI.Capabilities.CapabilitySearch (execSearchCapability)
import Sabela.AI.Capabilities.Discover (execFindExampleCell)
import Sabela.AI.Capabilities.Edit (
    execDeleteCell,
    execExecuteCell,
    execInsertCell,
    execProposeEdit,
    execReplaceCellSource,
    executeCell,
 )
import Sabela.AI.Capabilities.Edit.Ack (writeGate)
import Sabela.AI.Capabilities.Edit.CompileGate (compileGateCheck)
import Sabela.AI.Capabilities.Files (execListFiles, execReadFile)
import Sabela.AI.Capabilities.Kernel (
    execAwaitIdle,
    execExportNotebook,
    execInterrupt,
    execKernelRestart,
    execKernelStatus,
    execRunPending,
    execSetRunMode,
    haskellKernelOccupied,
 )
import Sabela.AI.Capabilities.KernelHealth (busyEvidenceNow, cellOwner)
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
    execPeekData,
 )
import Sabela.AI.Capabilities.ToolName (ToolName (..), resolveToolCall)
import Sabela.AI.Capabilities.Tools (chatTools)
import Sabela.AI.Capabilities.Try (execTry)
import Sabela.AI.Capabilities.Util (field, fieldInt)
import Sabela.AI.KernelVocab (
    BusyVerdict (..),
    Holding (..),
    busyDenyJson,
    busyRetryRounds,
    resolveOccupied,
 )
import Sabela.AI.Store
import Sabela.AI.Types
import Sabela.Anthropic.Types (CancelToken, newCancelToken)
import Sabela.Api (errorJson, errorJsonWith)
import Sabela.Handlers (ReactiveNotebook (..), setCellSourceChecked)
import Sabela.Handlers.Lifecycle (ensureSessionAlive)
import Sabela.Model
import Sabela.Reactivity (markRootedDirty)
import Sabela.Session.Admission (Admission (..))
import Sabela.State
import ScriptHs.Parser (CabalMeta (..))

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
    dispatch :: ToolName -> Value -> IO ToolOutcome
    dispatch tool input = do
        mAck <- writeGate app store (tool == InsertCell) (needsKernel tool) input
        case mAck of
            Just out -> pure out
            Nothing
                | needsKernel tool ->
                    admitKernel
                        store
                        (admissionCandidate input)
                        (kernelGuarded tool input)
                        >>= \case
                            Busy cid ms -> do
                                owner <- cellOwner app cid
                                pure
                                    ( errOutcome
                                        (busyDenyJson (Just (Holding owner ms)))
                                    )
                            Ran r -> pure r
                | otherwise -> guarded tool input

    kernelGuarded :: ToolName -> Value -> IO ToolOutcome
    kernelGuarded tool input = do
        unless (isTryAlias tool) (warmKernel app)
        v <-
            resolveOccupied
                busyRetryRounds
                (threadDelay 100000)
                (busyEvidenceNow app store (haskellKernelOccupied app))
        case v of
            DenyBusy holder -> pure (errOutcome (busyDenyJson holder))
            _ -> guarded tool input

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
        Try -> execTry app input
        Scratchpad -> execTry app input
        ListBindings -> execListBindings app input
        CheckType -> execCheckType app input
        FindByType -> execFindByType app input
        DescribeFunction -> execDescribeFunction app input
        ApiReference -> execApiReference app input
        ExploreResult -> execExploreResult store input
        KernelStatus -> execKernelStatus app
        SetRunMode -> execSetRunMode app rn input
        RunPending -> execRunPending app rn
        Interrupt -> execInterrupt app store
        KernelRestart -> execKernelRestart app rn
        AwaitIdle -> execAwaitIdle app store
        ExportNotebook -> execExportNotebook app input
        PeekData -> execPeekData app input
        EvalLive -> execTry app input
        FindExampleCell -> execFindExampleCell input
        FindFunction -> execFindFunction app input
        SearchCapability -> execSearchCapability app input
        ListFiles -> execListFiles app input
        ReadFile -> execReadFile app input

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

admissionCandidate :: Value -> Int
admissionCandidate input = fromMaybe 0 (fieldInt "cell_id" input)

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
    Try -> True
    Scratchpad -> True
    EvalLive -> True
    _ -> False

isTryAlias :: ToolName -> Bool
isTryAlias Try = True
isTryAlias Scratchpad = True
isTryAlias EvalLive = True
isTryAlias _ = False

lookupParseError :: Value -> Maybe Text
lookupParseError v = case field "_parseError" v of
    Just (String s) -> Just s
    _ -> Nothing

-- | The rejection's own error text, bounded; a patch refusal must say why.
gateRejectionSummary :: Value -> Text
gateRejectionSummary v = case field "error" v of
    Just (String s) -> T.take 400 s
    _ -> T.take 400 (T.pack (show v))

acceptEdit :: App -> AIStore -> ReactiveNotebook -> EditId -> IO (Maybe Cell)
acceptEdit app store rn eid = do
    mEdit <- lookupEdit store eid
    case mEdit of
        Nothing -> pure Nothing
        Just edit -> do
            nb0 <- readNotebook (appNotebook app)
            case lookupCell (aeCellId edit) nb0 of
                Nothing -> pure Nothing
                Just c -> do
                    gate <-
                        compileGateCheck
                            app
                            (Just (aeCellId edit))
                            (cellLang c)
                            (cellType c)
                            (aeNewSource edit)
                    case gate of
                        Left rejection -> do
                            broadcast
                                (appEvents app)
                                ( EvChatError
                                    Nothing
                                    ( "Accepting the patch for cell "
                                        <> T.pack (show (aeCellId edit))
                                        <> " was refused by the compile gate: "
                                        <> gateRejectionSummary rejection
                                    )
                                )
                            pure Nothing
                        Right () -> commitAcceptedEdit app store rn edit

commitAcceptedEdit ::
    App -> AIStore -> ReactiveNotebook -> AiEdit -> IO (Maybe Cell)
commitAcceptedEdit app store rn edit = do
    committed <- atomicEditNotebook (appNotebook app) $ \nb ->
        case lookupCell (aeCellId edit) nb of
            Nothing -> (nb, False)
            Just c -> case setCellSourceChecked c (aeNewSource edit) nb of
                Left _ -> (nb, False)
                Right (nb', _) -> (nb', True)
    if not committed
        then pure Nothing
        else do
            updateEditStatus store (aeEditId edit) Accepted
            broadcastNotebook app
            mode <- getRunMode app
            case mode of
                RunDeferred -> do
                    modifyNotebook
                        (appNotebook app)
                        (markRootedDirty (aeCellId edit))
                    broadcastNotebookState app
                RunReactive -> do
                    ct <- newCancelToken
                    _ <- executeCell app rn (aeCellId edit) ct
                    pure ()
            nb <- readNotebook (appNotebook app)
            pure (lookupCell (aeCellId edit) nb)

revertEdit :: App -> AIStore -> EditId -> IO ()
revertEdit app store eid = do
    updateEditStatus store eid Reverted
    broadcastNotebook app
