{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sabela.AI.Orchestrator (
    handleChatMessage,
    handleCancelTurn,
    handleClearChat,
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically, readTVarIO, writeTVar)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, void, when)
import Data.Aeson (Value (..), decode, encode, object, (.=))
import Data.IORef (atomicModifyIORef', readIORef)
import Data.Maybe (isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import Sabela.AI.Capabilities (chatTools, executeTool)
import Sabela.AI.Store
import Sabela.AI.Types
import Sabela.Anthropic
import Sabela.Handlers (ReactiveNotebook)
import Sabela.Model (
    Cell (..),
    CellType (..),
    Notebook (..),
    NotebookEvent (..),
 )
import Sabela.State (App (..))
import Sabela.State.EventBus (broadcast)
import Sabela.State.NotebookStore (readNotebook)

maxToolIterations :: Int
maxToolIterations = 25

------------------------------------------------------------------------
-- Entry points
------------------------------------------------------------------------

handleChatMessage :: App -> AIStore -> ReactiveNotebook -> Text -> IO ()
handleChatMessage app store rn userText = do
    mActive <- getCurrentTurn store
    when (isJust mActive) $ do
        broadcast (appEvents app) (EvChatError 0 "A turn is already in progress")
        return ()
    unless (isJust mActive) $ do
        turn <- newTurn (aiNextTurnId store)
        setCurrentTurn store turn
        -- Append user message
        appendMessage store (Message RoleUser [TextBlock userText])
        -- Run agentic loop in background
        void $ forkIO $ do
            let TurnId tid = turnId turn
            eResult <- try $ agenticLoop app store rn turn
            case eResult of
                Left (e :: SomeException) ->
                    broadcast (appEvents app) (EvChatError tid (T.pack (show e)))
                Right () -> pure ()
            clearCurrentTurn store
            -- Broadcast terminal event based on phase
            phase <- readTVarIO (turnPhase turn)
            case phase of
                TurnCancelled ->
                    broadcast (appEvents app) (EvChatCancelled tid)
                TurnFailed msg ->
                    broadcast (appEvents app) (EvChatError tid msg)
                _ ->
                    broadcast (appEvents app) (EvChatDone tid)

handleCancelTurn :: App -> AIStore -> IO ()
handleCancelTurn _app store = do
    mTurn <- getCurrentTurn store
    case mTurn of
        Nothing -> pure ()
        Just turn -> do
            cancel (turnCancel turn)
            atomically $ writeTVar (turnPhase turn) TurnCancelled

handleClearChat :: App -> AIStore -> IO ()
handleClearChat app store = do
    -- Cancel any active turn
    handleCancelTurn app store
    clearConversation store
    revertAllPendingEdits store
    clearScratchpad store

------------------------------------------------------------------------
-- Agentic loop
------------------------------------------------------------------------

agenticLoop :: App -> AIStore -> ReactiveNotebook -> Turn -> IO ()
agenticLoop app store rn turn = go
  where
    TurnId tid = turnId turn

    go = do
        -- Check cancellation
        cancelled <- isCancelled (turnCancel turn)
        when cancelled $ do
            atomically $ writeTVar (turnPhase turn) TurnCancelled
            return ()
        unless cancelled $ do
            -- Build request
            messages <- getMessages store
            nbSnapshot <- buildNotebookSnapshot app
            let sysBlocks =
                    [ SystemBlock systemPrompt (Just Ephemeral)
                    , SystemBlock nbSnapshot Nothing
                    ]
                req =
                    MessagesRequest
                        { mrModel = acModel (aiConfig store)
                        , mrMaxTokens = 4096
                        , mrSystem = sysBlocks
                        , mrMessages = messages
                        , mrTools = chatTools
                        , mrStream = True
                        }

            -- Stream response
            atomically $ writeTVar (turnPhase turn) TurnStreaming
            eResp <-
                streamMessages
                    (aiHttpManager store)
                    (aiConfig store)
                    req
                    (turnCancel turn)
                    (handleStreamEvent app tid)

            case eResp of
                Left err -> do
                    atomically $ writeTVar (turnPhase turn) (TurnFailed err)
                Right resp -> do
                    -- Finalize tool_use blocks: parse accumulated JSON
                    let finalContent = finalizeContent (mrsContent resp)
                    -- Append assistant message to history
                    appendMessage store (Message RoleAssistant finalContent)
                    -- Update usage
                    case mrsUsage resp of
                        Just u ->
                            atomicModifyIORef' (aiUsage store) $ \old ->
                                ( old
                                    { uInputTokens = uInputTokens old + uInputTokens u
                                    , uOutputTokens = uOutputTokens old + uOutputTokens u
                                    }
                                , ()
                                )
                        Nothing -> pure ()

                    case mrsStopReason resp of
                        Just SREndTurn ->
                            atomically $ writeTVar (turnPhase turn) (TurnComplete SREndTurn)
                        Just SRMaxTokens ->
                            atomically $ writeTVar (turnPhase turn) (TurnComplete SRMaxTokens)
                        Just SRToolUse -> do
                            count <- readIORef (turnToolCount turn)
                            if count >= maxToolIterations
                                then do
                                    broadcast (appEvents app) $
                                        EvChatError tid "Tool use limit reached (25)"
                                    atomically $
                                        writeTVar (turnPhase turn) (TurnFailed "Tool limit")
                                else do
                                    executeToolCalls app store rn turn finalContent
                                    go
                        Nothing ->
                            atomically $ writeTVar (turnPhase turn) (TurnComplete SREndTurn)

-- | Handle a streaming event — broadcast text deltas to the frontend.
handleStreamEvent :: App -> Int -> StreamEvent -> IO ()
handleStreamEvent app tid ev = case ev of
    SEContentBlockDelta _ (TextDelta text) ->
        broadcast (appEvents app) (EvChatTextDelta tid text)
    _ -> pure ()

-- | Finalize content blocks: parse accumulated JSON strings in ToolUseBlocks.
finalizeContent :: [ContentBlock] -> [ContentBlock]
finalizeContent = filter (not . isEmptyText) . map finalize
  where
    finalize (ToolUseBlock tubid name (String jsonStr)) =
        case decodeJson jsonStr of
            Just val -> ToolUseBlock tubid name val
            Nothing -> ToolUseBlock tubid name (object ["raw" .= jsonStr])
    finalize other = other

    decodeJson :: Text -> Maybe Value
    decodeJson t =
        let bs = TLE.encodeUtf8 (TL.fromStrict t)
         in decode bs

    isEmptyText (TextBlock t) = T.null t
    isEmptyText _ = False

-- | Execute tool calls from an assistant response.
executeToolCalls ::
    App -> AIStore -> ReactiveNotebook -> Turn -> [ContentBlock] -> IO ()
executeToolCalls app store rn turn content = do
    let toolUses = extractToolUses content
        TurnId tid = turnId turn
    forM_ toolUses $ \(tcId, toolName, input) -> do
        -- Check cancellation before each tool
        cancelled <- isCancelled (turnCancel turn)
        unless cancelled $ do
            -- Bump tool count
            atomicModifyIORef' (turnToolCount turn) (\n -> (n + 1, ()))
            -- Broadcast tool call
            broadcast (appEvents app) $
                EvChatToolCall tid tcId toolName input
            -- Execute tool
            (result, isErr) <-
                executeTool app store rn (turnCancel turn) toolName input
            -- Broadcast result
            broadcast (appEvents app) $
                EvChatToolResult tid tcId result
            -- Build tool result content block
            let truncatedResult = truncateResult result
                resultBlock =
                    ToolResultBlock
                        tcId
                        isErr
                        [TextBlock (resultToText truncatedResult)]
            -- Append to conversation
            appendMessage store (Message RoleUser [resultBlock])

-- | Extract tool_use blocks from content.
extractToolUses :: [ContentBlock] -> [(Text, Text, Value)]
extractToolUses = mapMaybe extract
  where
    extract (ToolUseBlock tid name input) = Just (tid, name, input)
    extract _ = Nothing

-- | Truncate large tool results to avoid context bloat.
truncateResult :: Value -> Value
truncateResult v =
    let text = resultToText v
     in if T.length text > 2000
            then String (T.take 2000 text <> "\n...[truncated]")
            else v

-- | Convert a JSON value to text for tool result content.
resultToText :: Value -> Text
resultToText (String s) = s
resultToText v = TL.toStrict (TLE.decodeUtf8 (encode v))

------------------------------------------------------------------------
-- System prompt
------------------------------------------------------------------------

systemPrompt :: Text
systemPrompt =
    T.unlines
        [ "You are an AI assistant embedded in Sabela, a reactive notebook environment."
        , "The notebook supports Haskell, Python, and Lean4 code cells mixed with Markdown prose."
        , ""
        , "## Available tools"
        , "- list_cells: Get an overview of all cells"
        , "- read_cell: Read full source and outputs of a specific cell"
        , "- read_cell_output: Read only outputs/errors of a cell"
        , "- find_cells_by_content: Search cell sources for a pattern"
        , "- propose_edit: Propose a source code change (user must accept)"
        , "- insert_cell: Insert a new cell"
        , "- delete_cell: Delete a cell"
        , "- execute_cell: Run a cell and get its results"
        , "- scratchpad: Run code in an isolated session (does not modify notebook)"
        , ""
        , "## Guidelines"
        , "- Start by using list_cells to understand the notebook structure."
        , "- Use read_cell to examine specific cells when needed."
        , "- When the user asks you to modify code, use propose_edit. The user will decide whether to accept."
        , "- Use execute_cell when you need to verify that code works."
        , "- Use scratchpad for standalone experiments that don't depend on notebook state."
        , "- The scratchpad has the same packages but separate state from the notebook."
        , "- If you need to test code that depends on notebook bindings, use execute_cell instead."
        , "- Be concise in your responses. Show code in your text when explaining."
        ]

-- | Build a notebook snapshot for the system prompt.
buildNotebookSnapshot :: App -> IO Text
buildNotebookSnapshot app = do
    nb <- readNotebook (appNotebook app)
    let header = "Current notebook: " <> nbTitle nb
        cellLines = map summarizeCell (nbCells nb)
    pure $ T.unlines (header : "" : cellLines)
  where
    summarizeCell c =
        let prefix = case cellType c of
                CodeCell -> "[" <> T.pack (show (cellLang c)) <> "]"
                ProseCell -> "[Prose]"
            firstLine = T.take 60 (head' (T.lines (cellSource c)))
            errFlag = if isJust (cellError c) then " ERROR" else ""
         in "  Cell "
                <> T.pack (show (cellId c))
                <> " "
                <> prefix
                <> ": "
                <> firstLine
                <> errFlag
    head' [] = "(empty)"
    head' (x : _) = x
