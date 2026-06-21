{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | The agentic loop: stream a 'MessagesRequest' to Anthropic, accumulate
the response, dispatch any @tool_use@ blocks, and iterate until the model
emits @end_turn@ (or we hit the tool budget). Helpers around streaming,
content finalisation, tool dispatch, and tool-result compaction live
here too — they form one cohesive unit with 'agenticLoop'.

The entry points in 'Sabela.AI.Orchestrator' are the only callers.
-}
module Sabela.AI.Orchestrator.Loop (
    -- * Loop
    agenticLoop,
    maxToolIterations,

    -- * Helpers (exposed for test mirrors and the entry-points module)
    mergeUsage,
) where

import Control.Concurrent.STM (atomically, writeTVar)
import Control.Monad (forM_, unless, when)
import Data.Aeson (Value (..), decode, object, (.=))
import Data.IORef (atomicModifyIORef', readIORef)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import Sabela.AI.Capabilities (chatTools, executeTool)
import Sabela.AI.Capabilities.Kernel (kernelStateBefore)
import Sabela.AI.Orchestrator.Compact (compactToolResult, resultToText)
import Sabela.AI.Orchestrator.Prompt (buildNotebookDocText, systemPrompt)
import Sabela.AI.Provenance (Actor (..), recordToolCall)
import Sabela.AI.Store
import Sabela.AI.Types
import Sabela.Anthropic
import Sabela.Handlers (ReactiveNotebook)
import Sabela.Model (NotebookEvent (..))
import Sabela.State (App (..))
import Sabela.State.Environment (Environment (..))
import Sabela.State.EventBus (broadcast)

-- | Per-turn cap on tool-call rounds; the agent fails the turn beyond this.
maxToolIterations :: Int
maxToolIterations = 25

agenticLoop :: App -> AIStore -> ReactiveNotebook -> Turn -> IO ()
agenticLoop app store rn turn = go
  where
    tid = turnId turn

    go = do
        -- Check cancellation
        cancelled <- isCancelled (turnCancel turn)
        when cancelled $ do
            atomically $ writeTVar (turnPhase turn) TurnCancelled
            return ()
        unless cancelled $ do
            -- Build request
            messages <- getMessages store
            nbDocJson <- buildNotebookDocText app
            cfg <- getAIConfig store
            let sysBlocks =
                    -- systemPrompt + reference card is super-stable; use the
                    -- 1-hour TTL so chat sessions with long read-think pauses
                    -- don't drop the cache every 5 minutes.
                    [ SystemBlock systemPrompt (Just EphemeralHour)
                    , SystemBlock nbDocJson (Just Ephemeral)
                    ]
                req =
                    MessagesRequest
                        { mrModel = acModel cfg
                        , -- Safety cap, not a billing knob — Anthropic bills
                          -- per emitted token. Capping low risks silently
                          -- truncating large tool_use payloads (e.g. a long
                          -- 'propose_edit' new_source), which breaks the JSON
                          -- and burns an iteration on retry.
                          mrMaxTokens = 4096
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
                    cfg
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
                    -- Bump iteration count (one assistant response received).
                    atomicModifyIORef' (turnIterations turn) (\n -> (n + 1, ()))
                    -- Accumulate usage both globally (session) and on the turn
                    -- so the frontend can show per-turn and cumulative stats.
                    case mrsUsage resp of
                        Just u -> do
                            atomicModifyIORef' (aiUsage store) (\old -> (mergeUsage old u, ()))
                            atomicModifyIORef' (turnUsage turn) (\old -> (mergeUsage old u, ()))
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
                                        EvChatError (Just tid) "Tool use limit reached (25)"
                                    atomically $
                                        writeTVar (turnPhase turn) (TurnFailed "Tool limit")
                                else do
                                    executeToolCalls app store rn turn finalContent
                                    go
                        Nothing ->
                            atomically $ writeTVar (turnPhase turn) (TurnComplete SREndTurn)

{- | Add two 'Usage' records componentwise. @Nothing@ cache fields collapse to
@Just 0@ once either side starts reporting them, so the UI never has to guess.
-}
mergeUsage :: Usage -> Usage -> Usage
mergeUsage a b =
    Usage
        { uInputTokens = uInputTokens a + uInputTokens b
        , uOutputTokens = uOutputTokens a + uOutputTokens b
        , uCacheCreationInputTokens =
            addMaybeInt (uCacheCreationInputTokens a) (uCacheCreationInputTokens b)
        , uCacheReadInputTokens =
            addMaybeInt (uCacheReadInputTokens a) (uCacheReadInputTokens b)
        }
  where
    addMaybeInt Nothing Nothing = Nothing
    addMaybeInt x y = Just (fromMaybe 0 x + fromMaybe 0 y)

-- | Handle a streaming event — broadcast text deltas to the frontend.
handleStreamEvent :: App -> TurnId -> StreamEvent -> IO ()
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
            Nothing ->
                -- Streamed tool_use JSON failed to parse. The most common
                -- cause is a max_tokens cap clipping the payload mid-value.
                -- We still dispatch so the model gets an informative error
                -- it can act on, rather than a silent no-op.
                ToolUseBlock
                    tubid
                    name
                    ( object
                        [ "_parseError"
                            .= ( "Tool input JSON failed to parse — almost certainly "
                                    <> "truncated mid-generation. Split the payload across "
                                    <> "multiple tool calls (e.g. smaller propose_edit, or "
                                    <> "insert an empty cell then patch it in a follow-up)." ::
                                    Text
                               )
                        , "raw" .= jsonStr
                        ]
                    )
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
        tid = turnId turn
    forM_ toolUses $ \(tcIdText, toolName, input) -> do
        -- Check cancellation before each tool
        cancelled <- isCancelled (turnCancel turn)
        unless cancelled $ do
            atomicModifyIORef' (turnToolCount turn) (\n -> (n + 1, ()))
            let tcId = ToolCallId tcIdText
            broadcast (appEvents app) $
                EvChatToolCall tid tcId toolName input
            (kBefore, gen) <- kernelStateBefore app
            outcome <-
                executeTool app store rn (turnCancel turn) toolName input
            -- The in-browser chat bypasses 'aiToolH', so record its own
            -- provenance here (actor=InBrowserChat, browser-session sentinel).
            recordToolCall
                (envWorkDir (appEnv app))
                Nothing
                InBrowserChat
                toolName
                input
                outcome
                kBefore
                gen
            let result = toolOutcomeValue outcome
                isErr = toolOutcomeIsError outcome
            -- Broadcast raw (un-compacted) result to the UI so users see full
            -- content; only the LLM history gets the compacted form.
            broadcast (appEvents app) $
                EvChatToolResult tid tcId result
            compacted <- compactToolResult store result
            let resultBlock =
                    ToolResultBlock
                        tcIdText
                        isErr
                        [TextBlock (resultToText compacted)]
            appendMessage store (Message RoleUser [resultBlock])

{- | Extract tool_use blocks from content. The first 'Text' is the
Anthropic-supplied tool-use id; downstream we wrap it in 'ToolCallId'
when we cross the typed-event boundary.
-}
extractToolUses :: [ContentBlock] -> [(Text, Text, Value)]
extractToolUses = mapMaybe extract
  where
    extract (ToolUseBlock tid name input) = Just (tid, name, input)
    extract _ = Nothing
