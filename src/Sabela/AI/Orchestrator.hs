{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Public entry points for the AI chat orchestrator. The agentic loop
itself, the streaming/tool-dispatch helpers, and the system prompt live
in 'Sabela.AI.Orchestrator.Loop' and 'Sabela.AI.Orchestrator.Prompt'.
-}
module Sabela.AI.Orchestrator (
    handleChatMessage,
    handleCancelTurn,
    handleClearChat,
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (readTVarIO)
import Control.Exception (SomeException, try)
import Control.Monad (void)
import Data.Aeson (object, (.=))
import Data.IORef (readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (diffUTCTime, getCurrentTime)

import Sabela.AI.Handles (clearHandles)
import Sabela.AI.Orchestrator.Loop (agenticLoop)
import Sabela.AI.Store
import Sabela.AI.Types
import Sabela.Anthropic.Types (Usage (..))
import Sabela.Handlers (ReactiveNotebook)
import Sabela.LLM.Cancel (cancel)
import Sabela.LLM.Message (ContentPart (..), Message (..), Role (..))
import Sabela.Model (NotebookEvent (..))
import Sabela.State (App (..))
import Sabela.State.EventBus (broadcast)

handleChatMessage :: App -> AIStore -> ReactiveNotebook -> Text -> IO ()
handleChatMessage app store rn userText = do
    mActive <- getCurrentTurn store
    case mActive of
        Just _ ->
            broadcast (appEvents app) (EvChatError Nothing "A turn is already in progress")
        Nothing -> do
            turn <- newTurn (aiNextTurnId store)
            setCurrentTurn store turn
            appendMessage store (Message User [TextPart userText])
            void $ forkIO $ do
                let tid = turnId turn
                eResult <- try $ agenticLoop app store rn turn
                case eResult of
                    Left (e :: SomeException) ->
                        broadcast (appEvents app) (EvChatError (Just tid) (T.pack (show e)))
                    Right () -> pure ()
                clearCurrentTurn store
                clearHandles (aiHandles store)
                emitTurnUsage app turn
                phase <- readTVarIO (turnPhase turn)
                case phase of
                    TurnCancelled ->
                        broadcast (appEvents app) (EvChatCancelled tid)
                    TurnFailed msg ->
                        broadcast (appEvents app) (EvChatError (Just tid) msg)
                    _ ->
                        broadcast (appEvents app) (EvChatDone tid)

{- | Emit an 'EvChatUsageUpdate' summarising token usage and elapsed
wall-clock time for the turn. Called once per turn after the agentic
loop finishes.
-}
emitTurnUsage :: App -> Turn -> IO ()
emitTurnUsage app turn = do
    let tid = turnId turn
    u <- readIORef (turnUsage turn)
    iters <- readIORef (turnIterations turn)
    toolCount <- readIORef (turnToolCount turn)
    now <- getCurrentTime
    let wallMs :: Int
        wallMs = floor (realToFrac (diffUTCTime now (turnStartedAt turn)) * 1000 :: Double)
        payload =
            object
                [ "inputTokens" .= uInputTokens u
                , "outputTokens" .= uOutputTokens u
                , "cacheCreationInputTokens" .= uCacheCreationInputTokens u
                , "cacheReadInputTokens" .= uCacheReadInputTokens u
                , "iterations" .= iters
                , "toolCalls" .= toolCount
                , "wallTimeMs" .= wallMs
                ]
    broadcast (appEvents app) (EvChatUsageUpdate tid payload)

handleCancelTurn :: App -> AIStore -> IO ()
handleCancelTurn _app store = do
    mTurn <- getCurrentTurn store
    case mTurn of
        Just turn -> cancel (turnCancel turn)
        Nothing -> pure ()

handleClearChat :: App -> AIStore -> IO ()
handleClearChat _app store = do
    clearConversation store
    revertAllPendingEdits store
    clearScratchpad store
