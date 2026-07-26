{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sabela.AI.Orchestrator.Loop (
    agenticLoop,
    maxToolIterations,
    mergeUsage,
    downstreamDependents,
    looksLikeToolCallText,
) where

import Control.Concurrent.STM (atomically, writeTVar)
import Control.Monad (unless, when)
import Data.Aeson (object, (.=))
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.Tools (chatToolSpecs)
import Sabela.AI.Capabilities.Util (featureEnabled)
import Sabela.AI.Discover (discoverGrammar, importedModules)
import Sabela.AI.Orchestrator.Loop.Accept (
    downstreamDependents,
    liveOwned,
    looksLikeToolCallText,
    maxReenter,
    reenterNudge,
    toolCallNudge,
    verifyDownstream,
 )
import Sabela.AI.Orchestrator.Loop.Dispatch (executeToolCalls)
import Sabela.AI.Orchestrator.Loop.Usage (
    accumulateUsage,
    mergeUsage,
    toStopReason,
 )
import Sabela.AI.Orchestrator.Prompt (buildNotebookDocText, systemPrompt)
import Sabela.AI.Orchestrator.Trace (traceEvent, traceReset)
import Sabela.AI.Owned (StopDecision (..), stopDecision)
import Sabela.AI.Salvage (salvageInsertSource)
import Sabela.AI.Store
import Sabela.AI.Types
import Sabela.Anthropic.Types (StopReason (..))
import Sabela.Handlers (ReactiveNotebook)
import Sabela.LLM.Cancel (isCancelled)
import Sabela.LLM.Completion (Completion (..), StopCondition (..))
import Sabela.LLM.Message (
    ContentPart (..),
    Message (..),
    Role (..),
    ToolCall (..),
 )
import Sabela.LLM.Provider (
    ChunkSink (..),
    CompletionRequest (..),
    ModelProvider (..),
    ProviderCaps (..),
 )
import Sabela.Model (NotebookEvent (..), cellSource, nbCells)
import Sabela.SessionTypes (sbQueryBrowse)
import Sabela.State (
    App (..),
    defaultToolLimit,
    getHaskellSession,
    readNotebook,
 )
import Sabela.State.EventBus (broadcast)

maxToolIterations :: Int
maxToolIterations = defaultToolLimit

salvageInsertPart :: Text -> ContentPart
salvageInsertPart src =
    ToolCallPart
        (ToolCall (ToolCallId "salvage") "insert_cell" (object ["source" .= src]))

discoverLiveGrammar :: App -> IO (Maybe Text)
discoverLiveGrammar app = do
    on <- featureEnabled "SABELA_LIVE_GRAMMAR"
    mBackend <- getHaskellSession (appSessions app)
    case (on, mBackend) of
        (True, Just backend) -> do
            nb <- readNotebook (appNotebook app)
            let mods = nub (concatMap (importedModules . cellSource) (nbCells nb))
            discoverGrammar (sbQueryBrowse backend) mods
        _ -> pure Nothing

agenticLoop :: App -> AIStore -> ReactiveNotebook -> Turn -> IO ()
agenticLoop app store rn turn = do
    traceReset app
    ownedRef <- newIORef Map.empty
    liveGrammar <- discoverLiveGrammar app
    traceEvent
        app
        ["kind" .= ("start" :: Text), "grammar" .= isJust liveGrammar]
    go ownedRef liveGrammar False 0
  where
    tid = turnId turn

    go ownedRef liveGrammar salvaged reenters = do
        cancelled <- isCancelled (turnCancel turn)
        when cancelled $ atomically $ writeTVar (turnPhase turn) TurnCancelled
        unless cancelled $ do
            messages <- getMessages store
            nbDocJson <- buildNotebookDocText app
            provider <- getAIProvider store
            let req =
                    CompletionRequest
                        { crSystem = [systemPrompt, nbDocJson] ++ maybe [] pure liveGrammar
                        , crMessages = messages
                        , crTools = chatToolSpecs
                        , crMaxTokens = 4096
                        }
            atomically $ writeTVar (turnPhase turn) TurnStreaming
            eResp <-
                mpComplete
                    provider
                    req
                    (turnCancel turn)
                    (ChunkSink (broadcast (appEvents app) . EvChatTextDelta tid))
            case eResp of
                Left err -> do
                    traceEvent app ["kind" .= ("error" :: Text), "msg" .= err]
                    atomically $ writeTVar (turnPhase turn) (TurnFailed err)
                Right comp -> onCompletion ownedRef liveGrammar salvaged reenters provider comp

    onCompletion ownedRef liveGrammar salvaged reenters provider comp = do
        let parts = compParts comp
        unless (capStreaming (mpCaps provider)) $
            let txt = T.concat [t | TextPart t <- parts]
             in unless (T.null txt) $
                    broadcast (appEvents app) (EvChatTextDelta tid txt)
        appendMessage store (Message Assistant parts)
        atomicModifyIORef' (turnIterations turn) (\n -> (n + 1, ()))
        accumulateUsage store turn (compUsage comp)
        toolCount <- readIORef (turnToolCount turn)
        traceEvent
            app
            [ "kind" .= ("model" :: Text)
            , "stop" .= T.pack (show (compStop comp))
            , "toolCount" .= toolCount
            , "tools" .= [tcName tc | ToolCallPart tc <- parts]
            , "text" .= T.take 500 (T.concat [t | TextPart t <- parts])
            ]
        case compStop comp of
            WantsTools -> onWantsTools ownedRef liveGrammar salvaged reenters toolCount parts
            Done -> handleDone ownedRef liveGrammar salvaged reenters parts
            stop ->
                atomically $
                    writeTVar (turnPhase turn) (TurnComplete (toStopReason stop))

    onWantsTools ownedRef liveGrammar salvaged reenters toolCount parts = do
        limit <- readIORef (appAIToolLimit app)
        if toolCount >= limit
            then do
                traceEvent app ["kind" .= ("toollimit" :: Text), "limit" .= limit]
                broadcast (appEvents app) $
                    EvChatError
                        (Just tid)
                        ("Tool use limit reached (" <> T.pack (show limit) <> ")")
                atomically $ writeTVar (turnPhase turn) (TurnFailed "Tool limit")
            else do
                executeToolCalls app store rn turn ownedRef parts
                go ownedRef liveGrammar salvaged reenters

    handleDone ownedRef liveGrammar salvaged reenters parts = do
        owned <- readIORef ownedRef
        let content = T.concat [t | TextPart t <- parts]
            mSalvage =
                if salvaged then Nothing else salvageInsertSource (Map.size owned) content
        case mSalvage of
            Just src -> do
                traceEvent app ["kind" .= ("salvage" :: Text)]
                executeToolCalls app store rn turn ownedRef [salvageInsertPart src]
                go ownedRef liveGrammar True reenters
            Nothing
                | reenters < maxReenter
                , looksLikeToolCallText content -> do
                    traceEvent
                        app
                        ["kind" .= ("nudge" :: Text), "reason" .= ("tool-call-in-text" :: Text)]
                    appendMessage store (Message User [TextPart toolCallNudge])
                    go ownedRef liveGrammar salvaged (reenters + 1)
                | otherwise -> acceptOrReenter ownedRef liveGrammar salvaged reenters owned

    acceptOrReenter ownedRef liveGrammar salvaged reenters owned = do
        reenterOn <- featureEnabled "SABELA_SELF_HEAL_REENTER"
        live <- liveOwned app owned
        downstream <-
            if reenterOn then verifyDownstream app rn turn owned else pure Map.empty
        let decision = stopDecision (live <> downstream)
        traceEvent
            app
            [ "kind" .= ("done" :: Text)
            , "decision" .= T.pack (show decision)
            , "reenterOn" .= reenterOn
            , "reenters" .= reenters
            ]
        case decision of
            Reenter reds
                | reenterOn
                , reenters < maxReenter -> do
                    appendMessage store (Message User [TextPart (reenterNudge reds)])
                    go ownedRef liveGrammar salvaged (reenters + 1)
            _ -> atomically $ writeTVar (turnPhase turn) (TurnComplete SREndTurn)
