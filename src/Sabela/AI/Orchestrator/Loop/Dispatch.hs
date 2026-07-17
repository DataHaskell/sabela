{-# LANGUAGE OverloadedStrings #-}

{- | Tool-call dispatch for the agentic loop: run each call the model asked for,
broadcast it, record provenance, and append the compacted result. Split from
"Sabela.AI.Orchestrator.Loop" to keep each module under the size cap.
-}
module Sabela.AI.Orchestrator.Loop.Dispatch (
    executeToolCalls,
    cellIdOf,
    okOf,
) where

import Control.Monad (forM_, unless)
import Data.Aeson (Value (..), (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (IORef, atomicModifyIORef', modifyIORef')
import Data.Map.Strict (Map)
import Data.Text (Text)

import Sabela.AI.Capabilities (executeTool)
import Sabela.AI.Capabilities.Kernel (kernelStateBefore)
import Sabela.AI.Capabilities.ToolName (actsOnNotebook, parseToolName)
import Sabela.AI.Health (healthOfCellError)
import Sabela.AI.Orchestrator.Compact (compactToolResult)
import Sabela.AI.Orchestrator.Trace (snip, traceEvent)
import Sabela.AI.Owned (MutationEvent (..), OwnedCell (..), recordMutation)
import Sabela.AI.Provenance (Actor (..), recordToolCall)
import Sabela.AI.Store
import Sabela.AI.Types
import Sabela.Handlers (ReactiveNotebook)
import Sabela.LLM.Cancel (isCancelled)
import Sabela.LLM.Message (
    ContentPart (..),
    Message (..),
    Role (..),
    ToolCall (..),
    ToolResult (..),
 )
import Sabela.Model (NotebookEvent (..), cellSource, lookupCell)
import Sabela.State (App (..), readNotebook)
import Sabela.State.Environment (Environment (..))
import Sabela.State.EventBus (broadcast)

{- | Dispatch the assistant turn's tool calls, broadcasting each call + result to
the UI and appending a neutral 'ToolResult' (compacted, error flag preserved) to
the conversation for the next turn.
-}
executeToolCalls ::
    App ->
    AIStore ->
    ReactiveNotebook ->
    Turn ->
    IORef (Map Int OwnedCell) ->
    [ContentPart] ->
    IO ()
executeToolCalls app store rn turn ownedRef parts = do
    let tid = turnId turn
    forM_ [tc | ToolCallPart tc <- parts] $ \tc -> do
        cancelled <- isCancelled (turnCancel turn)
        unless cancelled $ do
            atomicModifyIORef' (turnToolCount turn) (\n -> (n + 1, ()))
            let cid = tcId tc
                nm = tcName tc
                input = tcInput tc
            broadcast (appEvents app) $ EvChatToolCall tid cid nm input
            (kBefore, gen) <- kernelStateBefore app
            outcome <- executeTool app store rn (turnCancel turn) nm input
            recordOwnedMutation app ownedRef tc outcome
            traceEvent
                app
                [ "kind" .= ("tool" :: Text)
                , "name" .= nm
                , "isError" .= toolOutcomeIsError outcome
                , "cellId" .= cellIdOf outcome
                , "args" .= snip 200 input
                , "result" .= snip 300 (toolOutcomeValue outcome)
                ]
            -- The in-browser chat bypasses 'aiToolH', so record its own
            -- provenance here (actor=InBrowserChat, browser-session sentinel).
            recordToolCall
                (envWorkDir (appEnv app))
                Nothing
                InBrowserChat
                nm
                input
                outcome
                kBefore
                gen
            -- Broadcast the raw (un-compacted) result so the UI shows full
            -- content; only the LLM history gets the compacted form.
            broadcast (appEvents app) $
                EvChatToolResult tid cid (toolOutcomeValue outcome)
            compacted <- compactToolResult store (toolOutcomeValue outcome)
            let compactedOutcome =
                    if toolOutcomeIsError outcome
                        then errOutcome compacted
                        else okOutcome compacted
            appendMessage store $
                Message User [ToolResultPart (ToolResult cid nm compactedOutcome)]

{- | If a tool call is a notebook-acting mutation, fold the cell it wrote into
the owned map, so the accept gate knows which cells this turn is responsible for.
-}
recordOwnedMutation ::
    App -> IORef (Map Int OwnedCell) -> ToolCall -> ToolOutcome -> IO ()
recordOwnedMutation app ownedRef tc outcome =
    case parseToolName (tcName tc) of
        Just tool
            | actsOnNotebook tool
            , Just cid <- cellIdOf outcome -> do
                nb <- readNotebook (appNotebook app)
                let src = maybe "" cellSource (lookupCell cid nb)
                    health =
                        healthOfCellError (if okOf outcome then Nothing else Just "cell error")
                modifyIORef' ownedRef (recordMutation (MutationEvent tool cid src health))
        _ -> pure ()

-- | The @cellId@ a mutation tool's outcome reports, if any.
cellIdOf :: ToolOutcome -> Maybe Int
cellIdOf o = case toolOutcomeValue o of
    Object m -> case KM.lookup "cellId" m of
        Just (Number n) -> Just (round n)
        _ -> Nothing
    _ -> Nothing

-- | Whether a mutation tool's outcome reports a clean execution.
okOf :: ToolOutcome -> Bool
okOf o = case toolOutcomeValue o of
    Object m -> case KM.lookup "execution" m of
        Just (Object e) -> KM.lookup "ok" e == Just (Bool True)
        _ -> KM.lookup "ok" m == Just (Bool True)
    _ -> False
