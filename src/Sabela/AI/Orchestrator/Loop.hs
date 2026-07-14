{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | The agentic loop: send one 'CompletionRequest' to the notebook's
'ModelProvider', accumulate the 'Completion', dispatch any tool calls, and
iterate until the model stops asking for tools (or we hit the tool budget). The
provider — Anthropic or a local model — is chosen at the composition root and
lives in the 'AIStore'; this loop is provider-neutral and never names a wire
format.

The entry points in 'Sabela.AI.Orchestrator' are the only callers.
-}
module Sabela.AI.Orchestrator.Loop (
    -- * Loop
    agenticLoop,
    maxToolIterations,

    -- * Helpers (exposed for test mirrors)
    mergeUsage,
) where

import Control.Concurrent.STM (atomically, writeTVar)
import Control.Monad (forM_, unless, when)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (
    IORef,
    atomicModifyIORef',
    modifyIORef',
    newIORef,
    readIORef,
 )
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities (executeTool)
import Sabela.AI.Capabilities.Kernel (kernelStateBefore)
import Sabela.AI.Capabilities.ToolName (actsOnNotebook, parseToolName)
import Sabela.AI.Capabilities.Tools (chatToolSpecs)
import Sabela.AI.Discover (discoverGrammar, importedModules)
import Sabela.AI.Health (healthOfCellError)
import Sabela.AI.Orchestrator.Compact (compactToolResult)
import Sabela.AI.Orchestrator.Prompt (buildNotebookDocText, systemPrompt)
import Sabela.AI.Owned (
    MutationEvent (..),
    OwnedCell (..),
    StopDecision (..),
    recordMutation,
    stopDecision,
 )
import Sabela.AI.Provenance (Actor (..), recordToolCall)
import Sabela.AI.Salvage (salvageInsertSource)
import Sabela.AI.Store
import Sabela.AI.Types
import Sabela.Anthropic.Types (StopReason (..), Usage (..))
import Sabela.Handlers (ReactiveNotebook)
import Sabela.Ids (ToolCallId (..))
import Sabela.LLM.Cancel (isCancelled)
import Sabela.LLM.Completion (Completion (..), StopCondition (..))
import Sabela.LLM.Message (
    ContentPart (..),
    Message (..),
    Role (..),
    ToolCall (..),
    ToolResult (..),
 )
import Sabela.LLM.Provider (
    ChunkSink (..),
    CompletionRequest (..),
    ModelProvider (..),
    ProviderCaps (..),
 )
import qualified Sabela.LLM.Usage as K
import Sabela.Model (
    NotebookEvent (..),
    cellError,
    cellSource,
    lookupCell,
    nbCells,
 )
import Sabela.SessionTypes (sbQueryBrowse)
import Sabela.State (
    App (..),
    defaultToolLimit,
    getHaskellSession,
    readNotebook,
 )
import Sabela.State.Environment (Environment (..))
import Sabela.State.EventBus (broadcast)
import System.Environment (lookupEnv)

{- | Default per-turn cap on tool-call rounds. The live cap is read from
'appAIToolLimit' (configurable in the AI settings modal); this is its default.
-}
maxToolIterations :: Int
maxToolIterations = defaultToolLimit

{- | The synthetic @insert_cell@ a salvaged fenced block is routed through, so it
gets the same dispatch, audit, and progress events as a real tool call.
-}
salvageInsertPart :: Text -> ContentPart
salvageInsertPart src =
    ToolCallPart
        (ToolCall (ToolCallId "salvage") "insert_cell" (object ["source" .= src]))

{- | The live grammar for this turn: browse the notebook's imported modules and
synthesise their real signatures. 'Nothing' unless @SABELA_LIVE_GRAMMAR@ is set
and a session is up.
-}
discoverLiveGrammar :: App -> IO (Maybe Text)
discoverLiveGrammar app = do
    enabled <- lookupEnv "SABELA_LIVE_GRAMMAR"
    mBackend <- getHaskellSession (appSessions app)
    case (enabled, mBackend) of
        (Just _, Just backend) -> do
            nb <- readNotebook (appNotebook app)
            let mods = nub (concatMap (importedModules . cellSource) (nbCells nb))
            discoverGrammar (sbQueryBrowse backend) mods
        _ -> pure Nothing

agenticLoop :: App -> AIStore -> ReactiveNotebook -> Turn -> IO ()
agenticLoop app store rn turn = do
    ownedRef <- newIORef Map.empty
    liveGrammar <- discoverLiveGrammar app
    go ownedRef liveGrammar False 0
  where
    tid = turnId turn

    -- Threads the per-turn live grammar, the once-per-turn salvage flag, and the
    -- bounded re-enter count; owned cells accumulate in @ownedRef@.
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
                Left err ->
                    atomically $ writeTVar (turnPhase turn) (TurnFailed err)
                Right comp -> do
                    let parts = compParts comp
                    -- A non-streaming provider (Ollama) never fed the ChunkSink,
                    -- so the UI has no assistant text yet — surface it as one
                    -- delta now, or the reply is invisible (only chatDone shows).
                    unless (capStreaming (mpCaps provider)) $
                        let txt = T.concat [t | TextPart t <- parts]
                         in unless (T.null txt) $
                                broadcast (appEvents app) (EvChatTextDelta tid txt)
                    appendMessage store (Message Assistant parts)
                    atomicModifyIORef' (turnIterations turn) (\n -> (n + 1, ()))
                    accumulateUsage store turn (compUsage comp)
                    toolCount <- readIORef (turnToolCount turn)
                    case compStop comp of
                        WantsTools -> do
                            limit <- readIORef (appAIToolLimit app)
                            if toolCount >= limit
                                then do
                                    broadcast (appEvents app) $
                                        EvChatError
                                            (Just tid)
                                            ( "Tool use limit reached ("
                                                <> T.pack (show limit)
                                                <> ")"
                                            )
                                    atomically $
                                        writeTVar (turnPhase turn) (TurnFailed "Tool limit")
                                else do
                                    executeToolCalls app store rn turn ownedRef parts
                                    go ownedRef liveGrammar salvaged reenters
                        Done -> handleDone ownedRef liveGrammar salvaged reenters parts toolCount
                        stop ->
                            atomically $
                                writeTVar (turnPhase turn) (TurnComplete (toStopReason stop))

    -- At the model's natural stop: first the narrate-then-stop salvage, then the
    -- closure-verified accept — re-verify owned cells live and, when re-enter is
    -- on and budget remains, nudge on the red ones instead of accepting them.
    handleDone ownedRef liveGrammar salvaged reenters parts toolCount
        | not salvaged
        , Just src <-
            salvageInsertSource toolCount (T.concat [t | TextPart t <- parts]) = do
            executeToolCalls app store rn turn ownedRef [salvageInsertPart src]
            go ownedRef liveGrammar True reenters
        | otherwise = do
            reenterOn <- lookupEnv "SABELA_SELF_HEAL_REENTER"
            owned <- readIORef ownedRef
            live <- liveOwned app owned
            case stopDecision live of
                Reenter reds
                    | isJust reenterOn
                    , reenters < maxReenter -> do
                        appendMessage store (Message User [TextPart (reenterNudge reds)])
                        go ownedRef liveGrammar salvaged (reenters + 1)
                _ -> atomically $ writeTVar (turnPhase turn) (TurnComplete SREndTurn)

{- | Map the neutral stop condition onto 'TurnPhase''s (still Anthropic-shaped)
'StopReason'. The value is not surfaced — the turn just completes — and this
last domain leak goes away with the Phase-5 aggregate reshape.
-}
toStopReason :: StopCondition -> StopReason
toStopReason Truncated = SRMaxTokens
toStopReason _ = SREndTurn

-- | Fold a turn's token usage onto both the turn and the session accumulators.
accumulateUsage :: AIStore -> Turn -> K.TokenUsage -> IO ()
accumulateUsage store turn tu = do
    let u = toUsage tu
    atomicModifyIORef' (aiUsage store) (\old -> (mergeUsage old u, ()))
    atomicModifyIORef' (turnUsage turn) (\old -> (mergeUsage old u, ()))

toUsage :: K.TokenUsage -> Usage
toUsage tu =
    Usage
        { uInputTokens = K.tuInput tu
        , uOutputTokens = K.tuOutput tu
        , uCacheCreationInputTokens = K.tuCacheWrite tu
        , uCacheReadInputTokens = K.tuCacheRead tu
        }

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

{- | If a tool call is a notebook-acting mutation, fold the cell it wrote (id +
committed source + provisional health) into the owned map, so the accept gate
knows which cells this turn is responsible for.
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

{- | Re-verify each owned cell's health LIVE from the notebook, dropping any cell
the user has edited since (its source no longer matches what the agent committed)
or deleted — the accept must not judge a cell the turn no longer owns.
-}
liveOwned :: App -> Map Int OwnedCell -> IO (Map Int OwnedCell)
liveOwned app owned = do
    nb <- readNotebook (appNotebook app)
    pure (Map.mapMaybeWithKey (reverify nb) owned)
  where
    reverify nb cid oc = case lookupCell cid nb of
        Just cell
            | cellSource cell == ocSource oc ->
                Just oc{ocHealth = healthOfCellError (cellError cell)}
        _ -> Nothing

-- | Bounded re-enters when owned cells are left red (progress-independent).
maxReenter :: Int
maxReenter = 2

-- | The hidden nudge naming the still-red owned cells on a re-enter.
reenterNudge :: [Int] -> Text
reenterNudge reds =
    "These cells you wrote still have errors: "
        <> T.intercalate ", " (map (T.pack . show) reds)
        <> ". Fix them before finishing — read the error and repair or rewrite the cell."

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
