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
    downstreamDependents,
    looksLikeToolCallText,
) where

import Control.Concurrent.STM (atomically, writeTVar)
import Control.Monad (forM, forM_, unless, when)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AK
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
import qualified Data.Text.Encoding as TE

import Sabela.AI.Capabilities (executeTool)
import Sabela.AI.Capabilities.Edit.Run (executeCell)
import Sabela.AI.Capabilities.Kernel (kernelStateBefore)
import Sabela.AI.Capabilities.ToolName (actsOnNotebook, parseToolName)
import Sabela.AI.Capabilities.Tools (chatToolSpecs)
import Sabela.AI.Capabilities.Util (featureEnabled)
import Sabela.AI.Discover (discoverGrammar, importedModules)
import Sabela.AI.Health (healthOfCellError, healthOfResult)
import Sabela.AI.Orchestrator.Compact (compactToolResult)
import Sabela.AI.Orchestrator.Prompt (buildNotebookDocText, systemPrompt)
import Sabela.AI.Orchestrator.Trace (snip, traceEvent, traceReset)
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
    Cell,
    NotebookEvent (..),
    cellError,
    cellId,
    cellSource,
    lookupCell,
    nbCells,
 )
import Sabela.Reactivity (haskellCodeCells)
import Sabela.SessionTypes (sbQueryBrowse)
import Sabela.State (
    App (..),
    defaultToolLimit,
    getHaskellSession,
    readNotebook,
 )
import Sabela.State.Environment (Environment (..))
import Sabela.State.EventBus (broadcast)
import Sabela.Topo (TopoResult (..), selectAffectedTopoFrom)

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

{- | The live grammar for this turn: browse the notebook's imports and synthesise
their signatures. On by default; disabled by @SABELA_LIVE_GRAMMAR=0@ or no session.
-}
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
                Left err -> do
                    traceEvent app ["kind" .= ("error" :: Text), "msg" .= err]
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
                    traceEvent
                        app
                        [ "kind" .= ("model" :: Text)
                        , "stop" .= T.pack (show (compStop comp))
                        , "toolCount" .= toolCount
                        , "tools" .= [tcName tc | ToolCallPart tc <- parts]
                        , "text" .= T.take 500 (T.concat [t | TextPart t <- parts])
                        ]
                    case compStop comp of
                        WantsTools -> do
                            limit <- readIORef (appAIToolLimit app)
                            if toolCount >= limit
                                then do
                                    traceEvent app ["kind" .= ("toollimit" :: Text), "limit" .= limit]
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
                        Done -> handleDone ownedRef liveGrammar salvaged reenters parts
                        stop ->
                            atomically $
                                writeTVar (turnPhase turn) (TurnComplete (toStopReason stop))

    -- At the model's natural stop: first the narrate-then-stop salvage, then the
    -- closure-verified accept — re-verify owned cells live and, when re-enter is
    -- on and budget remains, nudge on the red ones instead of accepting them.
    handleDone ownedRef liveGrammar salvaged reenters parts = do
        owned <- readIORef ownedRef
        let content = T.concat [t | TextPart t <- parts]
            -- Salvage keys off whether a MUTATING tool wrote a cell this turn
            -- (@owned@), not total tool calls: query calls must not block it.
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
                | otherwise -> do
                    reenterOn <- featureEnabled "SABELA_SELF_HEAL_REENTER"
                    live <- liveOwned app owned
                    -- Fold in the reactive closure: dependents re-run for fresh
                    -- health, so a repair that broke a downstream cell is caught.
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

{- | Re-verify each owned cell's health live from the notebook, dropping any cell
the user has since edited or deleted — the accept must not judge a cell the turn
no longer owns.
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

{- | Re-run the owned cells' reactive dependents for fresh health: the AI path
runs only the edited cell, so a dependent's stored error is stale. Catches a
repair that greened its cell but broke a downstream one.
-}
verifyDownstream ::
    App -> ReactiveNotebook -> Turn -> Map Int OwnedCell -> IO (Map Int OwnedCell)
verifyDownstream app rn turn owned
    | Map.null owned = pure Map.empty
    | otherwise = do
        nb <- readNotebook (appNotebook app)
        let dependents = downstreamDependents owned (haskellCodeCells nb)
        fmap Map.fromList $ forM dependents $ \cid -> do
            res <- executeCell app rn cid (turnCancel turn)
            pure (cid, OwnedCell (healthOfResult res) "")

{- | The non-owned cells in the reactive closure of the owned set, topo-ordered:
the dependents whose health must be re-checked after a repair. Owned cells are
excluded, since the repair loop already tracks their health directly.
-}
downstreamDependents :: Map Int a -> [Cell] -> [Int]
downstreamDependents owned cells =
    [ cellId c
    | c <- trOrdered (fst (selectAffectedTopoFrom (Map.keysSet owned) cells))
    , not (Map.member (cellId c) owned)
    ]

-- | Bounded re-enters when owned cells are left red (progress-independent).
maxReenter :: Int
maxReenter = 2

-- | The hidden nudge naming the still-red owned cells on a re-enter.
reenterNudge :: [Int] -> Text
reenterNudge reds =
    "These cells you wrote still have errors: "
        <> T.intercalate ", " (map (T.pack . show) reds)
        <> ". Fix them before finishing — read the error and repair or rewrite the cell."

{- | The model sometimes writes a tool call as plain text — a bare JSON object
like @{\"query\":\"map\"}@ — instead of invoking it. Detect that so the loop can
nudge rather than mistake it for a finished turn.
-}
looksLikeToolCallText :: Text -> Bool
looksLikeToolCallText t = case Aeson.decodeStrict (TE.encodeUtf8 (T.strip t)) of
    Just (Object o) -> any (\k -> KM.member (AK.fromText k) o) toolArgKeys
    _ -> False
  where
    toolArgKeys =
        [ "query"
        , "module"
        , "name"
        , "source"
        , "code"
        , "cell_id"
        , "new_source"
        , "expr"
        , "goal"
        ]

-- | The nudge when the model emitted a tool call as text instead of invoking it.
toolCallNudge :: Text
toolCallNudge =
    "That looked like a tool call written as text. Invoke it through the tool\
    \ interface instead of writing the JSON in a message."

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
