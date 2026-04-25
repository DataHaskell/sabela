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
import qualified Data.Aeson as Aeson
import Data.IORef (atomicModifyIORef', readIORef)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Time (diffUTCTime, getCurrentTime)

import Sabela.AI.Capabilities (chatTools, executeTool)
import Sabela.AI.Doc (defaultDocOpts, renderNotebookDoc)
import Sabela.AI.Handles (clearHandles, storeLargeResult, summarizeForLLM)

-- NOTE: apiReferenceCard is concatenated into 'systemPrompt' (single site, at
-- the "## Sabela display + widgets" slot). Do not add it as a separate
-- SystemBlock — that would double-ship the same text per request and waste a
-- cache breakpoint. Anthropic caps cache_control markers at 4 per request.
import Sabela.AI.ReferenceCard (apiReferenceCard)
import Sabela.AI.Store
import Sabela.AI.Types
import Sabela.Anthropic
import Sabela.Handlers (ReactiveNotebook)
import Sabela.Model (
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
            clearHandles (aiHandles store)
            -- Emit per-turn usage telemetry before the terminal event so the
            -- frontend can update the badge regardless of terminal reason.
            emitTurnUsage app turn
            -- Broadcast terminal event based on phase
            phase <- readTVarIO (turnPhase turn)
            case phase of
                TurnCancelled ->
                    broadcast (appEvents app) (EvChatCancelled tid)
                TurnFailed msg ->
                    broadcast (appEvents app) (EvChatError tid msg)
                _ ->
                    broadcast (appEvents app) (EvChatDone tid)

-- | Assemble per-turn usage stats and broadcast EvChatUsageUpdate.
emitTurnUsage :: App -> Turn -> IO ()
emitTurnUsage app turn = do
    let TurnId tid = turnId turn
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
                        , -- 4096 unconditionally. max_tokens is a safety cap,
                          -- not a billing knob — Anthropic bills per actually
                          -- emitted token. Capping low risks silent truncation
                          -- of tool_use payloads (e.g. propose_edit new_source
                          -- on a long cell), which breaks the JSON and wastes
                          -- an iteration on retry. See earlier P0.6 regret.
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
                                        EvChatError tid "Tool use limit reached (25)"
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
            -- Broadcast raw (un-compacted) result to the UI so users see full
            -- content; only the LLM history gets the compacted form.
            broadcast (appEvents app) $
                EvChatToolResult tid tcId result
            compacted <- compactToolResult store result
            let resultBlock =
                    ToolResultBlock
                        tcId
                        isErr
                        [TextBlock (resultToText compacted)]
            appendMessage store (Message RoleUser [resultBlock])

-- | Extract tool_use blocks from content.
extractToolUses :: [ContentBlock] -> [(Text, Text, Value)]
extractToolUses = mapMaybe extract
  where
    extract (ToolUseBlock tid name input) = Just (tid, name, input)
    extract _ = Nothing

{- | Threshold (in characters of the JSON-encoded form) above which a tool
result is stashed in the handle store instead of being inlined into the
conversation. Tools that know their output is large should stash proactively
(see Capabilities.compactOutputs); this is the safety net for structured
payloads that weren't explicitly pre-compacted.
-}
compactToolResultThreshold :: Int
compactToolResultThreshold = 8000

{- | Safety compaction pass for tool results before they land in conversation
history. If a result is small, pass it through unchanged. If it exceeds
'compactToolResultThreshold' characters once JSON-encoded, stash it in the
handle store and return a compact summary object referencing the handle, so
the LLM can drill in via @explore_result@ instead of losing the tail.

This replaces the old silent-clip behaviour that dropped bytes past 8000.
-}
compactToolResult :: AIStore -> Value -> IO Value
compactToolResult store v = do
    let text = resultToText v
    if T.length text <= compactToolResultThreshold
        then pure v
        else do
            r <- storeLargeResult (aiHandles store) text
            case r of
                -- The handle store's own cleanup (ANSI strip + dedupe) shrank
                -- the payload below its own inline threshold. Use the cleaned
                -- text inline.
                Left cleaned -> pure (String cleaned)
                Right (hid, summary, nLines, nBytes) ->
                    pure $
                        object
                            [ "_compacted" .= True
                            , "_note"
                                .= ( "Tool result exceeded inline limit; stashed. Drill in via explore_result." ::
                                        Text
                                   )
                            , "_large" .= summarizeForLLM hid summary nLines nBytes
                            ]

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
        [ "You are a Haskell data analyst in Sabela. Load tabular data with"
        , "`dataframe`, plot with `granite` or `DataFrame.Display.Web.Plot`."
        , "Drop to Python only when the Haskell ecosystem is missing a piece."
        , ""
        , "## Principles"
        , ""
        , "- **Compiler is your superpower.** Before inserting or proposing any"
        , "  change, compile-check it with ghci_query or scratchpad."
        , "- **Small compileable units.** One definition at a time; shrink on"
        , "  failure instead of guessing."
        , "- **Report back often.** One short sentence to the user after each"
        , "  tool call. No silent tool flurries."
        , "- **Stop when the user's ask is satisfied.** Don't add gratuitous"
        , "  extras (\"let me also add a summary stats cell\") unless asked."
        , "  A clean minimal result beats an over-engineered one that hits"
        , "  the TPM ceiling mid-turn. If the core ask is done after 3 cells,"
        , "  stop and hand back to the user with a one-line summary."
        , ""
        , "The second system block is the notebook JSON index (id, hash, firstLine)."
        , "Read it instead of list_cells. Pass expected_hash on propose_edit."
        , ""
        , apiReferenceCard
        , ""
        , "## Cell syntax (scripths — same rules in cells AND scratchpad)"
        , ""
        , "- NO top-level `let`. Write `x = 10`. `let` works inside do/where"
        , "  and in `let..in..` expressions."
        , "- Multi-line defs work directly — scripths wraps them in `:{ :}` for"
        , "  you. DO NOT write `:{` / `:}` yourself; nested blocks break GHCi."
        , "- `$(F.declareColumns df)` and other top-level TH splices are fine."
        , "- Merge across `-- cabal: build-depends:`, `default-extensions:`,"
        , "  `ghc-options:` directives."
        , "- GHCi works: `:set -XFoo`, `:script path`, `:! shell-cmd`, bare"
        , "  expressions print (GHCi style)."
        , "- Bootstrap typical dataframe imports + extensions in one shot:"
        , "  `:! curl -s -o rc.hs https://raw.githubusercontent.com/mchav/ihaskell-dataframe/main/rc.hs`"
        , "  then `:script rc.hs`"
        , ""
        , "If scratchpad rejects something, the stderr will include a"
        , "`[sabela hint]` explaining what's wrong. Read it before retrying."
        , "If you hit 3+ consecutive scratchpad errors, STOP, explain the"
        , "blocker to the user, and ask them rather than churning."
        , ""
        , "## Core idioms (anchors only — call api_reference for full signatures)"
        , ""
        , "**Prefer `DataFrame.Typed` over the untyped `DataFrame` whenever"
        , "the schema is known** — it gives compile-time column-name checks,"
        , "better error messages, and cheaper iteration via GHCi's type"
        , "machinery. Drop to untyped `D.*` only for schema-polymorphic code"
        , "(write a generic transform over any DataFrame) or initial CSV"
        , "loading before the schema is known."
        , ""
        , "```haskell"
        , "-- cabal: build-depends: dataframe, text, granite"
        , "-- cabal: default-extensions: DataKinds, TemplateHaskell, TypeApplications, OverloadedStrings"
        , "import qualified DataFrame as D"
        , "import qualified DataFrame.Functions as F"
        , "import qualified DataFrame.Typed as DT"
        , "import DataFrame ((|>))"
        , "import qualified Data.Text as T"
        , ""
        , "-- Load untyped, then freeze to a typed schema named \"Housing\":"
        , "df <- D.readCsv \"./path.csv\""
        , "$(DT.deriveSchema \"Housing\" df)"
        , "tdf = either (error . show) id (DT.freezeWithError @Housing df)"
        , ""
        , "-- Typed ops: compile-checked column names via `@\"...\"`."
        , "tdf |> DT.take 5"
        , "    |> DT.derive @\"rooms_per_household\" (DT.col @\"total_rooms\" / DT.col @\"households\")"
        , "    |> DT.filter (DT.col @\"rooms_per_household\") (>= 5)"
        , "    |> DT.thaw"
        , "    |> D.toMarkdown' |> displayMarkdown"
        , ""
        , "-- Plots (work on untyped D.DataFrame; DT.thaw to get there):"
        , "import qualified DataFrame.Display.Web.Plot as Plt"
        , "Plt.HtmlPlot p <- Plt.plotAllHistograms (DT.thaw tdf)"
        , "displayHtml (T.unpack p)"
        , ""
        , "-- Granite SVG charts for static plots:"
        , "import Granite.Svg"
        , "displaySvg $ T.unpack $ bars [(\"Q1\",12),(\"Q2\",18)] defPlot { plotTitle = \"Sales\" }"
        , "```"
        , ""
        , "For anything beyond these: `api_reference {module:\"DataFrame.Typed\"}`"
        , "(or \"DataFrame\", \"Functions\", \"Plot\", \"Granite\"), or"
        , "`ghci_query {op:\"browse\", arg:\"<Module>\"}`."
        , ""
        , "## Workflow"
        , ""
        , "1. Read the notebook doc. Probe types with ghci_query if uncertain."
        , "2. Dry-run in scratchpad. If it compiles, proceed."
        , "3. insert_cell one at a time. Check `execution.ok` in the response."
        , "   If false, fix in the same turn before moving on."
        , "4. Narrate each step in a short line. Summarize at the end."
        , ""
        , "## Tools"
        , ""
        , "- ghci_query {op, arg}: `:type|:info|:kind|:browse|:doc` against the"
        , "  live session. Cheapest feedback loop. Use `browse <Module>` when"
        , "  you need to discover what's available."
        , "- scratchpad: isolated session, same packages. Dry-run snippets here"
        , "  before inserting or proposing."
        , "- insert_cell: applies + auto-runs Haskell code. Response includes"
        , "  `execution.ok` — act on it, don't claim success without ok == true."
        , "- replace_cell_source: directly edit a cell you inserted. Applies +"
        , "  auto-runs. USE THIS to iterate — never delete-and-reinsert, never"
        , "  propose_edit your own scaffolding."
        , "- propose_edit: NOT applied until the user accepts. Reserve for"
        , "  cells the USER wrote. Compile-verify via scratchpad first."
        , "- execute_cell: re-run an existing cell."
        , "- explore_result: drill into large outputs returned by handleId."
        , ""
        , "Output helpers: displayMarkdown, displayHtml, displaySvg, displayLatex."
        , "Be concise."
        ]

-- | Build the notebook JSON document rendered to text for the system block.
buildNotebookDocText :: App -> IO Text
buildNotebookDocText app = do
    nb <- readNotebook (appNotebook app)
    let doc = renderNotebookDoc defaultDocOpts nb
    pure $
        "Current notebook (JSON):\n"
            <> TL.toStrict (TLE.decodeUtf8 (Aeson.encode doc))
