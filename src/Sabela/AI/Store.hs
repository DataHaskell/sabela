{-# LANGUAGE BangPatterns #-}

module Sabela.AI.Store (
    AIStore (..),
    newAIStore,
    getAIConfig,
    setAIModel,
    setAIFullConfig,
    getAIProvider,
    setAIProvider,
    trimHistory,

    -- * Conversation
    getMessages,
    appendMessage,
    clearConversation,

    -- * Turn
    getCurrentTurn,
    setCurrentTurn,
    clearCurrentTurn,

    -- * Pending edits
    getPendingEdits,
    addPendingEdit,
    lookupEdit,
    updateEditStatus,
    revertAllPendingEdits,

    -- * Scratchpad
    getScratchpad,
    setScratchpad,
    clearScratchpad,

    -- * Kernel admission gate
    admitKernel,
) where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Control.Concurrent.STM (
    TVar,
    atomically,
    modifyTVar',
    newTVarIO,
    readTVar,
    readTVarIO,
    writeTVar,
 )
import Data.IORef (
    IORef,
    atomicModifyIORef',
    atomicWriteIORef,
    newIORef,
    readIORef,
 )
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Network.HTTP.Client (Manager)
import Sabela.AI.Handles (HandleStore, newHandleStore)
import Sabela.AI.Types
import Sabela.Anthropic.Types (AnthropicConfig (..), Usage (..))
import Sabela.LLM.Anthropic (anthropicProvider)
import Sabela.LLM.Message (ContentPart (..), Message (..), Role (..))
import Sabela.LLM.Provider (ModelProvider)
import Sabela.Session (Admission, admit)
import Sabela.SessionTypes (SessionBackend (..))

data AIStore = AIStore
    { aiMessages :: MVar [Message]
    , aiCurrentTurn :: TVar (Maybe Turn)
    , aiPendingEdits :: TVar (M.Map EditId AiEdit)
    , aiPendingByCell :: TVar (M.Map Int EditId)
    {- ^ Secondary index: cellId → 'EditId' of the pending edit for that
    cell. Lets 'addPendingEdit' find the prior pending edit (if any)
    in O(log n) instead of scanning the whole 'aiPendingEdits' map.
    -}
    , aiScratchpad :: MVar (Maybe ScratchpadSession)
    , aiNextEditId :: IORef Int
    , aiNextTurnId :: IORef Int
    , aiConfig :: IORef AnthropicConfig
    , aiProvider :: IORef ModelProvider
    {- ^ The active LLM backend (built from config, swapped when the provider or
    model changes). The agentic loop drives this through the neutral port
    instead of a hardcoded Anthropic client.
    -}
    , aiHttpManager :: Manager
    , aiUsage :: IORef Usage
    , aiHandles :: HandleStore
    , aiAdmission :: MVar ()
    {- ^ The AI kernel-admission gate. A single 'admit' (one 'tryTakeMVar')
    folds the busy decision and the hold into one step, so two AI tool
    callers can't both pass a busy check and stack behind the run-lock
    (the §1.4 TOCTOU). Held for the whole kernel-needing tool run.
    -}
    , aiAdmissionHolder :: IORef (Maybe Int)
    -- ^ Candidate id of the caller currently holding 'aiAdmission'.
    }

newAIStore :: AnthropicConfig -> Manager -> IO AIStore
newAIStore cfg mgr =
    AIStore
        <$> newMVar []
        <*> newTVarIO Nothing
        <*> newTVarIO M.empty
        <*> newTVarIO M.empty
        <*> newMVar Nothing
        <*> newIORef 0
        <*> newIORef 0
        <*> newIORef cfg
        <*> newIORef (anthropicProvider mgr cfg)
        <*> pure mgr
        <*> newIORef (Usage 0 0 Nothing Nothing)
        <*> newHandleStore
        <*> newMVar ()
        <*> newIORef Nothing

-- | Read the current Anthropic config.
getAIConfig :: AIStore -> IO AnthropicConfig
getAIConfig = readIORef . aiConfig

{- | Update only the Claude model. The API key is preserved so mid-conversation
switches don't require re-entering the key. The caller ('updateAIConfig') selects
the matching provider afterwards.
-}
setAIModel :: AIStore -> Text -> IO ()
setAIModel store model =
    atomicModifyIORef' (aiConfig store) (\c -> (c{acModel = model}, ()))

-- | Overwrite the full config (e.g. when the API key changes).
setAIFullConfig :: AIStore -> AnthropicConfig -> IO ()
setAIFullConfig store = atomicWriteIORef (aiConfig store)

-- | The active LLM backend the agentic loop drives.
getAIProvider :: AIStore -> IO ModelProvider
getAIProvider = readIORef . aiProvider

-- | Swap the active LLM backend (on a provider/model change, or a test fake).
setAIProvider :: AIStore -> ModelProvider -> IO ()
setAIProvider store = atomicWriteIORef (aiProvider store)

------------------------------------------------------------------------
-- Conversation
------------------------------------------------------------------------

getMessages :: AIStore -> IO [Message]
getMessages = readMVar . aiMessages

{- | How many recent conversation *turns* to retain (a turn = a user-text
message and everything after it). Counting turns, not raw messages, keeps prior
turns intact through tool-heavy agentic rounds — so cross-turn references ("the
image from before") still resolve. Trimming stays safe: never splits a
tool_use/tool_result pair or leaves an orphan tool_result at the head.
-}
historyWindow :: Int
historyWindow = 20

{- | Snoc @msg@ onto the conversation, trimmed to 'historyWindow'.
The bang forces the trimmed list so successive appends across an
agent turn don't pile thunks in the 'MVar'.
-}
appendMessage :: AIStore -> Message -> IO ()
appendMessage store msg = modifyMVar_ (aiMessages store) $ \msgs -> do
    let !trimmed = trimHistory historyWindow (msgs ++ [msg])
    pure trimmed

{- | Keep the last @n@ conversation turns. A turn boundary is a user message
whose content is pure text (no @tool_result@ blocks); cutting there keeps the
head a valid user message (required by Anthropic), never splits a tool_use /
tool_result pair, and never leaves an orphan tool_result at the head. Counting
turns (not raw messages) means a tool-heavy turn can't evict earlier turns, so
cross-turn references survive. Falls back to the whole list if there is no
user-text anchor at all.
-}
trimHistory :: Int -> [Message] -> [Message]
trimHistory n msgs =
    let anchors = [i | (i, m) <- zip [0 ..] msgs, isUserText m]
        cut = case drop (max 0 (length anchors - n)) anchors of
            (i : _) -> i
            [] -> 0
     in drop cut msgs

isUserText :: Message -> Bool
isUserText m = msgRole m == User && all isTextPart (msgParts m)
  where
    isTextPart (TextPart _) = True
    isTextPart _ = False

clearConversation :: AIStore -> IO ()
clearConversation store = modifyMVar_ (aiMessages store) (const (pure []))

------------------------------------------------------------------------
-- Turn
------------------------------------------------------------------------

getCurrentTurn :: AIStore -> IO (Maybe Turn)
getCurrentTurn = readTVarIO . aiCurrentTurn

setCurrentTurn :: AIStore -> Turn -> IO ()
setCurrentTurn store t = atomically $ writeTVar (aiCurrentTurn store) (Just t)

clearCurrentTurn :: AIStore -> IO ()
clearCurrentTurn store = atomically $ writeTVar (aiCurrentTurn store) Nothing

------------------------------------------------------------------------
-- Pending edits
------------------------------------------------------------------------

getPendingEdits :: AIStore -> IO (M.Map EditId AiEdit)
getPendingEdits = readTVarIO . aiPendingEdits

{- | Register a new pending edit, superseding any prior pending edit for
the same cell via the 'aiPendingByCell' secondary index.
-}
addPendingEdit :: AIStore -> AiEdit -> IO ()
addPendingEdit store edit = atomically $ do
    byCell <- readTVar (aiPendingByCell store)
    case M.lookup (aeCellId edit) byCell of
        Just priorEid -> do
            edits <- readTVar (aiPendingEdits store)
            case M.lookup priorEid edits of
                Just prior -> do
                    status <- readTVar (aeStatus prior)
                    case status of
                        Pending -> writeTVar (aeStatus prior) Superseded
                        _ -> pure ()
                Nothing -> pure ()
        Nothing -> pure ()
    modifyTVar' (aiPendingEdits store) (M.insert (aeEditId edit) edit)
    modifyTVar' (aiPendingByCell store) (M.insert (aeCellId edit) (aeEditId edit))

lookupEdit :: AIStore -> EditId -> IO (Maybe AiEdit)
lookupEdit store eid = do
    edits <- readTVarIO (aiPendingEdits store)
    pure (M.lookup eid edits)

{- | Update the pending edit's status. On any terminal state
('Accepted', 'Reverted', 'Superseded') the entry is also removed from
the pending-edits map so the old/new source text isn't pinned for the
rest of the conversation.
-}
updateEditStatus :: AIStore -> EditId -> EditStatus -> IO ()
updateEditStatus store eid status = do
    mEdit <- lookupEdit store eid
    case mEdit of
        Just edit -> atomically $ do
            writeTVar (aeStatus edit) status
            case status of
                Pending -> pure ()
                _ -> do
                    modifyTVar' (aiPendingEdits store) (M.delete eid)
                    -- Only drop from the secondary index if it still
                    -- points at this edit — a later edit for the same
                    -- cell may already have overwritten the slot.
                    modifyTVar' (aiPendingByCell store) $ \byCell ->
                        case M.lookup (aeCellId edit) byCell of
                            Just curr | curr == eid -> M.delete (aeCellId edit) byCell
                            _ -> byCell
        Nothing -> pure ()

revertAllPendingEdits :: AIStore -> IO ()
revertAllPendingEdits store = atomically $ do
    edits <- readTVar (aiPendingEdits store)
    mapM_ revertIfPending (M.elems edits)
    writeTVar (aiPendingEdits store) M.empty
    writeTVar (aiPendingByCell store) M.empty
  where
    revertIfPending edit = do
        status <- readTVar (aeStatus edit)
        case status of
            Pending -> writeTVar (aeStatus edit) Reverted
            _ -> pure ()

------------------------------------------------------------------------
-- Kernel admission gate
------------------------------------------------------------------------

{- | Atomically admit a kernel-needing tool through the AI gate: a SINGLE
'tryTakeMVar' decides busy and acquires in one step. The gate is held for the
whole @act@ and released on completion or exception. @candidate@ is recorded
as the holder, so a bounced caller's 'Busy' reports who holds the slot.
-}
admitKernel :: AIStore -> Int -> IO a -> IO (Admission a)
admitKernel store = admit (aiAdmission store) (aiAdmissionHolder store)

------------------------------------------------------------------------
-- Scratchpad
------------------------------------------------------------------------

getScratchpad :: AIStore -> IO (Maybe ScratchpadSession)
getScratchpad = readMVar . aiScratchpad

setScratchpad :: AIStore -> Maybe ScratchpadSession -> IO ()
setScratchpad store val = modifyMVar_ (aiScratchpad store) (const (pure val))

clearScratchpad :: AIStore -> IO ()
clearScratchpad store = modifyMVar_ (aiScratchpad store) $ \mSp -> do
    case mSp of
        Just sp -> sbClose (spBackend sp)
        Nothing -> pure ()
    pure Nothing
