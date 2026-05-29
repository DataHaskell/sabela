{-# LANGUAGE BangPatterns #-}

module Sabela.AI.Store (
    AIStore (..),
    newAIStore,
    getAIConfig,
    setAIModel,
    setAIFullConfig,
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
import Sabela.Anthropic.Types (
    AnthropicConfig (..),
    ContentBlock (..),
    Message (..),
    Role (..),
    Usage (..),
 )
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
    , aiHttpManager :: Manager
    , aiUsage :: IORef Usage
    , aiHandles :: HandleStore
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
        <*> pure mgr
        <*> newIORef (Usage 0 0 Nothing Nothing)
        <*> newHandleStore

-- | Read the current Anthropic config.
getAIConfig :: AIStore -> IO AnthropicConfig
getAIConfig = readIORef . aiConfig

{- | Update only the Claude model. The API key is preserved so mid-conversation
switches don't require re-entering the key.
-}
setAIModel :: AIStore -> Text -> IO ()
setAIModel store model =
    atomicModifyIORef' (aiConfig store) (\cfg -> (cfg{acModel = model}, ()))

-- | Update the full config (e.g. when the API key changes).
setAIFullConfig :: AIStore -> AnthropicConfig -> IO ()
setAIFullConfig store = atomicWriteIORef (aiConfig store)

------------------------------------------------------------------------
-- Conversation
------------------------------------------------------------------------

getMessages :: AIStore -> IO [Message]
getMessages = readMVar . aiMessages

{- | Soft cap on retained history. Trimming is safe — never splits a
tool_use/tool_result pair or leaves an orphan tool_result at the head.
-}
historyWindow :: Int
historyWindow = 10

{- | Snoc @msg@ onto the conversation, trimmed to 'historyWindow'.
The bang forces the trimmed list so successive appends across an
agent turn don't pile thunks in the 'MVar'.
-}
appendMessage :: AIStore -> Message -> IO ()
appendMessage store msg = modifyMVar_ (aiMessages store) $ \msgs -> do
    let !trimmed = trimHistory historyWindow (msgs ++ [msg])
    pure trimmed

{- | Trim to roughly @n@ messages but always cut at a turn boundary — a user
message whose content is pure text (no @tool_result@ blocks). That anchor is
required by Anthropic (first message must be user) and keeps tool_use /
tool_result pairs intact. Falls back to the latest safe anchor when the window
can't contain one, even if that means exceeding @n@: correctness beats budget.
-}
trimHistory :: Int -> [Message] -> [Message]
trimHistory n msgs =
    let len = length msgs
        minCut = max 0 (len - n)
        safeCuts = [i | (i, m) <- zip [0 ..] msgs, isUserText m]
        cut = case dropWhile (< minCut) safeCuts of
            (i : _) -> i
            [] -> case safeCuts of
                [] -> 0
                _ -> last safeCuts
     in drop cut msgs

isUserText :: Message -> Bool
isUserText m = msgRole m == RoleUser && all isTextBlock (msgContent m)
  where
    isTextBlock (TextBlock _) = True
    isTextBlock _ = False

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
