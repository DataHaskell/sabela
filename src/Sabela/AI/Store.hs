

module Sabela.AI.Store (
    AIStore (..),
    newAIStore,

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
    newTVarIO,
    readTVar,
    readTVarIO,
    writeTVar,
 )
import Data.IORef (IORef, newIORef)
import qualified Data.Map.Strict as M
import Network.HTTP.Client (Manager)
import Sabela.AI.Types
import Sabela.Anthropic.Types (AnthropicConfig, Message, Usage (..))
import Sabela.SessionTypes (SessionBackend (..))

data AIStore = AIStore
    { aiMessages :: MVar [Message]
    , aiCurrentTurn :: TVar (Maybe Turn)
    , aiPendingEdits :: TVar (M.Map EditId AiEdit)
    , aiScratchpad :: MVar (Maybe ScratchpadSession)
    , aiNextEditId :: IORef Int
    , aiNextTurnId :: IORef Int
    , aiConfig :: AnthropicConfig
    , aiHttpManager :: Manager
    , aiUsage :: IORef Usage
    }

newAIStore :: AnthropicConfig -> Manager -> IO AIStore
newAIStore cfg mgr =
    AIStore
        <$> newMVar []
        <*> newTVarIO Nothing
        <*> newTVarIO M.empty
        <*> newMVar Nothing
        <*> newIORef 0
        <*> newIORef 0
        <*> pure cfg
        <*> pure mgr
        <*> newIORef (Usage 0 0 Nothing Nothing)

------------------------------------------------------------------------
-- Conversation
------------------------------------------------------------------------

getMessages :: AIStore -> IO [Message]
getMessages = readMVar . aiMessages

appendMessage :: AIStore -> Message -> IO ()
appendMessage store msg = modifyMVar_ (aiMessages store) $ \msgs ->
    let msgs' = msgs ++ [msg]
     in -- Keep at most 40 messages (20 pairs)
        pure (drop (max 0 (length msgs' - 40)) msgs')

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

addPendingEdit :: AIStore -> AiEdit -> IO ()
addPendingEdit store edit = atomically $ do
    edits <- readTVar (aiPendingEdits store)
    -- Supersede any existing pending edit for the same cell
    edits' <- mapMSupersede (aeCellId edit) edits
    writeTVar (aiPendingEdits store) (M.insert (aeEditId edit) edit edits')
  where
    mapMSupersede cid edits = do
        let go (_, e)
                | aeCellId e == cid = do
                    status <- readTVar (aeStatus e)
                    case status of
                        Pending -> writeTVar (aeStatus e) Superseded
                        _ -> pure ()
                | otherwise = pure ()
        mapM_ go (M.toList edits)
        pure edits

lookupEdit :: AIStore -> EditId -> IO (Maybe AiEdit)
lookupEdit store eid = do
    edits <- readTVarIO (aiPendingEdits store)
    pure (M.lookup eid edits)

updateEditStatus :: AIStore -> EditId -> EditStatus -> IO ()
updateEditStatus store eid status = do
    mEdit <- lookupEdit store eid
    case mEdit of
        Just edit -> atomically $ writeTVar (aeStatus edit) status
        Nothing -> pure ()

revertAllPendingEdits :: AIStore -> IO ()
revertAllPendingEdits store = atomically $ do
    edits <- readTVar (aiPendingEdits store)
    mapM_ revertIfPending (M.elems edits)
    writeTVar (aiPendingEdits store) M.empty
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
