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
    getMessages,
    appendMessage,
    clearConversation,
    getCurrentTurn,
    setCurrentTurn,
    clearCurrentTurn,
    getPendingEdits,
    addPendingEdit,
    lookupEdit,
    updateEditStatus,
    revertAllPendingEdits,
    getScratchpad,
    setScratchpad,
    clearScratchpad,
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
import Data.Word (Word64)
import Network.HTTP.Client (Manager)
import Sabela.AI.Handles (HandleStore, newHandleStore)
import Sabela.AI.Types
import Sabela.AI.WriteRegistry (WriteRegistry, newWriteRegistry)
import Sabela.Anthropic.Types (AnthropicConfig (..), Usage (..))
import Sabela.LLM.Anthropic (anthropicProvider)
import Sabela.LLM.Message (ContentPart (..), Message (..), Role (..))
import Sabela.LLM.Provider (ModelProvider)
import Sabela.Session.Admission (Admission, admit)
import Sabela.SessionTypes (SessionBackend (..))

data AIStore = AIStore
    { aiMessages :: MVar [Message]
    , aiCurrentTurn :: TVar (Maybe Turn)
    , aiPendingEdits :: TVar (M.Map EditId AiEdit)
    , aiPendingByCell :: TVar (M.Map Int EditId)
    , aiWriteReg :: WriteRegistry
    , aiScratchpad :: MVar (Maybe ScratchpadSession)
    , aiNextEditId :: IORef Int
    , aiNextTurnId :: IORef Int
    , aiConfig :: IORef AnthropicConfig
    , aiProvider :: IORef ModelProvider
    , aiHttpManager :: Manager
    , aiUsage :: IORef Usage
    , aiHandles :: HandleStore
    , aiAdmission :: MVar ()
    , aiAdmissionHolder :: IORef (Maybe (Int, Word64))
    , aiSettledGen :: IORef (Maybe Int)
    , aiBusySince :: IORef (Maybe Word64)
    }

newAIStore :: AnthropicConfig -> Manager -> IO AIStore
newAIStore cfg mgr =
    AIStore
        <$> newMVar []
        <*> newTVarIO Nothing
        <*> newTVarIO M.empty
        <*> newTVarIO M.empty
        <*> newWriteRegistry
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
        <*> newIORef Nothing
        <*> newIORef Nothing

getAIConfig :: AIStore -> IO AnthropicConfig
getAIConfig = readIORef . aiConfig

setAIModel :: AIStore -> Text -> IO ()
setAIModel store model =
    atomicModifyIORef' (aiConfig store) (\c -> (c{acModel = model}, ()))

setAIFullConfig :: AIStore -> AnthropicConfig -> IO ()
setAIFullConfig store = atomicWriteIORef (aiConfig store)

getAIProvider :: AIStore -> IO ModelProvider
getAIProvider = readIORef . aiProvider

setAIProvider :: AIStore -> ModelProvider -> IO ()
setAIProvider store = atomicWriteIORef (aiProvider store)

getMessages :: AIStore -> IO [Message]
getMessages = readMVar . aiMessages

historyWindow :: Int
historyWindow = 20

appendMessage :: AIStore -> Message -> IO ()
appendMessage store msg = modifyMVar_ (aiMessages store) $ \msgs -> do
    let !trimmed = trimHistory historyWindow (msgs ++ [msg])
    pure trimmed

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

getCurrentTurn :: AIStore -> IO (Maybe Turn)
getCurrentTurn = readTVarIO . aiCurrentTurn

setCurrentTurn :: AIStore -> Turn -> IO ()
setCurrentTurn store t = atomically $ writeTVar (aiCurrentTurn store) (Just t)

clearCurrentTurn :: AIStore -> IO ()
clearCurrentTurn store = atomically $ writeTVar (aiCurrentTurn store) Nothing

getPendingEdits :: AIStore -> IO (M.Map EditId AiEdit)
getPendingEdits = readTVarIO . aiPendingEdits

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

admitKernel :: AIStore -> Int -> IO a -> IO (Admission a)
admitKernel store = admit (aiAdmission store) (aiAdmissionHolder store)

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
