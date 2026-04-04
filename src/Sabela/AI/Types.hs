
{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Types (
    -- * IDs
    TurnId (..),
    EditId (..),
    ToolCallId (..),

    -- * Turn state
    TurnPhase (..),
    Turn (..),
    newTurn,

    -- * Pending edits
    AiEdit (..),
    EditStatus (..),

    -- * Scratchpad
    ScratchpadSession (..),

    -- * Execution result
    ExecutionResult (..),
) where

import Control.Concurrent.STM (TVar, newTVarIO)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Sabela.Anthropic.Types (CancelToken, StopReason, newCancelToken)
import Sabela.Model (CellError, OutputItem)
import Sabela.SessionTypes (CellLang, SessionBackend)

newtype TurnId = TurnId Int
    deriving (Show, Eq, Ord)

instance ToJSON TurnId where
    toJSON (TurnId n) = toJSON n

newtype EditId = EditId Int
    deriving (Show, Eq, Ord)

instance ToJSON EditId where
    toJSON (EditId n) = toJSON n

newtype ToolCallId = ToolCallId Text
    deriving (Show, Eq)

instance ToJSON ToolCallId where
    toJSON (ToolCallId t) = toJSON t

------------------------------------------------------------------------
-- Turn state machine
------------------------------------------------------------------------

data TurnPhase
    = TurnStreaming
    | TurnExecutingTools Int
    | TurnAwaitingLLM
    | TurnComplete StopReason
    | TurnCancelled
    | TurnFailed Text
    deriving (Show)

data Turn = Turn
    { turnId :: TurnId
    , turnPhase :: TVar TurnPhase
    , turnCancel :: CancelToken
    , turnToolCount :: IORef Int
    , turnStartedAt :: UTCTime
    }

newTurn :: IORef Int -> IO Turn
newTurn nextIdRef = do
    tid <- atomicModifyIORef' nextIdRef (\n -> (n + 1, n))
    phase <- newTVarIO TurnStreaming
    ct <- newCancelToken
    tc <- newIORef 0
    now <- getCurrentTime
    pure
        Turn
            { turnId = TurnId tid
            , turnPhase = phase
            , turnCancel = ct
            , turnToolCount = tc
            , turnStartedAt = now
            }

------------------------------------------------------------------------
-- Pending edits
------------------------------------------------------------------------

data EditStatus = Pending | Accepted | Reverted | Superseded
    deriving (Show, Eq)

instance ToJSON EditStatus where
    toJSON Pending = "pending"
    toJSON Accepted = "accepted"
    toJSON Reverted = "reverted"
    toJSON Superseded = "superseded"

data AiEdit = AiEdit
    { aeEditId :: EditId
    , aeCellId :: Int
    , aeOldSource :: Text
    , aeNewSource :: Text
    , aeStatus :: TVar EditStatus
    , aeTurnId :: TurnId
    }

------------------------------------------------------------------------
-- Scratchpad
------------------------------------------------------------------------

data ScratchpadSession = ScratchpadSession
    { spBackend :: SessionBackend
    , spWorkDir :: FilePath
    , spLang :: CellLang
    }

------------------------------------------------------------------------
-- Execution result
------------------------------------------------------------------------

data ExecutionResult = ExecutionResult
    { erOutputs :: [OutputItem]
    , erError :: Maybe Text
    , erErrors :: [CellError]
    }

instance ToJSON ExecutionResult where
    toJSON er =
        object
            [ "outputs" .= erOutputs er
            , "error" .= erError er
            , "errors" .= erErrors er
            ]
