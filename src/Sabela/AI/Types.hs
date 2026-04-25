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

    -- * Usage helpers
    emptyUsage,
) where

import Control.Concurrent.STM (TVar, newTVarIO)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Sabela.Anthropic.Types (
    CancelToken,
    StopReason,
    Usage (..),
    newCancelToken,
 )
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
    , turnUsage :: IORef Usage
    -- ^ Accumulated token usage across every LLM iteration of this turn.
    , turnIterations :: IORef Int
    -- ^ Count of LLM round-trips (= assistant responses received) for this turn.
    , turnScratchpadFails :: IORef Int
    {- ^ Consecutive scratchpad calls that produced non-empty stderr. Reset
    to 0 on any scratchpad success. Used for the circuit breaker that
    nudges the model to change approach after repeated failures.
    -}
    }

newTurn :: IORef Int -> IO Turn
newTurn nextIdRef = do
    tid <- atomicModifyIORef' nextIdRef (\n -> (n + 1, n))
    phase <- newTVarIO TurnStreaming
    ct <- newCancelToken
    tc <- newIORef 0
    now <- getCurrentTime
    usageRef <- newIORef emptyUsage
    iterRef <- newIORef 0
    scratchFailRef <- newIORef 0
    pure
        Turn
            { turnId = TurnId tid
            , turnPhase = phase
            , turnCancel = ct
            , turnToolCount = tc
            , turnStartedAt = now
            , turnUsage = usageRef
            , turnIterations = iterRef
            , turnScratchpadFails = scratchFailRef
            }

emptyUsage :: Usage
emptyUsage = Usage 0 0 Nothing Nothing

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
