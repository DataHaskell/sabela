{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Types (
    TurnId (..),
    EditId (..),
    ToolCallId (..),
    TurnPhase (..),
    Turn (..),
    newTurn,
    AiEdit (..),
    EditStatus (..),
    ScratchpadSession (..),
    ExecutionResult (..),
    ToolOutcome (..),
    toolOutcomeValue,
    toolOutcomeIsError,
    okOutcome,
    errOutcome,
    emptyUsage,
) where

import Control.Concurrent.STM (TVar, newTVarIO)
import Data.Aeson (ToJSON (..), Value)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Sabela.Anthropic.Types (
    CancelToken,
    StopReason,
    Usage (..),
    newCancelToken,
 )
import Sabela.Ids (EditId (..), ToolCallId (..), TurnId (..))
import Sabela.Model (CellError, OutputItem)
import Sabela.SessionTypes (CellLang, SessionBackend)

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
    , turnIterations :: IORef Int
    , turnScratchpadFails :: IORef Int
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

data EditStatus = Pending | Accepted | Reverted | Superseded
    deriving (Eq, Show)

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
    , aeTurnId :: Maybe TurnId
    }

data ScratchpadSession = ScratchpadSession
    { spBackend :: SessionBackend
    , spWorkDir :: FilePath
    , spLang :: CellLang
    , spDeps :: [Text]
    }

data ExecutionResult = ExecutionResult
    { erOutputs :: [OutputItem]
    , erError :: Maybe Text
    , erErrors :: [CellError]
    , erWarnings :: [CellError]
    }

data ToolOutcome
    = ToolOk !Value
    | ToolErr !Value
    deriving (Eq, Show)

toolOutcomeValue :: ToolOutcome -> Value
toolOutcomeValue (ToolOk v) = v
toolOutcomeValue (ToolErr v) = v

toolOutcomeIsError :: ToolOutcome -> Bool
toolOutcomeIsError ToolOk{} = False
toolOutcomeIsError ToolErr{} = True

okOutcome :: Value -> ToolOutcome
okOutcome = ToolOk

errOutcome :: Value -> ToolOutcome
errOutcome = ToolErr
