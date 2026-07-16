{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Types (
    -- * IDs (re-exported from "Sabela.Ids" for compat)
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

    -- * Tool execution outcome
    ToolOutcome (..),
    toolOutcomeValue,
    toolOutcomeIsError,
    okOutcome,
    errOutcome,

    -- * Usage helpers
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
    , aeTurnId :: Maybe TurnId
    {- ^ 'Nothing' when the edit was proposed outside a chat turn (REST
    tool bridge). Previously the placeholder was @TurnId 0@.
    -}
    }

------------------------------------------------------------------------
-- Scratchpad
------------------------------------------------------------------------

data ScratchpadSession = ScratchpadSession
    { spBackend :: SessionBackend
    , spWorkDir :: FilePath
    , spLang :: CellLang
    , spDeps :: [Text]
    -- ^ build-depends the project was scaffolded with; a change rebuilds it.
    }

------------------------------------------------------------------------
-- Execution result
------------------------------------------------------------------------

{- | The raw cell-execution result. Mapped to the typed 'CellResult' by
'Sabela.AI.CellResult.toCellResult'; it carries NO 'ToJSON' instance — the
legacy @{outputs,error,errors}@ blob was the duplicate encoding C-2 removed.
-}
data ExecutionResult = ExecutionResult
    { erOutputs :: [OutputItem]
    , erError :: Maybe Text
    , erErrors :: [CellError]
    , erWarnings :: [CellError]
    }

------------------------------------------------------------------------
-- Tool execution outcome
------------------------------------------------------------------------

{- | Result of executing one AI tool call. Distinct success and failure
constructors replace the @(Value, Bool)@ pair every @exec*@ used to return —
@(value, True)@ was \"error carrying this value\", @(value, False)@ was
\"success carrying this value\", and getting the boolean backwards silently
turned a tool error into a successful tool result on the wire.

Convert to the Anthropic wire shape at the very last step via
'toolOutcomeIsError' (for the @is_error@ field on @ToolResultBlock@) and
'toolOutcomeValue' (for the content).
-}
data ToolOutcome
    = ToolOk !Value
    | ToolErr !Value
    deriving (Show, Eq)

toolOutcomeValue :: ToolOutcome -> Value
toolOutcomeValue (ToolOk v) = v
toolOutcomeValue (ToolErr v) = v

toolOutcomeIsError :: ToolOutcome -> Bool
toolOutcomeIsError ToolOk{} = False
toolOutcomeIsError ToolErr{} = True

-- | Short alias matching @pure (v, False)@.
okOutcome :: Value -> ToolOutcome
okOutcome = ToolOk

-- | Short alias matching @pure (v, True)@.
errOutcome :: Value -> ToolOutcome
errOutcome = ToolErr
