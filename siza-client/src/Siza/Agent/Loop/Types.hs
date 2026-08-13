{- |
Technique: the episode Handle [Episode].
Guarantee: the 'Driver' record is the loop's whole effect seam; every technique intercepts one wire.
Entry: 'Driver'. Implementations: chat REPL, MCP server, eval harness. Next: Siza.Agent.Loop.
-}
module Siza.Agent.Loop.Types (
    AgentRun (..),
    Driver (..),
    EpisodeBudget (..),
    defaultBudget,
) where

import Data.Aeson (Value)
import Data.Map.Strict (Map)
import Data.Text (Text)

import Sabela.AI.CellResult (CellId)
import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..), Turn (..))
import Siza.Agent.Check (CheckResult (..))
import Siza.Agent.Owned (OwnedCell (..))

data AgentRun = AgentRun
    { arTurns :: Int
    , arToolCalls :: Int
    , arFinal :: Text
    , arStopped :: Text
    , arTranscript :: [Value]
    }
    deriving (Show)

data Driver = Driver
    { drvChat :: [Value] -> IO (Either Text Turn)
    -- ^ one model turn; the chat REPL, MCP server and eval harness each wire this
    , drvDispatch :: ToolCall -> IO (Either Text ToolOutcome)
    -- ^ run one tool call; the dispatch stack sits behind this wire on all three hosts
    , drvNow :: IO Double
    -- ^ the wall clock the deadline budget reads; each host supplies its own
    , drvVerify :: Map CellId OwnedCell -> IO (CheckResult, Maybe Text)
    -- ^ verification is keyed to what this episode owns, not to the notebook
    }

data EpisodeBudget = EpisodeBudget
    { ebMaxRepairs :: Int
    , ebDeadlineSecs :: Double
    }
    deriving (Show)

defaultBudget :: EpisodeBudget
defaultBudget = EpisodeBudget{ebMaxRepairs = 4, ebDeadlineSecs = 600}
