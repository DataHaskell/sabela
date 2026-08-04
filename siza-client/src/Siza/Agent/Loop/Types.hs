{- | What an episode is driven by and what it produces: the four effects the
loop needs from its host, the budget it runs under, and the record it returns.
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
    , drvDispatch :: ToolCall -> IO (Either Text ToolOutcome)
    , drvNow :: IO Double
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
