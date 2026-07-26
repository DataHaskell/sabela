module Sabela.AI.Orchestrator.Loop.Usage (
    accumulateUsage,
    mergeUsage,
    toStopReason,
) where

import Data.IORef (atomicModifyIORef')
import Data.Maybe (fromMaybe)

import Sabela.AI.Store
import Sabela.AI.Types
import Sabela.Anthropic.Types (StopReason (..), Usage (..))
import Sabela.LLM.Completion (StopCondition (..))
import qualified Sabela.LLM.Usage as K

accumulateUsage :: AIStore -> Turn -> K.TokenUsage -> IO ()
accumulateUsage store turn tu = do
    let u = toUsage tu
    atomicModifyIORef' (aiUsage store) (\old -> (mergeUsage old u, ()))
    atomicModifyIORef' (turnUsage turn) (\old -> (mergeUsage old u, ()))

toUsage :: K.TokenUsage -> Usage
toUsage tu =
    Usage
        { uInputTokens = K.tuInput tu
        , uOutputTokens = K.tuOutput tu
        , uCacheCreationInputTokens = K.tuCacheWrite tu
        , uCacheReadInputTokens = K.tuCacheRead tu
        }

mergeUsage :: Usage -> Usage -> Usage
mergeUsage a b =
    Usage
        { uInputTokens = uInputTokens a + uInputTokens b
        , uOutputTokens = uOutputTokens a + uOutputTokens b
        , uCacheCreationInputTokens =
            addMaybeInt (uCacheCreationInputTokens a) (uCacheCreationInputTokens b)
        , uCacheReadInputTokens =
            addMaybeInt (uCacheReadInputTokens a) (uCacheReadInputTokens b)
        }
  where
    addMaybeInt Nothing Nothing = Nothing
    addMaybeInt x y = Just (fromMaybe 0 x + fromMaybe 0 y)

toStopReason :: StopCondition -> StopReason
toStopReason Truncated = SRMaxTokens
toStopReason _ = SREndTurn
