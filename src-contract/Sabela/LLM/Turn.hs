{- | How a whole agent turn ended, unifying what the notebook loop split across
@TurnPhase@'s terminal constructors and Anthropic's @stop_reason@. The
in-flight phase states stay in "Sabela.AI.Types"; the terminal shape lives here,
neutral.
-}
module Sabela.LLM.Turn (TurnOutcome (..)) where

import Data.Text (Text)

import Sabela.LLM.Completion (StopCondition)

data TurnOutcome
    = Completed StopCondition
    | HitToolLimit Int
    | Cancelled
    | Failed Text
    deriving (Show, Eq)
