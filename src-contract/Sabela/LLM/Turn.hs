module Sabela.LLM.Turn (TurnOutcome (..)) where

import Data.Text (Text)

import Sabela.LLM.Completion (StopCondition)

data TurnOutcome
    = Completed StopCondition
    | HitToolLimit Int
    | Cancelled
    | Failed Text
    deriving (Eq, Show)
