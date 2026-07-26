module Sabela.LLM.Completion (
    StopCondition (..),
    Completion (..),
) where

import Sabela.LLM.Message (ContentPart)
import Sabela.LLM.Usage (TokenUsage)

data StopCondition
    = Done
    | WantsTools
    | Truncated
    | Refused
    deriving (Eq, Show)

data Completion = Completion
    { compParts :: [ContentPart]
    , compStop :: StopCondition
    , compUsage :: TokenUsage
    }
    deriving (Eq, Show)
