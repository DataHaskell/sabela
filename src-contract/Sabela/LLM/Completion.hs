{- | The provider-neutral result of one model round-trip. @StopCondition@
replaces Anthropic's wire @stop_reason@ enum; adapters map their own stop signal
onto it (Ollama, which reports none, derives it from whether tools were called).
-}
module Sabela.LLM.Completion (
    StopCondition (..),
    Completion (..),
) where

import Sabela.LLM.Message (ContentPart)
import Sabela.LLM.Usage (TokenUsage)

-- | Why the model stopped producing this completion.
data StopCondition
    = Done
    | WantsTools
    | Truncated
    | Refused
    deriving (Show, Eq)

data Completion = Completion
    { compParts :: [ContentPart]
    , compStop :: StopCondition
    , compUsage :: TokenUsage
    }
    deriving (Show, Eq)
