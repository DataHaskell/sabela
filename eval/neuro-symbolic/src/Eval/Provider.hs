{- | Re-export shim. The Ollama @Turn -> Completion@ mapping moved into the
shared adapter @Sabela.LLM.Ollama@; this keeps the @Eval.Provider@ name its
tests use.
-}
module Eval.Provider (turnToCompletion) where

import Sabela.LLM.Ollama (turnToCompletion)
