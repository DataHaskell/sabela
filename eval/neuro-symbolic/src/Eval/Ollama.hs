{- | Re-export shim. The Ollama client + wire-recovery layer moved to
@Sabela.LLM.Ollama.Client@ in @sabela-contract@ so the in-notebook chat and this
eval harness share ONE implementation. This module keeps the @Eval.Ollama@ name
its many importers already use.
-}
module Eval.Ollama (module Sabela.LLM.Ollama.Client) where

import Sabela.LLM.Ollama.Client
