{- | Re-export shim. The streaming Messages API client moved to
@Sabela.LLM.Anthropic.Client@ in @sabela-contract@ (so it sits beside the
neutral port and the Ollama adapter, and any consumer can reach it). This keeps
the @Sabela.Anthropic@ name — and its re-export of @Sabela.Anthropic.Types@ —
that the rest of the main library already imports.
-}
module Sabela.Anthropic (module Sabela.LLM.Anthropic.Client) where

import Sabela.LLM.Anthropic.Client
