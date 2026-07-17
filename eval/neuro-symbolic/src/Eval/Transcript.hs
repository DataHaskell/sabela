{- | Re-export shim: 'Transcript' moved to @Siza.Agent.Transcript@ in siza-client so the product
chat and this eval harness share one implementation. This keeps the @Eval.Transcript@
name its existing importers already use.
-}
module Eval.Transcript (module Siza.Agent.Transcript) where

import Siza.Agent.Transcript
