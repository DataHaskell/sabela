{- | Re-export shim: 'Exemplars' moved to @Siza.Agent.Exemplars@ in siza-client so the product
chat and this eval harness share one implementation. This keeps the @Eval.Exemplars@
name its existing importers already use.
-}
module Eval.Exemplars (module Siza.Agent.Exemplars) where

import Siza.Agent.Exemplars
