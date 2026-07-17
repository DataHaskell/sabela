{- | Re-export shim: 'Preflight' moved to @Siza.Agent.Preflight@ in siza-client so the product
chat and this eval harness share one implementation. This keeps the @Eval.Preflight@
name its existing importers already use.
-}
module Eval.Preflight (module Siza.Agent.Preflight) where

import Siza.Agent.Preflight
