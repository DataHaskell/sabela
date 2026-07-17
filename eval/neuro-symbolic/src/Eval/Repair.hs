{- | Re-export shim: 'Repair' moved to @Siza.Agent.Repair@ in siza-client so the product
chat and this eval harness share one implementation. This keeps the @Eval.Repair@
name its existing importers already use.
-}
module Eval.Repair (module Siza.Agent.Repair) where

import Siza.Agent.Repair
