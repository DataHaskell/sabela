{- | Re-export shim: 'Sample' moved to @Siza.Agent.Sample@ in siza-client so the product
chat and this eval harness share one implementation. This keeps the @Eval.Sample@
name its existing importers already use.
-}
module Eval.Sample (module Siza.Agent.Sample) where

import Siza.Agent.Sample
