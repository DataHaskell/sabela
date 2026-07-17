{- | Re-export shim: 'Discover' moved to @Siza.Agent.Discover@ in siza-client so the product
chat and this eval harness share one implementation. This keeps the @Eval.Discover@
name its existing importers already use.
-}
module Eval.Discover (module Siza.Agent.Discover) where

import Siza.Agent.Discover
