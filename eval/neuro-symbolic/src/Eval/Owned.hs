{- | Re-export shim: 'Owned' moved to @Siza.Agent.Owned@ in siza-client so the product
chat and this eval harness share one implementation. This keeps the @Eval.Owned@
name its existing importers already use.
-}
module Eval.Owned (module Siza.Agent.Owned) where

import Siza.Agent.Owned
