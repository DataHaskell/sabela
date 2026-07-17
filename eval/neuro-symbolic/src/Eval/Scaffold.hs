{- | Re-export shim: 'Scaffold' moved to @Siza.Agent.Scaffold@ in siza-client so
the product chat and this eval harness share one implementation. Note the reshaped
'scaffoldCall' now takes the request 'Text' directly (not a benchmark @Task@).
-}
module Eval.Scaffold (module Siza.Agent.Scaffold) where

import Siza.Agent.Scaffold
