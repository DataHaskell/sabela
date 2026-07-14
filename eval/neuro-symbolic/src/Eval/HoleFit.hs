{- | Re-export shim. The typed-hole repair helpers moved to
@Sabela.AI.HoleRepair@ in @sabela-contract@ so the in-notebook repair path and
this eval harness share one implementation. This module keeps the @Eval.HoleFit@
name its importers already use.
-}
module Eval.HoleFit (module Sabela.AI.HoleRepair) where

import Sabela.AI.HoleRepair
