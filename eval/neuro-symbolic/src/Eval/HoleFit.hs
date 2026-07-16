{- | Re-export shim. The typed-hole repair helpers moved to
@Sabela.AI.HoleRepair@ in @sabela-contract@ so notebook and eval share one
implementation; this keeps the @Eval.HoleFit@ name importers already use.
-}
module Eval.HoleFit (module Sabela.AI.HoleRepair) where

import Sabela.AI.HoleRepair
