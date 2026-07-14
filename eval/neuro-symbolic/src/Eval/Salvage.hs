{- | Re-export shim. The salvage helpers moved to @Sabela.AI.Salvage@ in
@sabela-contract@ so the in-notebook orchestrator and this eval harness recover
a fenced-block cell the same way. This module keeps the @Eval.Salvage@ name its
importers already use.
-}
module Eval.Salvage (module Sabela.AI.Salvage) where

import Sabela.AI.Salvage
