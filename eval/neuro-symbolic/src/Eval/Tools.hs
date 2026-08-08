module Eval.Tools (
    module Siza.Agent.Tools,
    episodeCatalogue,
) where

import Data.Aeson (Value)

import Siza.Agent.Recall (resetRecallStore)
import Siza.Agent.Tools

{- | The catalogue a fresh episode is served, with the recall store cleared of
whatever the last episode elided. A bench process runs every (task, seed, arm)
back to back, so without this an index minted in one run answers in the next.
-}
episodeCatalogue :: IO [Value]
episodeCatalogue = do
    resetRecallStore
    catalogue
