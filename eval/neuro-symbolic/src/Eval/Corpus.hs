{- | Phase-0.1 hard corpus: fold-tagged assembly and fold selection.

Re-exports the 'Fold' tag and the fold-tagged task list from
'Eval.Corpus.Tasks', and offers 'selectFold' to pick a fold by name
(@in-index@ / @held-out@ / anything else → all).
-}
module Eval.Corpus (
    Fold (..),
    hardCorpus,
    selectFold,
) where

import Data.Text (Text)
import qualified Data.Text as T

import Eval.Corpus.Reasoning (reasoningTasks)
import Eval.Corpus.Tasks (Fold (..), hardTasks)
import Eval.Task (Task)

-- | The fold-tagged Phase-0.1 corpus.
hardCorpus :: [(Fold, Task)]
hardCorpus = hardTasks

{- | Select tasks by fold name: @Just "in-index"@ → InIndex tasks,
@Just "held-out"@ → HeldOut tasks, @Just "capability"@ → Capability tasks,
@Just "reasoning"@ → the answer-quality reasoning corpus ("Eval.Corpus.Reasoning",
run A/B over the search-tool lever by the gate), anything else (including
@Nothing@) → all of the hard corpus.
-}
selectFold :: Maybe Text -> [Task]
selectFold sel = case fmap normalise sel of
    Just "in-index" -> tasksOf InIndex
    Just "held-out" -> tasksOf HeldOut
    Just "capability" -> tasksOf Capability
    Just "reasoning" -> reasoningTasks
    _ -> map snd hardCorpus
  where
    normalise = T.toLower . T.strip
    tasksOf f = [t | (g, t) <- hardCorpus, g == f]
