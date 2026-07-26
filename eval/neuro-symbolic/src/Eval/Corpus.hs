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

hardCorpus :: [(Fold, Task)]
hardCorpus = hardTasks

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
