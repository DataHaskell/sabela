{-# LANGUAGE TupleSections #-}

{- | Reasoning corpus: category-tagged answer-QUALITY tasks.

A task set that probes the model's REASONING and answer quality rather than
library discovery: competition algorithms with a single deterministic answer,
logic/deduction puzzles with a unique answer, and open-ended design tasks graded
by a behavioural smoke. Every task carries a pure 'ByValue' check pinned to a
LIVE-VALIDATED value, so whether the model hand-rolls or reaches for a package is
irrelevant. Nothing renders and no task names a library.

The gate runs this corpus A/B over the search-tool lever via
@SIZA_GATE_FOLD=reasoning@ (see "Eval.Corpus" and the @siza-gate@ executable).
The bench reaches it with @SIZA_BENCH_CORPUS=reasoning@, narrowed by
@SIZA_BENCH_FOLD@ to a category (@competition@ / @design@ / @logic@); anything
else (including @Nothing@) yields the whole corpus.
-}
module Eval.Corpus.Reasoning (
    Category (..),
    reasoningCorpus,
    reasoningTasks,
    selectReasoning,
) where

import Data.Text (Text)
import qualified Data.Text as T

import Eval.Corpus.Reasoning.Competition (competitionTasks)
import Eval.Corpus.Reasoning.Design (designTasks)
import Eval.Corpus.Reasoning.Logic (logicTasks)
import Eval.Task (Task)

-- | Which of the three task families a reasoning task belongs to.
data Category = Competition | Design | Logic
    deriving (Show, Eq)

-- | The full reasoning corpus, tagged by category.
reasoningCorpus :: [(Category, Task)]
reasoningCorpus =
    tagged Competition competitionTasks
        ++ tagged Design designTasks
        ++ tagged Logic logicTasks
  where
    tagged c = map (c,)

-- | The whole reasoning corpus as a flat task list (what the gate runs).
reasoningTasks :: [Task]
reasoningTasks = [t | (_, t) <- reasoningCorpus]

{- | Select tasks by a category name (@competition@ / @design@ / @logic@);
anything else (including @Nothing@) yields the whole corpus.
-}
selectReasoning :: Maybe Text -> [Task]
selectReasoning sel = case fmap normalise sel of
    Just "competition" -> byCategory Competition
    Just "design" -> byCategory Design
    Just "logic" -> byCategory Logic
    _ -> reasoningTasks
  where
    normalise = T.toLower . T.strip
    byCategory c = [t | (k, t) <- reasoningCorpus, k == c]
