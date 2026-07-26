{-# LANGUAGE TupleSections #-}

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

data Category = Competition | Design | Logic
    deriving (Eq, Show)

reasoningCorpus :: [(Category, Task)]
reasoningCorpus =
    tagged Competition competitionTasks
        ++ tagged Design designTasks
        ++ tagged Logic logicTasks
  where
    tagged c = map (c,)

reasoningTasks :: [Task]
reasoningTasks = [t | (_, t) <- reasoningCorpus]

selectReasoning :: Maybe Text -> [Task]
selectReasoning sel = case fmap normalise sel of
    Just "competition" -> byCategory Competition
    Just "design" -> byCategory Design
    Just "logic" -> byCategory Logic
    _ -> reasoningTasks
  where
    normalise = T.toLower . T.strip
    byCategory c = [t | (k, t) <- reasoningCorpus, k == c]
