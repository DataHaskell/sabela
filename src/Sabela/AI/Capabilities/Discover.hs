{-# LANGUAGE OverloadedStrings #-}

{- | Tool handler for @find_example_cell@ (query → runnable example cells): a
pure lookup over "Sabela.AI.Examples", formatted as a tool outcome; touches no
kernel, so the dispatcher runs it lock-free. (Package discovery moved to
@search_capability@ over the local Hackage index.)
-}
module Sabela.AI.Capabilities.Discover (
    findExamplesOutcome,
    execFindExampleCell,
) where

import Data.Aeson (Value, object, (.=))

import Sabela.AI.Capabilities.Util (fieldText)
import Sabela.AI.Examples (Example (..), searchExamples)
import Sabela.AI.Types (ToolOutcome, okOutcome)

-- | The @find_examples@ outcome: matching runnable example cells for the query.
findExamplesOutcome :: Value -> ToolOutcome
findExamplesOutcome input =
    okOutcome $
        object ["query" .= q, "examples" .= map exampleJSON (searchExamples q)]
  where
    q = fieldText "query" input
    exampleJSON e =
        object ["title" .= exTitle e, "tags" .= exTags e, "code" .= exCode e]

execFindExampleCell :: Value -> IO ToolOutcome
execFindExampleCell = pure . findExamplesOutcome
