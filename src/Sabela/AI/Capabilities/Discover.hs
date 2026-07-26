{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Capabilities.Discover (
    findExamplesOutcome,
    execFindExampleCell,
) where

import Data.Aeson (Value, object, (.=))

import Sabela.AI.Capabilities.Util (fieldText)
import Sabela.AI.Examples (Example (..), searchExamples)
import Sabela.AI.Types (ToolOutcome, okOutcome)

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
