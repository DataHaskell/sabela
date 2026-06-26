{-# LANGUAGE OverloadedStrings #-}

{- | Tool handlers for the two discovery searches: @find_package@ (intent →
package) and @find_example_cell@ (query → runnable example cells). Both are pure
lookups over "Sabela.Discover" and "Sabela.AI.Examples", formatted as tool
outcomes; they touch no kernel, so the dispatcher runs them lock-free.
-}
module Sabela.AI.Capabilities.Discover (
    findLibraryOutcome,
    findExamplesOutcome,
    execFindPackage,
    execFindExampleCell,
) where

import Data.Aeson (Value, object, (.=))

import Sabela.AI.Capabilities.Util (fieldText)
import Sabela.AI.Examples (Example (..), searchExamples)
import Sabela.AI.Types (ToolOutcome, okOutcome)
import Sabela.Discover (LibSuggestion (..), cabalLine, discoverLibraries)

-- | The @find_library@ outcome: ranked package suggestions for the query.
findLibraryOutcome :: Value -> ToolOutcome
findLibraryOutcome input =
    okOutcome $
        object ["query" .= q, "suggestions" .= map suggestionJSON (discoverLibraries q)]
  where
    q = fieldText "query" input
    suggestionJSON s =
        object
            [ "packages" .= lsPackages s
            , "cabal" .= cabalLine s
            , "for" .= lsFor s
            , "modules" .= lsModules s
            ]

-- | The @find_examples@ outcome: matching runnable example cells for the query.
findExamplesOutcome :: Value -> ToolOutcome
findExamplesOutcome input =
    okOutcome $
        object ["query" .= q, "examples" .= map exampleJSON (searchExamples q)]
  where
    q = fieldText "query" input
    exampleJSON e =
        object ["title" .= exTitle e, "tags" .= exTags e, "code" .= exCode e]

execFindPackage :: Value -> IO ToolOutcome
execFindPackage = pure . findLibraryOutcome

execFindExampleCell :: Value -> IO ToolOutcome
execFindExampleCell = pure . findExamplesOutcome
