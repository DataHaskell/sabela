module Sabela.AI.Capabilities.Tools (chatTools, chatToolSpecs) where

import Data.Maybe (mapMaybe)

import Sabela.AI.Capabilities.ToolName (parseToolName)
import Sabela.AI.Capabilities.Tools.Notebook (notebookTools)
import Sabela.AI.Capabilities.Tools.Query (queryTools)
import Sabela.AI.Capabilities.Tools.World (worldTools)
import Sabela.Anthropic.Types (CacheControl (..), ToolDef (..))
import Sabela.LLM.Tool (ToolSpec (..))

chatTools :: [ToolDef]
chatTools = withLastCached (notebookTools ++ queryTools ++ worldTools)

withLastCached :: [ToolDef] -> [ToolDef]
withLastCached xs = case reverse xs of
    [] -> []
    (t : rest) -> reverse (t{tdCacheControl = Just EphemeralHour} : rest)

chatToolSpecs :: [ToolSpec]
chatToolSpecs = mapMaybe toSpec (notebookTools ++ queryTools ++ worldTools)
  where
    toSpec td =
        (\n -> ToolSpec n (tdDescription td) (tdInputSchema td))
            <$> parseToolName (tdName td)
