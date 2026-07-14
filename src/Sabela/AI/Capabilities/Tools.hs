{- | Catalogue of tools the AI agent can call. The schemas are split
across "Sabela.AI.Capabilities.Tools.Notebook" (read/mutate/execute)
and "Sabela.AI.Capabilities.Tools.Query" (introspect/drill).

'chatTools' is the legacy Anthropic-'ToolDef' view (still served over the REST
bridge / MCP); 'chatToolSpecs' is the provider-neutral 'ToolSpec' view the
'ModelProvider' port consumes. The umbrella bumps the last 'chatTools' entry's
@cache_control@ to @ephemeral-hour@ so the whole schema block joins Anthropic's
cached prefix; 'chatToolSpecs' carries no cache marker (the Anthropic adapter
applies its own policy).
-}
module Sabela.AI.Capabilities.Tools (chatTools, chatToolSpecs) where

import Data.Maybe (mapMaybe)

import Sabela.AI.Capabilities.ToolName (parseToolName)
import Sabela.AI.Capabilities.Tools.Notebook (notebookTools)
import Sabela.AI.Capabilities.Tools.Query (queryTools)
import Sabela.Anthropic.Types (CacheControl (..), ToolDef (..))
import Sabela.LLM.Tool (ToolSpec (..))

chatTools :: [ToolDef]
chatTools = withLastCached (notebookTools ++ queryTools)

withLastCached :: [ToolDef] -> [ToolDef]
withLastCached xs = case reverse xs of
    [] -> []
    (t : rest) -> reverse (t{tdCacheControl = Just EphemeralHour} : rest)

chatToolSpecs :: [ToolSpec]
chatToolSpecs = mapMaybe toSpec (notebookTools ++ queryTools)
  where
    toSpec td =
        (\n -> ToolSpec n (tdDescription td) (tdInputSchema td))
            <$> parseToolName (tdName td)
