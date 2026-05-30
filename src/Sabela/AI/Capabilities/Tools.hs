{- | Catalogue of tools the AI agent can call. The schemas are split
across "Sabela.AI.Capabilities.Tools.Notebook" (read/mutate/execute)
and "Sabela.AI.Capabilities.Tools.Query" (introspect/drill); this
umbrella concatenates them and bumps the last entry's
@cache_control@ to @ephemeral-hour@ so the whole schema block joins
Anthropic's cached prefix.
-}
module Sabela.AI.Capabilities.Tools (chatTools) where

import Sabela.AI.Capabilities.Tools.Notebook (notebookTools)
import Sabela.AI.Capabilities.Tools.Query (queryTools)
import Sabela.Anthropic.Types (CacheControl (..), ToolDef (..))

chatTools :: [ToolDef]
chatTools = withLastCached (notebookTools ++ queryTools)

withLastCached :: [ToolDef] -> [ToolDef]
withLastCached xs = case reverse xs of
    [] -> []
    (t : rest) -> reverse (t{tdCacheControl = Just EphemeralHour} : rest)
