{- | What the server says about itself before any work happens: the annotated
tool catalogue, the pairing prompt, and the notebook as a resource.

A tool a host must prompt for is a tool the model learns to avoid, so the
read-only and destructive hints are load-bearing, not decoration.
-}
module Siza.Mcp.Surface (
    mcpCatalogue,
    toolAnnotations,
    promptsList,
    promptGet,
    resourcesList,
    notebookUri,
    readOnlyTools,
    destructiveTools,
) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import qualified Siza.Agent.Loop as AgentLoop
import Siza.Agent.Stack (Surface (..))
import qualified Siza.Agent.Tools as AgentTools

notebookUri :: Text
notebookUri = "siza://notebook"

{- | The catalogue as MCP wants it: the OpenAI envelope unwrapped, plus the
annotations a host acts on.
-}
mcpCatalogue :: [Value]
mcpCatalogue = map toMcp (AgentTools.catalogueFor McpSurface)

toMcp :: Value -> Value
toMcp (Object o)
    | Just (Object f) <- KM.lookup "function" o =
        let name = case KM.lookup "name" f of
                Just (String n) -> n
                _ -> ""
         in object
                [ "name" .= name
                , "description" .= fromMaybe Null (KM.lookup "description" f)
                , "inputSchema"
                    .= fromMaybe (object []) (KM.lookup "parameters" f)
                , "annotations" .= toolAnnotations name
                ]
toMcp v = v

toolAnnotations :: Text -> Value
toolAnnotations name =
    object
        [ "title" .= titleFor name
        , "readOnlyHint" .= (name `elem` readOnlyTools)
        , "destructiveHint" .= (name `elem` destructiveTools)
        , "openWorldHint" .= (name `elem` openWorldTools)
        ]

-- | Tools that only look. A host may run these without asking.
readOnlyTools :: [Text]
readOnlyTools =
    [ "list_cells"
    , "read_cell"
    , "list_bindings"
    , "check_type"
    , "discover"
    , "list_files"
    , "read_file"
    , "try"
    , "kernel_status"
    , "await_idle"
    ]

{- | Tools that destroy work already done. Everything else that writes only
adds to it.
-}
destructiveTools :: [Text]
destructiveTools = ["delete_cell", "kernel_restart", "interrupt"]

-- | Tools whose answers come from outside the notebook.
openWorldTools :: [Text]
openWorldTools = ["discover", "read_file", "list_files"]

titleFor :: Text -> Text
titleFor n = fromMaybe (defaultTitle n) (lookup n titleTable)

defaultTitle :: Text -> Text
defaultTitle = id

titleTable :: [(Text, Text)]
titleTable =
    [ ("list_cells", "Map the whole notebook")
    , ("read_cell", "Read one cell in full")
    , ("insert_cell", "Add a cell and run it")
    , ("replace_cell_source", "Rewrite a cell and re-run it")
    , ("execute_cell", "Run one cell")
    , ("delete_cell", "Delete a cell")
    , ("list_bindings", "List what the session has in scope")
    , ("check_type", "Look a name up, or ask the compiler for a type")
    , ("discover", "Search Haskell APIs and this notebook")
    , ("list_files", "List files beside the notebook")
    , ("read_file", "Read a file beside the notebook")
    , ("try", "Run code without touching the notebook")
    , ("verify", "Check a claim against the live kernel")
    , ("kernel_status", "Ask whether the kernel is busy")
    , ("await_idle", "Wait for the running cell to finish")
    , ("interrupt", "Abort the running cell")
    , ("kernel_restart", "Restart the kernel")
    ]

promptsList :: Value
promptsList =
    object
        [ "prompts"
            .= [ object
                    [ "name" .= ("pair" :: Text)
                    , "description"
                        .= ( "Start pairing on the notebook: what it is, and \
                             \which tool answers which question." ::
                                Text
                           )
                    , "arguments" .= ([] :: [Value])
                    ]
               ]
        ]

promptGet :: Text -> Maybe Value
promptGet "pair" =
    Just
        ( object
            [ "description" .= ("Pair on a live Sabela notebook." :: Text)
            , "messages"
                .= [ object
                        [ "role" .= ("user" :: Text)
                        , "content"
                            .= object
                                [ "type" .= ("text" :: Text)
                                , "text" .= AgentLoop.mcpInstructions
                                ]
                        ]
                   ]
            ]
        )
promptGet _ = Nothing

resourcesList :: Value
resourcesList =
    object
        [ "resources"
            .= [ object
                    [ "uri" .= notebookUri
                    , "name" .= ("notebook" :: Text)
                    , "description"
                        .= ( "Every cell in the notebook being paired on: id, \
                             \source, what it defines, and whether it errored." ::
                                Text
                           )
                    , "mimeType" .= ("application/json" :: Text)
                    ]
               ]
        ]
