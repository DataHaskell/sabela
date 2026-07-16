module Eval.Tools (
    catalogue,
    dispatch,
    withInsertDefaults,
    renderOutcome,
    unknownToolMsg,
) where

import Data.Aeson (Value (..), encode, object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Sabela.AI.Capabilities.ToolName (ToolName (..), resolveToolCall)
import Sabela.AI.Types (ToolOutcome (..))
import Siza.Transport (Conn, callTool)
import System.Environment (lookupEnv)

import Eval.Ollama (ToolCall (..))

fn :: Text -> Text -> Value -> Value
fn name desc params =
    object
        [ "type" .= ("function" :: Text)
        , "function"
            .= object
                ["name" .= name, "description" .= desc, "parameters" .= params]
        ]

prop :: Text -> Value
prop d = object ["type" .= ("string" :: Text), "description" .= d]

intProp :: Text -> Value
intProp d = object ["type" .= ("integer" :: Text), "description" .= d]

boolProp :: Text -> Value
boolProp d = object ["type" .= ("boolean" :: Text), "description" .= d]

{- | The model-facing tool catalogue. @search_capability@ is gated behind the
@SABELA_CAPABILITY_SEARCH@ env var (the A/B lever): the ON arm sets it and the
model sees the tool; with it unset the tool is omitted entirely so the OFF arm's
model never knows it exists. The rest of the catalogue is constant.
-}
catalogue :: IO [Value]
catalogue = do
    capSearch <- isJust <$> lookupEnv "SABELA_CAPABILITY_SEARCH"
    pure (baseCatalogue ++ [searchCapabilityTool | capSearch])

searchCapabilityTool :: Value
searchCapabilityTool =
    fn
        "search_capability"
        "Search ALL of Hackage by plain-language description, type signature, or keyword to DISCOVER functions/packages you don't know. e.g. \"compute how different two strings are\", or a type \"Text -> Text -> Int\", or a name. Returns ranked candidate packages, each with its cabal build-depends line, the modules to import, its key exported functions with signatures, and a paste-able usage example (import line plus a call skeleton for the top function). Use this to find the right library AND see how to call it before importing it."
        ( props
            [("query", prop "A description, a type signature, or a name.")]
            ["query"]
        )

baseCatalogue :: [Value]
baseCatalogue =
    [ fn
        "list_cells"
        "Map of EVERY cell in the notebook (the whole notebook in one call): each cell's id, position, type, language, the bindings it `defines`, and whether it errored. By default each cell shows only its first line; pass `full: true` to include each cell's source. To find which cell defines or uses a name, scan `defines` here or use find_cells_by_content."
        ( props
            [
                ( "full"
                , boolProp "Include each cell's source (default false: first-line preview)."
                )
            ]
            []
        )
    , fn
        "read_cell"
        "Read ONE cell's full SOURCE and error by id. Its outputs (often large rendered HTML/SVG) are omitted by default — a `hasOutputs` flag signals them; pass `full: true` to include outputs."
        ( props
            [ ("cell_id", intProp "Cell id from list_cells.")
            , ("full", boolProp "Include the cell's outputs (default false).")
            ]
            ["cell_id"]
        )
    , fn
        "find_cells_by_content"
        "Search the NOTEBOOK's own cell sources for a substring (e.g. \"model\", \"fitDecisionTree\"); returns matching cell ids and the matching lines. This is how you locate which cell defines or uses something so you can edit it — unlike find_function/find_by_type, which search installed LIBRARIES, not your notebook."
        ( props
            [
                ( "pattern"
                , prop "A substring to find in cell sources, e.g. \"fitDecisionTree\"."
                )
            ]
            ["pattern"]
        )
    , fn
        "insert_cell"
        "Append a new Haskell cell and run it. Put the cell's full Haskell source in the `source` argument. Use this to add code."
        (props [("source", prop "The full Haskell source for the new cell.")] ["source"])
    , fn
        "replace_cell_source"
        "Replace a cell's entire source and re-run it. Pass the cell_id and the replacement Haskell source. Use to fix an existing cell."
        ( props
            [ ("cell_id", intProp "Cell to replace.")
            , ("new_source", prop "The replacement Haskell source.")
            ]
            ["cell_id", "new_source"]
        )
    , fn
        "execute_cell"
        "Run one cell by id; returns its outputs and any errors."
        (props [("cell_id", intProp "Cell to run.")] ["cell_id"])
    , fn
        "list_bindings"
        "List every value, function, and type already defined in the notebook session, with its type. Use BEFORE building on earlier work, to reuse an existing binding instead of recomputing it."
        (props [] [])
    , fn
        "check_type"
        "Get the type of an expression, or the kind/definition of a type or class you already know, without running it. To find a name you do not know, use find_function or find_by_type."
        (props [("expr", prop "An expression, value name, or type/class name.")] ["expr"])
    , fn
        "find_by_type"
        "Find an installed function whose TYPE matches a goal type (e.g. \"[Int] -> Int\"). Use when you know the type you need but not the name; differs from find_function, which searches by name/keyword."
        (props [("goal", prop "A goal type, e.g. \"[Int] -> Int\".")] ["goal"])
    , fn
        "scratchpad"
        "Run a SELF-CONTAINED Haskell snippet in an isolated session (cannot see notebook bindings). Put the snippet in the `code` argument."
        (props [("code", prop "The self-contained Haskell snippet to run.")] ["code"])
    , fn
        "find_package"
        "Find which Haskell package provides a capability you have not installed yet. Query a keyword or description (e.g. \"linear regression\", \"read csv\", \"plot a bar chart\"); returns the packages, the -- cabal: build-depends: line for a cell's first line, and key modules. The FIRST step for a new capability."
        ( props
            [
                ( "query"
                , prop
                    "A keyword or task, e.g. \"linear regression\", \"read csv\", \"plotting\"."
                )
            ]
            ["query"]
        )
    , fn
        "find_example_cell"
        "Search runnable example cells for a cell-shape idiom (e.g. \"read csv\", \"typed column\"); returns source to paste and adapt. To find which package or function does a task, use find_package or find_function."
        ( props
            [("query", prop "A shape idiom, e.g. \"read csv\" or \"typed column\".")]
            ["query"]
        )
    , fn
        "find_function"
        "Find a function by NAME or KEYWORD in the installed LIBRARIES, or list a library module's exports by passing a module name (\"DataFrame\", \"Granite.Svg\"). Returns ranked matches with module and signature; nothing on a true miss. This searches installed libraries, NOT your notebook — to find where something is defined or used in your notebook, use find_cells_by_content or list_cells. To search by TYPE use find_by_type."
        ( props
            [
                ( "query"
                , prop
                    "A keyword (\"animate\"), a type fragment (\"Double -> Picture\"), or a module name (\"Granite.Svg\")."
                )
            ]
            ["query"]
        )
    , fn
        "delete_cell"
        "Delete a cell from the notebook. Use this to remove a cell you cannot fix in place — e.g. a failing cell that is blocking you from inserting a new one."
        (props [("cell_id", prop "The id of the cell to delete.")] ["cell_id"])
    ]

-- | Build an object schema from (name, schema) properties plus a required list.
props :: [(Text, Value)] -> [Text] -> Value
props ps required =
    object
        [ "type" .= ("object" :: Text)
        , "properties" .= object [(K.fromText k, v) | (k, v) <- ps]
        , "required" .= required
        ]

dispatch :: Conn -> Text -> ToolCall -> IO (Either Text ToolOutcome)
dispatch conn base (ToolCall name args) =
    case resolveToolCall name args of
        Nothing -> pure (Left (unknownToolMsg name))
        Just (InsertCell, a) -> callTool conn base InsertCell (withInsertDefaults a)
        Just (tn, a) -> callTool conn base tn a

unknownToolMsg :: Text -> Text
unknownToolMsg name =
    "unknown tool '"
        <> name
        <> "'. Valid tools: list_cells, read_cell, find_cells_by_content, insert_cell, \
           \replace_cell_source, execute_cell, delete_cell, list_bindings, check_type, \
           \find_by_type, scratchpad, find_package, find_example_cell, find_function, \
           \search_capability."

{- | Fill the cell_type/language defaults the eval model often omits. Placement
needs no default: the server appends every new cell.
-}
withInsertDefaults :: Value -> Value
withInsertDefaults (Object o) =
    Object $
        def "cell_type" (String "CodeCell") $
            def "language" (String "Haskell") o
  where
    def k val m = if KM.member k m then m else KM.insert k val m
withInsertDefaults v = v

renderOutcome :: Either Text ToolOutcome -> Text
renderOutcome (Left e) = "transport error: " <> e
renderOutcome (Right (ToolOk v)) = trunc (enc v)
renderOutcome (Right (ToolErr v)) = "TOOL ERROR: " <> trunc (enc v)

enc :: Value -> Text
enc = TE.decodeUtf8 . LBS.toStrict . encode

trunc :: Text -> Text
trunc t = if T.length t > 6000 then T.take 6000 t <> " …[truncated]" else t
