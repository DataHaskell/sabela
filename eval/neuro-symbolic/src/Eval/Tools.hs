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
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Sabela.AI.Capabilities.ToolName (ToolName (..), parseToolName)
import Sabela.AI.Types (ToolOutcome (..))
import Siza.Transport (Conn, callTool)

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

catalogue :: [Value]
catalogue =
    [ fn
        "list_cells"
        "List all cells: id, type, language, hasError, first line."
        (props [] [])
    , fn
        "read_cell"
        "Read a cell's full source and outputs."
        (props [("cell_id", intProp "Cell id from list_cells.")] ["cell_id"])
    , fn
        "insert_cell"
        "Append a new Haskell cell with the given source and run it. Use this to add code."
        (props [("source", prop "Full Haskell source for the new cell.")] ["source"])
    , fn
        "replace_cell_source"
        "Replace a cell's entire source and re-run it. Use to fix an existing cell."
        ( props
            [ ("cell_id", intProp "Cell to replace.")
            , ("new_source", prop "Replacement Haskell source.")
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
        "Run a SELF-CONTAINED Haskell snippet in an isolated session (cannot see notebook bindings)."
        (props [("code", prop "Self-contained Haskell to evaluate.")] ["code"])
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
        "Find a function by NAME or KEYWORD in the installed modules, or list a module's exports by passing a module name (\"DataFrame\", \"Granite.Svg\"). Returns the best-matching functions with module and signature, ranked; nothing on a true miss. To search by TYPE use find_by_type."
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
    case parseToolName name of
        Nothing -> pure (Left (unknownToolMsg name))
        Just InsertCell -> callTool conn base InsertCell (withInsertDefaults args)
        Just tn -> callTool conn base tn args

unknownToolMsg :: Text -> Text
unknownToolMsg name =
    "unknown tool '"
        <> name
        <> "'. Valid tools: list_cells, read_cell, insert_cell, replace_cell_source, \
           \execute_cell, delete_cell, list_bindings, check_type, find_by_type, \
           \scratchpad, find_package, find_example_cell, find_function."

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
