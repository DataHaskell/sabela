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
import Data.Foldable (toList)
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
        "ghci_query"
        "Introspect the live session without mutating it: type/info/kind of an expression or name, browse a module's exports, doc a name, holefits a goal type, or list the session's bindings. Use browse <Module> to discover an installed package's real API. Use holefits with a concrete goal type to list in-scope names that fit it, so you pick a real name instead of inventing one. Use bindings to list every variable currently bound with its type."
        ( props
            [
                ( "op"
                , object
                    [ "type" .= ("string" :: Text)
                    , "enum"
                        .= (["type", "info", "kind", "browse", "doc", "holefits", "bindings"] :: [Text])
                    ]
                )
            ,
                ( "arg"
                , prop
                    "Expression, name, or type; for browse a module name like \"DataFrame\"; for holefits a concrete typed hole like \"_ :: [Int] -> Int\"; for bindings, ignored."
                )
            ]
            ["op", "arg"]
        )
    , fn
        "scratchpad"
        "Run a SELF-CONTAINED Haskell snippet in an isolated session (cannot see notebook bindings)."
        (props [("code", prop "Self-contained Haskell to evaluate.")] ["code"])
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
        Just InsertCell -> do
            anchor <- endAnchor conn base
            callTool conn base InsertCell (withInsertDefaults anchor args)
        Just tn -> callTool conn base tn args

unknownToolMsg :: Text -> Text
unknownToolMsg name =
    "unknown tool '"
        <> name
        <> "'. Valid tools: list_cells, read_cell, insert_cell, replace_cell_source, \
           \execute_cell, ghci_query, scratchpad. To browse a module use ghci_query \
           \with op=browse."

withInsertDefaults :: Int -> Value -> Value
withInsertDefaults anchor (Object o) =
    Object $
        def "after_cell_id" (Number (fromIntegral anchor)) $
            def "cell_type" (String "CodeCell") $
                def "language" (String "Haskell") o
  where
    def k val m = if KM.member k m then m else KM.insert k val m
withInsertDefaults _ v = v

endAnchor :: Conn -> Text -> IO Int
endAnchor conn base = do
    r <- callTool conn base ListCells (object [])
    pure $ case r of
        Right (ToolOk cells) -> let n = maxCellId cells in if n <= 0 then -1 else n
        _ -> -1

maxCellId :: Value -> Int
maxCellId (Array a) = maximum (0 : [i | Object c <- toList a, Just i <- [idOf c]])
  where
    idOf c = case KM.lookup "id" c of
        Just (Number s) -> Just (round s)
        _ -> Nothing
maxCellId _ = 0

renderOutcome :: Either Text ToolOutcome -> Text
renderOutcome (Left e) = "transport error: " <> e
renderOutcome (Right (ToolOk v)) = trunc (enc v)
renderOutcome (Right (ToolErr v)) = "TOOL ERROR: " <> trunc (enc v)

enc :: Value -> Text
enc = TE.decodeUtf8 . LBS.toStrict . encode

trunc :: Text -> Text
trunc t = if T.length t > 6000 then T.take 6000 t <> " …[truncated]" else t
