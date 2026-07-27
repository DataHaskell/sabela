module Siza.Agent.Tools (
    catalogue,
    catalogueWith,
    dispatch,
    offeredArgKeys,
    toolSurfacePrompt,
    toolSurfaceHelp,
    offeredNames,
    withInsertDefaults,
    renderOutcome,
    unknownToolMsg,
) where

import Data.Aeson (Value (..), encode, object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Sabela.AI.Capabilities.ToolName (ToolName (..))
import Sabela.AI.Types (ToolOutcome (..))
import Sabela.AI.Verdict (VerdictClass (..), verdictTag)
import Siza.Agent.Ack (reconcileWrite)
import Siza.Agent.Discover.Request (requestProperties, requestRequired)
import Siza.Agent.DiscoverTool (
    discoverToolDescription,
    runDiscoverCall,
 )
import Siza.Agent.OutcomeDistill (distillOutcome)
import Siza.Agent.ToolRoute (Route (..), installSteer, routeCallWith)
import Siza.Transport (Conn, callTool)
import System.Environment (lookupEnv)

import Sabela.LLM.Ollama.Client (ToolCall (..))

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

catalogueWith :: Bool -> [Value]
catalogueWith _ = baseCatalogue

catalogue :: IO [Value]
catalogue = pure baseCatalogue

baseCatalogue :: [Value]
baseCatalogue =
    [ fn
        "list_cells"
        "Map of EVERY cell in the notebook (the whole notebook in one call): each cell's id, position, type, language, the bindings it `defines`, and whether it errored. By default each cell shows only its first line; pass `full: true` to include each cell's source. To find which cell defines or uses a name, scan `defines` here or search for it with discover, which reports notebook cells alongside library results."
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
        "Get the type of an expression, or the kind/definition of a type or class you already know, without running it. To find a name you do not know, use discover."
        (props [("expr", prop "An expression, value name, or type/class name.")] ["expr"])
    , fn
        "discover"
        discoverToolDescription
        (props requestProperties requestRequired)
    , fn
        "try"
        "Try candidate code without touching the notebook: it sees the notebook's live bindings and may declare a candidate-only dependency to test with. Always safe to call — nothing is added, changed, or removed. Returns the value or output on success, or a diagnostic saying why it could not run."
        ( props
            [ ("code", prop "The code to try.")
            ,
                ( "language"
                , object
                    [ "type" .= ("string" :: Text)
                    , "enum" .= (["Haskell", "Python"] :: [Text])
                    , "description" .= ("Language. Default: Haskell." :: Text)
                    ]
                )
            ]
            ["code"]
        )
    , fn
        "delete_cell"
        "Delete a cell from the notebook. Use this to remove a cell you cannot fix in place — e.g. a failing cell that is blocking you from inserting a new one."
        (props [("cell_id", prop "The id of the cell to delete.")] ["cell_id"])
    , fn
        "kernel_status"
        "Lock-free kernel status. Always answers, even while a cell holds the run-lock, so you can tell \"busy\" (a slow cell) from \"wedged\" (unresponsive) without blocking. Returns a typed `state` tag (cold | idle | executing | building) plus `buildingMs` while compiling, so a long-but-progressing build reads differently from a wedge."
        (props [] [])
    , fn
        "await_idle"
        "Block until the running cell or build finishes (a bounded ~45s long-poll), then return the fresh kernel status. `waited` is one of: idle | settled | timedOut | kernelDead. Call this when a tool says the kernel is busy — do NOT re-run the cell. Re-call while it reports timedOut (the kernel is still working); if the reply carries a `resource` line the cell looks non-terminating — interrupt, shrink the work, rewrite. kernelDead means the kernel died: kernel_restart."
        (props [] [])
    , fn
        "interrupt"
        "Abort the cell the Haskell kernel is currently running (group SIGINT). No-op when the kernel is idle. Use this when a cell is stuck before reaching for kernel_restart."
        (props [] [])
    , fn
        "kernel_restart"
        "Hard-reset the Haskell kernel: force-kill the kernel process (even a wedged one that ignores interrupt) and respawn it clean — reusing the installed packages without rebuilding, and WITHOUT re-running any cells. Returns immediately; poll kernel_status until idle. This is how you recover a stuck or wedged kernel."
        (props [] [])
    ]

props :: [(Text, Value)] -> [Text] -> Value
props ps required =
    object
        [ "type" .= ("object" :: Text)
        , "properties" .= object [(K.fromText k, v) | (k, v) <- ps]
        , "required" .= required
        ]

dispatch :: Conn -> Text -> ToolCall -> IO (Either Text ToolOutcome)
dispatch conn base tc = case routeCallWith offeredArgKeys tc of
    RouteBadArgs hint ->
        pure (Right (ToolErr (object ["error" .= hint])))
    RouteDiscover q args -> do
        capSearch <- isJust <$> lookupEnv "SABELA_CAPABILITY_SEARCH"
        Right <$> runDiscoverCall capSearch (callTool conn base) q args
    RouteTool InsertCell a ->
        reconcile =<< callTool conn base InsertCell (withInsertDefaults a)
    RouteTool tn a -> reconcile =<< callTool conn base tn a
    RouteUnknown name -> pure (Left (unknownToolMsg name))
  where
    reconcile = reconcileWrite (callTool conn base)

unknownToolMsg :: Text -> Text
unknownToolMsg name =
    "unknown tool '"
        <> name
        <> "'. Valid tools: "
        <> T.intercalate ", " offeredNames
        <> "."
        <> installSteer name

offeredNames :: [Text]
offeredNames = map fst offeredArgKeys

offeredArgKeys :: [(Text, ([Text], [Text]))]
offeredArgKeys =
    [ (n, schemaKeys f)
    | Object o <- baseCatalogue
    , Just (Object f) <- [KM.lookup "function" o]
    , Just (String n) <- [KM.lookup "name" f]
    ]

schemaKeys :: KM.KeyMap Value -> ([Text], [Text])
schemaKeys f = case KM.lookup "parameters" f of
    Just (Object p) -> (propKeys p, reqKeys p)
    _ -> ([], [])
  where
    propKeys p = case KM.lookup "properties" p of
        Just (Object ps) -> map K.toText (KM.keys ps)
        _ -> []
    reqKeys p = case KM.lookup "required" p of
        Just (Array rs) -> [r | String r <- foldr (:) [] rs]
        _ -> []

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
renderOutcome (Right (ToolOk v)) = trunc (enc (distillOutcome v))
renderOutcome (Right (ToolErr v)) = errLabel v <> ": " <> trunc (enc (distillOutcome v))

errLabel :: Value -> Text
errLabel (Object o)
    | Just (String t) <- KM.lookup "verdict" o
    , t == verdictTag VerdictDiagnostic =
        "CODE ISSUE"
    | Just (String t) <- KM.lookup "verdict" o
    , t == verdictTag VerdictCouldNotRun =
        "NOT RUN"
errLabel _ = "TOOL ERROR"

enc :: Value -> Text
enc = TE.decodeUtf8 . LBS.toStrict . encode

trunc :: Text -> Text
trunc t = if T.length t > 6000 then T.take 6000 t <> " …[truncated]" else t

toolSurfacePrompt :: Text
toolSurfacePrompt =
    T.unlines $
        ["Available tools:", ""]
            <> concatMap group toolGroups
  where
    group (label, names) =
        ("* " <> label <> ":")
            : [ "    * " <> n <> ": " <> synopsis n
              | n <- names
              , not (T.null (synopsis n))
              ]
                <> [""]
    synopsis n = firstSentence (fromMaybe "" (lookup n catalogueDescriptions))
    firstSentence d = go (T.splitOn ". " d)
      where
        go [] = d
        go [s] = s
        go (s : ss)
            | endsAbbrev s = s <> ". " <> go ss
            | otherwise = s <> "."
        endsAbbrev s = any (`T.isSuffixOf` s) ["e.g", "i.e", "etc", "cf", "vs"]

toolGroups :: [(Text, [Text])]
toolGroups =
    [
        ( "Notebook"
        ,
            [ "list_cells"
            , "read_cell"
            , "insert_cell"
            , "replace_cell_source"
            , "execute_cell"
            , "delete_cell"
            ]
        )
    ,
        ( "Finding things"
        , ["discover", "check_type", "list_bindings"]
        )
    , ("Trying code", ["try"])
    ,
        ( "Kernel"
        , ["kernel_status", "await_idle", "interrupt", "kernel_restart"]
        )
    ]

catalogueDescriptions :: [(Text, Text)]
catalogueDescriptions =
    [ (n, d)
    | Object o <- baseCatalogue
    , Just (Object f) <- [KM.lookup "function" o]
    , Just (String n) <- [KM.lookup "name" f]
    , Just (String d) <- [KM.lookup "description" f]
    ]

toolSurfaceHelp :: Text
toolSurfaceHelp =
    T.unlines $
        ["Tools offered to an agent driving a Sabela notebook.", ""]
            <> concatMap group toolGroups
  where
    group (label, names) =
        (label <> ":")
            : concat [entry n | n <- names, isJust (lookup n catalogueDescriptions)]
                <> [""]
    entry n =
        [ "  " <> n <> argSummary n
        , "      " <> fromMaybe "" (lookup n catalogueDescriptions)
        , ""
        ]
    argSummary n = case lookup n catalogueArgs of
        Just (ps, req)
            | not (null ps) ->
                " " <> T.unwords [render p (p `elem` req) | p <- ps]
        _ -> ""
    render p True = "<" <> p <> ">"
    render p False = "[" <> p <> "]"

catalogueArgs :: [(Text, ([Text], [Text]))]
catalogueArgs =
    [ (n, (props', req))
    | Object o <- baseCatalogue
    , Just (Object f) <- [KM.lookup "function" o]
    , Just (String n) <- [KM.lookup "name" f]
    , Just (Object params) <- [KM.lookup "parameters" f]
    , let props' = case KM.lookup "properties" params of
            Just (Object ps) -> map K.toText (KM.keys ps)
            _ -> []
    , let req = case KM.lookup "required" params of
            Just (Array rs) -> [r | String r <- foldr (:) [] rs]
            _ -> []
    ]
