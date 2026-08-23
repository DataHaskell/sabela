module Siza.Agent.Tools (
    catalogue,
    catalogueFor,
    catalogueWith,
    dispatch,
    offeredArgKeys,
    chatArgKeys,
    toolSurfacePrompt,
    toolSurfaceHelp,
    toolGroups,
    offeredNames,
    dispatchableNames,
    withInsertDefaults,
    renderOutcome,
    unknownToolMsg,
) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.Capabilities.ToolName (ToolName (..))
import Sabela.AI.Types (ToolOutcome (..))
import Siza.Agent.Ack (reconcileWrite, withDeclaredModules)
import Siza.Agent.Bulk (
    applyCellFilters,
    narrowCellOutputs,
    narrowOutcome,
    runReplaceCells,
    serverArgs,
 )
import Siza.Agent.DiscoverTool (runDiscoverCall)
import Siza.Agent.Recall (answerRecall, recallToolDef, recallToolName)
import Siza.Agent.Render (renderOutcome, withInsertDefaults)
import Siza.Agent.Stack (Surface (..), SurfacePolicy (..), surfacePolicy)
import Siza.Agent.ToolRoute (Route (..), installSteer, routeCallWithin)
import Siza.Agent.Tools.Catalogue (baseCatalogue, mcpOnlyCatalogue)
import Siza.Agent.VerifyTool (runVerifyCall)
import Siza.Transport (Conn, callTool)

import Sabela.LLM.Ollama.Client (ToolCall (..))

{- | @catalogueWith elides@, the primitive. Only a surface that elides results
advertises the tool that reads them back; 'dispatch' answers that tool on
either surface, advertised or not. Production callers use 'catalogueFor'.
-}
catalogueWith :: Bool -> [Value]
catalogueWith elides
    | elides = baseCatalogue ++ [recallToolDef]
    | otherwise = baseCatalogue ++ mcpOnlyCatalogue

-- | The catalogue a surface is served, read off that surface's own policy.
catalogueFor :: Surface -> [Value]
catalogueFor = catalogueWith . spElides . surfacePolicy

{- | The catalogue the episode drivers serve. They run the chat loop, so they
elide, so they are served the catalogue that carries the tool the elision
markers name.
-}
catalogue :: IO [Value]
catalogue = pure (catalogueFor ChatSurface)

dispatch :: Conn -> Text -> ToolCall -> IO (Either Text ToolOutcome)
dispatch conn base tc = case routeCallWithin offeredArgKeys chatArgKeys tc of
    RouteBadArgs hint ->
        pure (Right (ToolErr (object ["error" .= hint])))
    -- No retrieval source is an experiment arm: a channel gated behind an
    -- environment variable is one the model silently loses, and a miss it
    -- causes is indistinguishable from the answer not existing.
    RouteDiscover q args ->
        Right <$> runDiscoverCall True (callTool conn base) q args
    RouteRecall args -> Right <$> answerRecall args
    RouteVerify chk _ ->
        Right <$> runVerifyCall (callTool conn base) chk
    RouteTool InsertCell a ->
        reconcile a =<< callTool conn base InsertCell (withInsertDefaults a)
    -- The narrowing tools answer from the server's own payload, so the filter
    -- never has to agree with a second implementation of what a cell is.
    RouteTool ListCells a ->
        fmap (narrowOutcome (applyCellFilters a))
            <$> callTool conn base ListCells (serverArgs a)
    RouteTool ReadCell a ->
        fmap (narrowOutcome (narrowCellOutputs a))
            <$> callTool conn base ReadCell (serverArgs a)
    RouteTool tn a -> reconcile a =<< callTool conn base tn a
    RouteUnknown "replace_cells" -> Right <$> runReplaceCells conn base (tcArgs tc)
    RouteUnknown name -> pure (Left (unknownToolMsg name))
  where
    reconcile a out =
        withDeclaredModules a =<< reconcileWrite (callTool conn base) out

unknownToolMsg :: Text -> Text
unknownToolMsg name =
    "unknown tool '"
        <> name
        <> "'. Valid tools: "
        <> T.intercalate ", " dispatchableNames
        <> "."
        <> installSteer name

{- | Every name the router places, catalogued or intercepted. What a caller who
just missed is told to use must be what the router will actually take.
-}
dispatchableNames :: [Text]
dispatchableNames = offeredNames ++ [recallToolName]

-- | The names the chat prompt offers. The MCP-only tools are not among them.
offeredNames :: [Text]
offeredNames = map fst chatArgKeys

offeredArgKeys :: [(Text, ([Text], [Text]))]
offeredArgKeys =
    [ (n, schemaKeys f)
    | Object o <- baseCatalogue ++ mcpOnlyCatalogue
    , Just (Object f) <- [KM.lookup "function" o]
    , Just (String n) <- [KM.lookup "name" f]
    ]

{- | The chat surface's tools: what its prompt names, and what a malformed call
is matched against by argument shape. The MCP-only tools are left out of both.
@propose_edit@ takes the same arguments as @replace_cell_source@, so including
it would make every recovery of a mangled replace call ambiguous, and those
calls come from the surface that has no @propose_edit@ anyway.
-}
chatArgKeys :: [(Text, ([Text], [Text]))]
chatArgKeys =
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

{- | Names, grouped. The request already carries every description in the
tools array, so the prompt keeps no second, shorter copy of one: a truncated
description is a description that disagrees with the tool.
-}
toolSurfacePrompt :: Text
toolSurfacePrompt =
    T.unlines $
        ["Available tools:", ""]
            <> [ "* " <> label <> ": " <> T.intercalate ", " names
               | (label, names) <- toolGroups
               ]
            <> [""]

{- | The prompt's tool list, grouped. Membership is filtered against the
catalogue and whatever the groups do not claim is appended, so a catalogued
tool cannot go unnamed in the prompt and an empty group cannot be named.
-}
toolGroups :: [(Text, [Text])]
toolGroups =
    [ (label, members)
    | (label, names) <- groups
    , let members = [n | n <- names, n `elem` catalogued]
    , not (null members)
    ]
        ++ [("Other", unclaimed) | not (null unclaimed)]
  where
    catalogued = map fst catalogueDescriptions
    unclaimed = [n | n <- catalogued, n `notElem` concatMap snd groups]

groups :: [(Text, [Text])]
groups =
    [
        ( "Notebook"
        ,
            [ "list_cells"
            , "export_notebook"
            , "read_cell"
            , "insert_cell"
            , "replace_cell_source"
            , "replace_cells"
            , "propose_edit"
            , "execute_cell"
            , "delete_cell"
            ]
        )
    ,
        ( "Finding things"
        , ["discover", "check_type", "list_bindings", "read_source"]
        )
    ,
        ( "Files and data"
        , ["list_files", "read_file"]
        )
    , ("Trying code", ["try"])
    , ("Checking a claim", ["verify"])
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
