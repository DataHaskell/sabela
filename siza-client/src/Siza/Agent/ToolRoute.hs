module Siza.Agent.ToolRoute (
    isStateQuery,
    stateQueryTools,
    Route (..),
    installSteer,
    isDiscoverName,
    isRecallName,
    isVerifyName,
    maxNameDrift,
    nameCandidates,
    normalizeToolCall,
    recoverCall,
    recoverTurn,
    routeCall,
    routeCallWith,
    routeCallWithin,
    schemaMatches,
    unwrapArgs,
) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Char (isAlpha)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.ToolName (ToolName, resolveToolCall, toolWireName)
import Sabela.AI.HoleRepair (editDistance)
import Sabela.LLM.Ollama.Client (ToolCall (..), Turn (..), rawWithCalls)
import Siza.Agent.Discover.Request (discoverQuery)
import Siza.Agent.Recall (recallToolName)

data Route
    = RouteDiscover Text Value
    | RouteRecall Value
    | RouteVerify Text Value
    | RouteTool ToolName Value
    | RouteBadArgs Text
    | RouteUnknown Text
    deriving (Eq, Show)

maxNameDrift :: Int
maxNameDrift = 2

routeCallWith :: [(Text, ([Text], [Text]))] -> ToolCall -> Route
routeCallWith vocab = routeCallWithin vocab vocab

{- | @routeCallWithin known recoverable@. A malformed call is rescued by
matching its arguments against @recoverable@, which is narrower than @known@
wherever two tools take the same arguments: shape recovery is for the weak
models on the chat surface, and a tool only an MCP client calls by exact name
would make every recovery ambiguous.
-}
routeCallWithin ::
    [(Text, ([Text], [Text]))] -> [(Text, ([Text], [Text]))] -> ToolCall -> Route
routeCallWithin vocab recoverable tc = case routeCall tc of
    RouteUnknown name -> case recoverCall recoverable tc of
        Just rc -> routeCall rc
        Nothing
            | null (nameCandidates (map fst vocab) name) -> RouteUnknown name
            | otherwise ->
                RouteBadArgs
                    (parseFailureHint name (schemaMatches recoverable (plainArgs tc)))
    RouteTool tn a
        | Just schema <- lookup (toolWireName tn) vocab
        , not (argsFit a schema) ->
            RouteBadArgs (badArgsHint (toolWireName tn) a schema)
    route -> route

recoverTurn :: [(Text, ([Text], [Text]))] -> Turn -> Turn
recoverTurn vocab t
    | calls == turnCalls t = t
    | otherwise = t{turnCalls = calls, turnRaw = rawWithCalls (turnRaw t) calls}
  where
    calls = map recoverOne (turnCalls t)
    recoverOne tc = fromMaybe tc (recoverCall vocab tc)

recoverCall :: [(Text, ([Text], [Text]))] -> ToolCall -> Maybe ToolCall
recoverCall vocab tc = case routeCall tc of
    RouteUnknown name -> recoverUnknown vocab tc name
    _ -> Nothing

recoverUnknown ::
    [(Text, ([Text], [Text]))] -> ToolCall -> Text -> Maybe ToolCall
recoverUnknown vocab tc name
    | null cands = Nothing
    | otherwise = case schemaMatches vocab args of
        [t] -> Just tc{tcName = t}
        _ -> case [t | t <- cands, consistent t] of
            [t] -> Just tc{tcName = t}
            _ -> Nothing
  where
    args = plainArgs tc
    cands = nameCandidates (map fst vocab) name
    consistent t = maybe False (argsFit args) (lookup t vocab)

schemaMatches :: [(Text, ([Text], [Text]))] -> Value -> [Text]
schemaMatches vocab v@(Object o)
    | KM.null o = []
    | otherwise = [t | (t, keys) <- vocab, argsFit v keys]
schemaMatches _ _ = []

argsFit :: Value -> ([Text], [Text]) -> Bool
argsFit v (propsK, reqK) =
    all (`elem` propsK) keys && all (`elem` keys) reqK
  where
    keys = argKeys v

argKeys :: Value -> [Text]
argKeys (Object o) = map K.toText (KM.keys o)
argKeys _ = []

badArgsHint :: Text -> Value -> ([Text], [Text]) -> Text
badArgsHint name args (propsK, reqK) =
    "bad arguments for '"
        <> name
        <> "': "
        <> T.intercalate "; " (missingPart <> extraPart)
        <> ". Re-send with exactly this tool's arguments."
  where
    given = argKeys args
    missing = filter (`notElem` given) reqK
    extra = filter (`notElem` propsK) given
    missingPart = ["missing required: " <> T.intercalate ", " missing | not (null missing)]
    extraPart = ["unrecognized: " <> T.intercalate ", " extra | not (null extra)]

nameCandidates :: [Text] -> Text -> [Text]
nameCandidates names name
    | not (T.any isAlpha name) = names
    | otherwise = filter corruptionOf names
  where
    corruptionOf t =
        editDistance name t <= maxNameDrift
            || t `T.isPrefixOf` name
            || (T.length name >= 4 && name `T.isPrefixOf` t)

parseFailureHint :: Text -> [Text] -> Text
parseFailureHint name matches =
    "could not parse tool call '"
        <> name
        <> "': the name matches no offered tool"
        <> ambiguity
        <> ". Re-send ONE call with an exact tool name and only that \
           \tool's arguments."
        <> installSteer name
  where
    ambiguity = case matches of
        [] -> " and the arguments fit no single tool's schema"
        ts ->
            " and the arguments match more than one tool ("
                <> T.intercalate ", " ts
                <> ")"

installSteer :: Text -> Text
installSteer name
    | "install" `T.isInfixOf` T.toLower name =
        " There is no install tool: declare a package with \
        \`-- cabal: build-depends: <package>` as the FIRST line of a cell instead."
    | otherwise = ""

plainArgs :: ToolCall -> Value
plainArgs tc = either (const (tcArgs tc)) id (unwrapArgs (tcArgs tc))

wrapperKeys :: [Text]
wrapperKeys = ["input", "arguments"]

unwrapArgs :: Value -> Either Text Value
unwrapArgs = go (3 :: Int)
  where
    go 0 v = Right v
    go n (Object o)
        | [(k, v)] <- KM.toList o
        , K.toText k `elem` wrapperKeys =
            case v of
                Object _ -> go (n - 1) v
                _ -> Left (K.toText k)
    go _ v = Right v

normalizeToolCall :: ToolCall -> ToolCall
normalizeToolCall tc = case unwrapArgs (tcArgs tc) of
    Right args -> tc{tcArgs = args}
    Left _ -> tc

isDiscoverName :: Text -> Bool
isDiscoverName name = T.takeWhile (/= ' ') (T.strip name) == "discover"

stateQueryTools :: [Text]
stateQueryTools = ["kernel_status", "await_idle", "list_cells", "list_bindings"]

isStateQuery :: Text -> Bool
isStateQuery = (`elem` stateQueryTools)

routeCall :: ToolCall -> Route
routeCall (ToolCall name rawArgs) = case unwrapArgs rawArgs of
    Left wrapper -> RouteBadArgs (badWrapperHint name wrapper)
    Right args
        | isDiscoverName name ->
            RouteDiscover (fromMaybe "" (discoverQuery name args)) args
        | isRecallName name -> RouteRecall args
        | isVerifyName name -> RouteVerify (argText "check" args) args
        | otherwise -> case resolveToolCall name args of
            Just (tn, a) -> RouteTool tn a
            Nothing -> RouteUnknown name

isVerifyName :: Text -> Bool
isVerifyName = (== "verify")

{- | Reading an elided result back is answered here, not in one entry point's
own wrapper, so both surfaces answer the tool their text names.
-}
isRecallName :: Text -> Bool
isRecallName = (== recallToolName)

argText :: Text -> Value -> Text
argText k (Object o) = case KM.lookup (K.fromText k) o of
    Just (String s) -> s
    _ -> ""
argText _ _ = ""

badWrapperHint :: Text -> Text -> Text
badWrapperHint name wrapper =
    "bad arguments for '"
        <> name
        <> "': the arguments were wrapped in a '"
        <> wrapper
        <> "' key that is not an object. Pass the tool's fields as the \
           \top-level arguments object, e.g. {\"query\": \"...\"}."
