{- | The honest request boundary (R1.7, the M8 laundering class): resolve a
tool call's NAME and normalise its argument shape BEFORE any guard or
dispatcher sees it, so an offered tool name can never be answered with
"unknown tool" because of how the arguments were wrapped. Routing is by
name alone; argument-shape problems answer with a shape-correcting hint.
R9-T5 adds schema-match recovery for garbled names: a unique match
dispatches (stamped by 'recoverTurn'); ambiguity reprompts with the parse
failure; a word-like foreign name stays honestly unknown.
-}
module Siza.Agent.ToolRoute (
    Route (..),
    isDiscoverName,
    maxNameDrift,
    nameCandidates,
    normalizeToolCall,
    recoverCall,
    recoverTurn,
    routeCall,
    routeCallWith,
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

import Sabela.AI.Capabilities.ToolName (ToolName, resolveToolCall)
import Sabela.AI.HoleRepair (editDistance)
import Sabela.LLM.Ollama.Client (ToolCall (..), Turn (..), rawWithCalls)
import Siza.Agent.Discover.Request (discoverQuery)

-- | Where a normalised tool call goes; the boundary's whole vocabulary.
data Route
    = RouteDiscover Text Value
    | RouteTool ToolName Value
    | RouteBadArgs Text
    | RouteUnknown Text
    deriving (Eq, Show)

-- | Max Levenshtein drift a corrupted name may sit from its recovery.
maxNameDrift :: Int
maxNameDrift = 2

{- | 'routeCall' plus the R9-T5 garbled-name recovery over @vocab@ (name,
(property keys, required keys)): a unique schema match dispatches; garble
with no unique match reprompts naming the parse failure; a foreign
word-like name stays 'RouteUnknown' — no over-acceptance.
-}
routeCallWith :: [(Text, ([Text], [Text]))] -> ToolCall -> Route
routeCallWith vocab tc = case routeCall tc of
    RouteUnknown name -> case recoverCall vocab tc of
        Just rc -> routeCall rc
        Nothing
            | null (nameCandidates (map fst vocab) name) -> RouteUnknown name
            | otherwise ->
                RouteBadArgs
                    (parseFailureHint name (schemaMatches vocab (plainArgs tc)))
    route -> route

{- | Rename every unresolvable-but-uniquely-recoverable call in a turn and
stamp the corrected calls into 'turnRaw', so the transcript always records
the call its result answers (R8.4). Resolvable and ambiguous calls pass
through untouched.
-}
recoverTurn :: [(Text, ([Text], [Text]))] -> Turn -> Turn
recoverTurn vocab t
    | calls == turnCalls t = t
    | otherwise = t{turnCalls = calls, turnRaw = rawWithCalls (turnRaw t) calls}
  where
    calls = map recoverOne (turnCalls t)
    recoverOne tc = fromMaybe tc (recoverCall vocab tc)

{- | The unique recovery of a garbled call, if any: only a call whose name
fails resolution is touched; schema evidence decides first (the argument
keys fit exactly one offered schema), then a unique name candidate whose
schema the arguments fit. Anything else recovers nothing.
-}
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

{- | The offered tools whose schema the argument object uniquely evidences:
every given key among the tool's properties, every required key given.
Empty or non-object args carry no schema evidence.
-}
schemaMatches :: [(Text, ([Text], [Text]))] -> Value -> [Text]
schemaMatches vocab v@(Object o)
    | KM.null o = []
    | otherwise = [t | (t, keys) <- vocab, argsFit v keys]
schemaMatches _ _ = []

-- | Do the argument keys fit one (properties, required) schema?
argsFit :: Value -> ([Text], [Text]) -> Bool
argsFit (Object o) (propsK, reqK) =
    all (`elem` propsK) keys && all (`elem` keys) reqK
  where
    keys = map K.toText (KM.keys o)
argsFit _ _ = False

{- | The offered names a garbled name is a plausible corruption of: within
'maxNameDrift' edits, an offered-name prefix with trailing garbage (the
thinking-fused class), a >=4-char truncation — or, for letterless garble,
every offered name (the schema alone must decide). A word-like foreign
name yields nothing.
-}
nameCandidates :: [Text] -> Text -> [Text]
nameCandidates names name
    | not (T.any isAlpha name) = names
    | otherwise = filter corruptionOf names
  where
    corruptionOf t =
        editDistance name t <= maxNameDrift
            || t `T.isPrefixOf` name
            || (T.length name >= 4 && name `T.isPrefixOf` t)

{- | One reprompt naming the parse failure — never an execution, never the
catalogue: the model must re-send a corrected call.
-}
parseFailureHint :: Text -> [Text] -> Text
parseFailureHint name matches =
    "could not parse tool call '"
        <> name
        <> "': the name matches no offered tool"
        <> ambiguity
        <> ". Re-send ONE call with an exact tool name and only that \
           \tool's arguments."
  where
    ambiguity = case matches of
        [] -> " and the arguments fit no single tool's schema"
        ts ->
            " and the arguments match more than one tool ("
                <> T.intercalate ", " ts
                <> ")"

-- | A call's unwrapped argument object (still-wrong shapes pass through).
plainArgs :: ToolCall -> Value
plainArgs tc = either (const (tcArgs tc)) id (unwrapArgs (tcArgs tc))

-- | Argument keys weak callers wrap the real argument object in.
wrapperKeys :: [Text]
wrapperKeys = ["input", "arguments"]

{- | Unwrap up to a few nested single-key @input@/@arguments@ envelopes.
'Left' names the wrapper when one is present but its value is not an object
(the still-wrong shape that earns a hint, never "unknown tool").
-}
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

{- | Normalise a call for the guard seams (futility, history ledger): the
guards must see the same argument keys the dispatcher resolves, or a wrapped
discover call would bypass dedup. A still-wrong shape passes through
untouched so 'routeCall' can name it.
-}
normalizeToolCall :: ToolCall -> ToolCall
normalizeToolCall tc = case unwrapArgs (tcArgs tc) of
    Right args -> tc{tcArgs = args}
    Left _ -> tc

{- | The one discovery tool routes by name, args or no args ('discoverQuery'
may still recover a name-baked query).
-}
isDiscoverName :: Text -> Bool
isDiscoverName name = T.takeWhile (/= ' ') (T.strip name) == "discover"

{- | Route a call: name resolution never depends on argument shape, so an
offered name cannot land in 'RouteUnknown' by being wrapped (P4/M8).
-}
routeCall :: ToolCall -> Route
routeCall (ToolCall name rawArgs) = case unwrapArgs rawArgs of
    Left wrapper -> RouteBadArgs (badWrapperHint name wrapper)
    Right args
        | isDiscoverName name ->
            RouteDiscover (fromMaybe "" (discoverQuery name args)) args
        | otherwise -> case resolveToolCall name args of
            Just (tn, a) -> RouteTool tn a
            Nothing -> RouteUnknown name

badWrapperHint :: Text -> Text -> Text
badWrapperHint name wrapper =
    "bad arguments for '"
        <> name
        <> "': the arguments were wrapped in a '"
        <> wrapper
        <> "' key that is not an object. Pass the tool's fields as the \
           \top-level arguments object, e.g. {\"query\": \"...\"}."
