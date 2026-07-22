{-# LANGUAGE OverloadedStrings #-}

{- | The IO fan-out of a discover call: build the notebook environment,
consult the session with qualified-name fallback, call the capability
channel, and run the one hidden-package probe. Split from
"Siza.Agent.DiscoverTool" for the module-size cap; pure classification stays
in "Siza.Agent.Discover.Classify".
-}
module Siza.Agent.Discover.Fetch (
    blankPayload,
    capabilityArgs,
    fetchNotebookEnv,
    fetchOk,
    fetchSession,
    probeHidden,
    queryVariants,
) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Char (isUpper)
import Data.List (sortOn)
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.ToolName (ToolName (..))
import Sabela.AI.Types (ToolOutcome (..))
import Siza.Agent.Discover.Classify (sessionAnswer)
import Siza.Agent.Discover.Interpret (envFromCells, parseCells)
import Siza.Agent.Discover.Types (
    Interpreted (..),
    NotebookEnv,
    SourceAnswer (..),
 )

{- | A backend payload that answers nothing: only scalar echoes (the query),
empty arrays, or nested blanks. A structured status card (hidden-package \/
not-found \/ ok) is substance; an @error@ card stays blank.
-}
blankPayload :: Value -> Bool
blankPayload (Object o) = case KM.lookup "status" o of
    Just (String st) -> st == "error"
    _ -> all blankPayload (KM.elems o)
blankPayload (Array a) = null a
blankPayload _ = True

{- | The query and its qualified-name fallback: a dotted @Alias.name@ query
whose last component is a value name retries as the bare name. A module-shaped
query (Upper last component) and prose stay as they are.
-}
queryVariants :: Text -> [Text]
queryVariants q = t : [bare | Just bare <- [unqualify t], bare /= t]
  where
    t = T.strip q
    unqualify s
        | T.any (== '.') s
        , not (" " `T.isInfixOf` s)
        , Just (c0, _) <- T.uncons s
        , isUpper c0
        , lastComp <- last (T.splitOn "." s)
        , Just (c, _) <- T.uncons lastComp
        , not (isUpper c) =
            Just lastComp
        | otherwise = Nothing

-- | The fuzzy capability call: the semantic flag carries the A/B lever.
capabilityArgs :: Bool -> Interpreted -> Value
capabilityArgs capSearch interp =
    object ["query" .= iRaw interp, "semantic" .= capSearch]

-- | The notebook environment from one full cell listing (aliases, bindings).
fetchNotebookEnv ::
    (ToolName -> Value -> IO (Either Text ToolOutcome)) -> IO NotebookEnv
fetchNotebookEnv call = do
    r <- call ListCells (object ["full" .= True])
    pure . envFromCells $ case r of
        Right (ToolOk v) -> parseCells v
        _ -> []

{- | The session answer: the resolved name, falling back through
'queryVariants'. Queried UNSCOPED (section 3.3): the module filter is a
post-union predicate at the merge, never a pre-query narrowing handed to one
backend — stage-0 browses the scoped module separately. 'Nothing' only when
every call failed outright.
-}
fetchSession ::
    (ToolName -> Value -> IO (Either Text ToolOutcome)) ->
    Interpreted ->
    IO (Maybe Value)
fetchSession call interp = go variants Nothing
  where
    variants =
        iName interp
            : [v | v <- queryVariants (iRaw interp), v /= iName interp]
    go [] lastAnswered = pure lastAnswered
    go (v : rest) lastAnswered = do
        r <- call FindFunction (object ["query" .= v])
        case r of
            Right (ToolOk payload)
                | not (blankPayload payload) -> pure (Just payload)
                | otherwise -> go rest (Just payload)
            _ -> go rest lastAnswered

-- | One call whose OK payload is kept; errors read as channel-unavailable.
fetchOk ::
    (ToolName -> Value -> IO (Either Text ToolOutcome)) ->
    ToolName ->
    Value ->
    IO (Maybe Value)
fetchOk call tn args = do
    r <- call tn args
    pure $ case r of
        Right (ToolOk v) -> Just v
        _ -> Nothing

{- | When no source produced a session card, browse ONE module of each
candidate package (bounded) — the probe that classifies each candidate
installed \/ hidden \/ absent by session evidence instead of guessing.
Probing only the first candidate left every other package's install state
decided by hackage membership alone — a false absent-known.
-}
probeHidden ::
    (ToolName -> Value -> IO (Either Text ToolOutcome)) ->
    Interpreted ->
    [SourceAnswer] ->
    IO [SourceAnswer]
probeHidden call interp answers
    | any (isJust . saCard) answers = pure []
    | sessionDown = pure []
    | otherwise = fmap concat . mapM probeOne $ take maxProbes probeTargets
  where
    sessionDown =
        any (\a -> saSource a == "session" && not (saOk a)) answers
    probeOne m = do
        r <- fetchOk call FindFunction (object ["query" .= m])
        pure [sessionAnswer interp r | isJust r]
    -- One module per distinct candidate package: packages the query itself
    -- names probe first (R2.4 — fuzzy-channel bucket order must not exhaust
    -- the budget before the named package), then disclosure order (stable).
    probeTargets =
        nubOrd
            [ m
            | (_, mods) <-
                sortOn
                    (not . queryNamed . fst)
                    (nubOrdOn fst (concatMap saPkgModules answers))
            , m <- take 1 mods
            ]
    queryNamed p =
        T.toLower p `elem` map T.toLower (iName interp : iTerms interp)

-- | Bounded probe fan-out: at most this many one-module browses per call.
maxProbes :: Int
maxProbes = 3

nubOrd :: (Ord a) => [a] -> [a]
nubOrd = nubOrdOn id

nubOrdOn :: (Ord b) => (a -> b) -> [a] -> [a]
nubOrdOn f = go Set.empty
  where
    go _ [] = []
    go seen (x : xs)
        | f x `Set.member` seen = go seen xs
        | otherwise = x : go (Set.insert (f x) seen) xs
