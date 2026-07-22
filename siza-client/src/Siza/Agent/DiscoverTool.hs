{-# LANGUAGE OverloadedStrings #-}

{- | The model-facing @discover@ tool: fan out IN UNION over the notebook
environment, the live session, the capability channel and the Hackage name
list, and merge every source's answer into one bounded, ranked envelope
(docs/discover/search-api.md). No first-hit ladder: a miss in one source can
never discard another source's true answer.
-}
module Siza.Agent.DiscoverTool (
    blankPayload,
    discoverArgs,
    discoverKey,
    discoverPlan,
    discoverQuery,
    discoverToolDescription,
    packageShaped,
    queryVariants,
    runDiscoverCall,
    runDiscoverRequest,
    runDiscoverTool,
) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Char (isDigit, isLower)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.Capabilities.ToolName (
    ToolName (..),
    primaryArgKey,
 )
import Sabela.AI.Types (ToolOutcome (..))
import Siza.Agent.Discover.Advice (setField)
import Siza.Agent.Discover.Classify (
    candidatePackages,
    capabilityAnswer,
    envAnswer,
    sessionAnswer,
 )
import Siza.Agent.Discover.Construct (
    attachProducers,
    constructAnswers,
    constructEnvelope,
 )
import Siza.Agent.Discover.Envelope (badRequest, boundEnvelope, schemaPromise)
import Siza.Agent.Discover.Exact (stageZero)
import Siza.Agent.Discover.Fetch (
    blankPayload,
    capabilityArgs,
    fetchNotebookEnv,
    fetchOk,
    fetchSession,
    probeHidden,
    queryVariants,
 )
import Siza.Agent.Discover.Goal (goalFromArgs)
import Siza.Agent.Discover.Hackage (hackageInfoFor, hackageMatching)
import Siza.Agent.Discover.Interpret (interpret)
import Siza.Agent.Discover.Inventory (inventoryEnvelope, topicTokens)
import Siza.Agent.Discover.Merge (discoverEnvelopeScoped)
import Siza.Agent.Discover.ProducerCard (establishedFallback)
import Siza.Agent.Discover.Request (
    DiscoverMode (..),
    DiscoverRequest (..),
    defaultRequest,
    discoverKey,
    discoverQuery,
    parseRequest,
    scopeFallbackQuery,
 )
import Siza.Agent.Discover.Types (
    HackageInfo,
    Interpreted (..),
    NotebookEnv,
    SourceAnswer,
    StandingGoal,
 )

{- | Backend fan-out order for a query, by shape: a type expression searches by
type first, a single identifier by name first, prose by capability first. The
Bool lever gates SHIP semantic enrichment INSIDE the capability call only —
no arm ever drops the lexical channel (search-api.md section 2).
-}
discoverPlan :: Bool -> Text -> [ToolName]
discoverPlan _ q = plan
  where
    plan
        | "->" `T.isInfixOf` q || "::" `T.isInfixOf` q =
            [FindByType, FindFunction, SearchCapability]
        | length (T.words (T.strip q)) <= 1 = [FindFunction, SearchCapability]
        | otherwise = [SearchCapability, FindFunction]

-- | The backend's argument object, keyed by its own primary key.
discoverArgs :: ToolName -> Text -> Value
discoverArgs tn q =
    object [K.fromText (fromMaybe "query" (primaryArgKey tn)) .= q]

-- | A query that could name a Hackage package: one lowercase hyphenated token.
packageShaped :: Text -> Bool
packageShaped q = case T.uncons t of
    Just (c, _) ->
        isLower c && T.all (\ch -> isLower ch || isDigit ch || ch == '-') t
    Nothing -> False
  where
    t = T.strip q

{- | The honest tool description (R1.7): its field promises are generated from
the envelope schema ('schemaPromise'), so contract drift is impossible.
-}
discoverToolDescription :: Text
discoverToolDescription =
    "Find a function, package, or module in one call: pass a NAME (\"divvy\"), \
    \a goal TYPE (\"[Int] -> Int\"), a MODULE (\"Granite.Svg\"), or a \
    \plain-language DESCRIPTION. Consults the live session, the local Hoogle \
    \index, and the Hackage name list, and unions their answers into one \
    \ranked result (exact name first). "
        <> schemaPromise
        <> " A miss lists what was consulted. This searches LIBRARIES, not \
           \your notebook — for the notebook use find_cells_by_content."

-- | Query-only compatibility surface: default scope, limit and mode.
runDiscoverTool ::
    Bool ->
    (ToolName -> Value -> IO (Either Text ToolOutcome)) ->
    Text ->
    IO ToolOutcome
runDiscoverTool capSearch call q =
    runDiscoverRequest capSearch call (defaultRequest q)

{- | Validate a raw call's arguments against the one request schema and run
it; a malformed argument is a @bad_request@ envelope naming the field (R2.8).
-}
runDiscoverCall ::
    Bool ->
    (ToolName -> Value -> IO (Either Text ToolOutcome)) ->
    Text ->
    Value ->
    IO ToolOutcome
runDiscoverCall capSearch call q args = case parseRequest q args of
    Left reason -> pure (ToolOk (boundEnvelope (badRequest q reason)))
    Right req -> runDiscoverGoal (goalFromArgs args) capSearch call req

{- | Run a validated @discover@ request: build the notebook environment,
consult every source, and union-merge into one envelope under the request's
scope, limit and mode. Sources that fail are reported unavailable — never as
absence. The lexical channel runs on every arm; the lever gates SHIP
semantic enrichment only (search-api.md section 2).
-}
runDiscoverRequest ::
    Bool ->
    (ToolName -> Value -> IO (Either Text ToolOutcome)) ->
    DiscoverRequest ->
    IO ToolOutcome
runDiscoverRequest = runDiscoverGoal Nothing

{- | 'runDiscoverRequest' with the guard-injected standing goal (section
8.3): ledger provenance feeds producer ranking and the argument-producer
attachment; everything else is identical.
-}
runDiscoverGoal ::
    Maybe StandingGoal ->
    Bool ->
    (ToolName -> Value -> IO (Either Text ToolOutcome)) ->
    DiscoverRequest ->
    IO ToolOutcome
runDiscoverGoal mSG capSearch call req0
    | T.null (T.strip q) =
        pure (ToolOk (boundEnvelope (badRequest q blankReason)))
    | otherwise = do
        env <- fetchNotebookEnv call
        let interp0 = interpret env q
            interp = asConstruct req interp0
        -- Section 3.3: ONE stage-0 exact resolution runs before any mode
        -- branch — modes select a rendering, never which index is consulted.
        exact0 <- stageZero call env interp0
        let envA = envAnswer env interp0
        if isConstruct req interp
            then do
                cAnswers <- constructAnswers call interp
                let answers = cAnswers ++ envA : exact0
                hk <- hackageInfoFor (candidatePackages interp answers)
                let v = constructEnvelope mSG env interp (drScope req) (drLimit req) answers hk
                vOut <-
                    establishedFallback mSG call req $
                        modeRedirect req env interp0 answers hk v
                pure (ToolOk (boundEnvelope vOut))
            else do
                sess <- fetchSession call interp
                cap <- fetchOk call SearchCapability (capabilityArgs capSearch interp)
                let base =
                        [ envA
                        , sessionAnswer interp sess
                        , capabilityAnswer interp cap
                        ]
                            ++ exact0
                probed <- probeHidden call interp base
                attached <-
                    if drMode req == ModeSearch
                        then attachProducers mSG call env interp (base ++ probed)
                        else pure []
                let answers = base ++ probed ++ attached
                hk <- hackageInfoFor (candidatePackages interp answers)
                v <- answerFor req env interp answers hk
                vOut <-
                    establishedFallback mSG call req $
                        modeRedirect req env interp0 answers hk v
                pure (ToolOk (boundEnvelope vOut))
  where
    -- R2.6: a blank query in inventory mode falls back to the scope's own
    -- name, so {mode:"inventory", module:"M"} answers the scoped module card.
    q = case (T.strip (drQuery req0), drMode req0) of
        ("", ModeInventory) ->
            fromMaybe "" (scopeFallbackQuery (drScope req0))
        (q0, _) -> q0
    req = req0{drQuery = q}
    blankReason =
        "query must be a non-blank string (or set module/package with \
        \mode=\"inventory\" to list a scope's card)"

-- | The facet fires on the construct diagnostic class: shape or explicit mode.
isConstruct :: DiscoverRequest -> Interpreted -> Bool
isConstruct req interp = drMode req == ModeConstruct || iShape interp == "construct"

{- | Section 3.3 denial legality across modes: a mode-specific envelope may
never answer not_found for a name the stage-0-fed union resolves — the same
union's SEARCH rendering answers instead, carrying a redirect note. A miss
the union cannot resolve passes through untouched (honest in every mode).
-}
modeRedirect ::
    DiscoverRequest ->
    NotebookEnv ->
    Interpreted ->
    [SourceAnswer] ->
    HackageInfo ->
    Value ->
    Value
modeRedirect req env interp0 answers hk v
    | stateText v /= "not_found" = v
    | stateText searchV /= "found" = v
    | otherwise = setField "next" redirectNote searchV
  where
    searchV =
        discoverEnvelopeScoped env interp0 (drScope req) (drLimit req) answers hk
    redirectNote =
        "'"
            <> iName interp0
            <> "' resolves; mode="
            <> modeName
            <> " had no mode-shaped answer for it, so this is its search \
               \rendering (modes change the rendering, never the index)."
    modeName = case drMode req of
        ModeInventory -> "inventory"
        ModeConstruct -> "construct"
        ModeSearch
            | iShape interp0 == "construct" -> "construct"
            | otherwise -> "search"

stateText :: Value -> Text
stateText (Object o) = case KM.lookup "state" o of
    Just (String s) -> s
    _ -> ""
stateText _ = ""

-- | Under @mode=construct@ the bare query IS the goal type: force the shape.
asConstruct :: DiscoverRequest -> Interpreted -> Interpreted
asConstruct req interp
    | drMode req == ModeConstruct = interp{iShape = "construct"}
    | otherwise = interp

-- | The mode's envelope: ranked search hits, or the M6 inventory card.
answerFor ::
    DiscoverRequest ->
    NotebookEnv ->
    Interpreted ->
    [SourceAnswer] ->
    HackageInfo ->
    IO Value
answerFor req env interp answers hk = case drMode req of
    ModeSearch ->
        pure
            (discoverEnvelopeScoped env interp (drScope req) (drLimit req) answers hk)
    ModeInventory -> do
        lexical <- hackageMatching lexicalCap (topicTokens interp)
        pure
            ( inventoryEnvelope
                env
                interp
                (drScope req)
                (drLimit req)
                answers
                hk
                lexical
            )

-- | Upstream lexical candidates an inventory answer may add, at most.
lexicalCap :: Int
lexicalCap = 25
