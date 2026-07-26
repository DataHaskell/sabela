{-# LANGUAGE OverloadedStrings #-}

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
    notebookAnswer,
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
    fetchSessionScoped,
    probeHidden,
    queryVariants,
 )
import Siza.Agent.Discover.Goal (goalFromArgs, recentFromArgs)
import Siza.Agent.Discover.Hackage (hackageInfoFor, hackageMatching)
import Siza.Agent.Discover.Interpret (interpret)
import Siza.Agent.Discover.Inventory (inventoryEnvelope, topicTokens)
import Siza.Agent.Discover.Merge (
    discoverEnvelopeRecent,
    discoverEnvelopeScoped,
 )
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
    Scope (..),
    SourceAnswer,
    StandingGoal,
 )

discoverPlan :: Bool -> Text -> [ToolName]
discoverPlan _ q = plan
  where
    plan
        | "->" `T.isInfixOf` q || "::" `T.isInfixOf` q =
            [FindByType, FindFunction, SearchCapability]
        | length (T.words (T.strip q)) <= 1 = [FindFunction, SearchCapability]
        | otherwise = [SearchCapability, FindFunction]

discoverArgs :: ToolName -> Text -> Value
discoverArgs tn q =
    object [K.fromText (fromMaybe "query" (primaryArgKey tn)) .= q]

packageShaped :: Text -> Bool
packageShaped q = case T.uncons t of
    Just (c, _) ->
        isLower c && T.all (\ch -> isLower ch || isDigit ch || ch == '-') t
    Nothing -> False
  where
    t = T.strip q

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

runDiscoverTool ::
    Bool ->
    (ToolName -> Value -> IO (Either Text ToolOutcome)) ->
    Text ->
    IO ToolOutcome
runDiscoverTool capSearch call q =
    runDiscoverRequest capSearch call (defaultRequest q)

runDiscoverCall ::
    Bool ->
    (ToolName -> Value -> IO (Either Text ToolOutcome)) ->
    Text ->
    Value ->
    IO ToolOutcome
runDiscoverCall capSearch call q args = case parseRequest q args of
    Left reason -> pure (ToolOk (boundEnvelope (badRequest q reason)))
    Right req -> runDiscoverGoal (goalFromArgs args) (recentFromArgs args) capSearch call req

runDiscoverRequest ::
    Bool ->
    (ToolName -> Value -> IO (Either Text ToolOutcome)) ->
    DiscoverRequest ->
    IO ToolOutcome
runDiscoverRequest = runDiscoverGoal Nothing []

runDiscoverGoal ::
    Maybe StandingGoal ->
    [Text] ->
    Bool ->
    (ToolName -> Value -> IO (Either Text ToolOutcome)) ->
    DiscoverRequest ->
    IO ToolOutcome
runDiscoverGoal mSG recent capSearch call req0
    | T.null (T.strip q) =
        pure (ToolOk (boundEnvelope (badRequest q blankReason)))
    | otherwise = do
        env <- fetchNotebookEnv call
        let interp0 = interpret env q
            interp = asConstruct req interp0
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
                sess <- fetchSessionScoped call (scModule (drScope req)) interp
                cap <- fetchOk call SearchCapability (capabilityArgs capSearch interp)
                nb <-
                    fetchOk call FindCellsByContent (object ["pattern" .= iName interp])
                let base =
                        [ envA
                        , sessionAnswer interp sess
                        , capabilityAnswer interp cap
                        , notebookAnswer interp nb
                        ]
                            ++ exact0
                probed <- probeHidden call interp base
                attached <-
                    if drMode req == ModeSearch
                        then attachProducers mSG call env interp (base ++ probed)
                        else pure []
                let answers = base ++ probed ++ attached
                hk <- hackageInfoFor (candidatePackages interp answers)
                v <- answerFor recent req env interp answers hk
                vOut <-
                    establishedFallback mSG call req $
                        modeRedirect req env interp0 answers hk v
                pure (ToolOk (boundEnvelope vOut))
  where
    q = case (T.strip (drQuery req0), drMode req0) of
        ("", ModeInventory) ->
            fromMaybe "" (scopeFallbackQuery (drScope req0))
        (q0, _) -> q0
    req = req0{drQuery = q}
    blankReason =
        "query must be a non-blank string (or set module/package with \
        \mode=\"inventory\" to list a scope's card)"

isConstruct :: DiscoverRequest -> Interpreted -> Bool
isConstruct req interp = drMode req == ModeConstruct || iShape interp == "construct"

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

asConstruct :: DiscoverRequest -> Interpreted -> Interpreted
asConstruct req interp
    | drMode req == ModeConstruct = interp{iShape = "construct"}
    | otherwise = interp

answerFor ::
    [Text] ->
    DiscoverRequest ->
    NotebookEnv ->
    Interpreted ->
    [SourceAnswer] ->
    HackageInfo ->
    IO Value
answerFor recent req env interp answers hk = case drMode req of
    ModeSearch ->
        pure
            ( discoverEnvelopeRecent
                recent
                env
                interp
                (drScope req)
                (drLimit req)
                answers
                hk
            )
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

lexicalCap :: Int
lexicalCap = 25
