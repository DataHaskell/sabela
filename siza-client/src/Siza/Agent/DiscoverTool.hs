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

import Control.Monad (foldM)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import Data.Char (isDigit, isLower)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.Capabilities.ToolName (
    ToolName (..),
    primaryArgKey,
 )
import Sabela.AI.Types (ToolOutcome (..))
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
import Siza.Agent.Discover.Hackage (
    hackageInfoFor,
    withModuleOwners,
 )
import Siza.Agent.Discover.Interpret (interpret)
import Siza.Agent.Discover.Mode (
    answerFor,
    asConstruct,
    isConstruct,
    modeRedirect,
 )
import Siza.Agent.Discover.ProducerCard (
    establishedFallback,
    withProducerHint,
 )
import Siza.Agent.Discover.Request (
    DiscoverMode (..),
    DiscoverRequest (..),
    defaultRequest,
    discoverKey,
    discoverQuery,
    effectiveQuery,
    parseRequest,
 )
import Siza.Agent.Discover.Types (
    HackageInfo,
    Interpreted (..),
    Scope (..),
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
        <> " A miss lists what was consulted. Notebook cells are searched \
           \too: a hit a cell defines says so. Ask in your own words if you \
           \do not know the name ({query: \"plot a sine wave\"}). The \
           \notebook's own library is not on Hackage and is in no file you \
           \can grep - this is the only index of it."

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
                hk <- hackageInfoFor (candidatePackages interp answers ++ scopePkg req)
                let v = constructEnvelope mSG env interp (drScope req) (drLimit req) answers hk
                vOut <-
                    establishedFallback mSG call req $
                        modeRedirect req env interp0 answers hk v
                pure (ToolOk (boundEnvelope (withProducerHint vOut)))
            else do
                sess <- fetchSessionScoped call (scModule (drScope req)) interp
                cap <-
                    fetchOk
                        call
                        SearchCapability
                        (capabilityArgs capSearch (drScope req) interp)
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
                hk0 <- hackageInfoFor (candidatePackages interp answers ++ scopePkg req)
                hk <- moduleOwnerFacts interp (drScope req) hk0
                v <- answerFor recent req env interp answers hk
                vOut <-
                    establishedFallback mSG call req $
                        modeRedirect req env interp0 answers hk v
                pure (ToolOk (boundEnvelope (withProducerHint vOut)))
  where
    q = effectiveQuery req0
    req = req0{drQuery = q}
    blankReason =
        "query must be a non-blank string, or name a module or package to ask \
        \for that scope's card"

-- | The package a request scoped itself to, which is a candidate like any other.
scopePkg :: DiscoverRequest -> [Text]
scopePkg req = maybeToList (scPackage (drScope req))

{- | A module no installed package exposes is not thereby absent: the index
states which package exposes it. A scoped module raises that same question as a
queried one, so both ask it, or a scoped search denies a describable package.
-}
moduleOwnerFacts :: Interpreted -> Scope -> HackageInfo -> IO HackageInfo
moduleOwnerFacts interp scope hk0 = foldM (flip withModuleOwners) hk0 asked
  where
    asked =
        [iName interp | iShape interp == "module"]
            ++ [m | Just m <- [scModule scope], m /= iName interp]
