{-# LANGUAGE OverloadedStrings #-}

{- | C1/C2: an unsatisfied goal is the strongest lead the ladder holds, and a
repeated lexical miss is evidence the name is the wrong axis. Everything here
is stated over generated goal types, packages and identifiers.
-}
module Test.DiscoverEscalateSpec (discoverEscalateSpec) where

import Data.Aeson (Value (..), object, (.=))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Sabela.AI.FitRule (Freeness (..), classifyFit)
import Sabela.AI.HoleFits (HoleFit (..))
import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.Discover.Envelope (envelopeViolations)
import Siza.Agent.Discover.GoalEscalate (
    TypeAnswer (..),
    escalationNext,
    escalationQueries,
    producersOfType,
 )
import Siza.Agent.Discover.History (
    SearchLedger,
    emptyLedger,
    ledgerRecord,
    slGoalSpent,
 )
import Siza.Agent.Discover.HistoryGuard (guardDiscover, newSearchLedger)
import Siza.Agent.Discover.MissLadder (MissOutcome (..), missAdvice)
import Siza.Agent.Discover.Types (StandingGoal (..))
import Test.DiscoverFixtures (textField)
import Test.DiscoverGen (genGoalFreeRow, genPkgName)

discoverEscalateSpec :: Spec
discoverEscalateSpec = describe "goal escalation (C1, C2)" $ do
    filterLawSpec
    rungDecisionSpec
    spendOnceSpec
    disclosureSpec

-- ---------------------------------------------------------------- fixtures

consumerName :: Text
consumerName = "consumeIt"

moduleName :: Text
moduleName = "Some.Module"

foundConsumer :: Text -> Text -> Value
foundConsumer goal pkg =
    object
        [ "query" .= consumerName
        , "state" .= ("found" :: Text)
        , "hits"
            .= [ object
                    [ "name" .= consumerName
                    , "type" .= (goal <> " -> Text")
                    , "module" .= moduleName
                    , "package" .= pkg
                    , "version" .= ("1.0.0" :: Text)
                    , "install" .= ("installed" :: Text)
                    , "matchKind" .= ("exact" :: Text)
                    , "origin" .= ("hoogle" :: Text)
                    , "cabal" .= ("-- cabal: build-depends: " <> pkg)
                    ]
               ]
        , "shown" .= (1 :: Int)
        , "omitted" .= (0 :: Int)
        , "total" .= (1 :: Int)
        , "consulted" .= ([] :: [Value])
        , "next" .= ("act on the hit" :: Text)
        ]

missFor :: Text -> Value
missFor q =
    object
        [ "query" .= q
        , "state" .= ("not_found" :: Text)
        , "next" .= ("No match for '" <> q <> "'." :: Text)
        , "total" .= (0 :: Int)
        ]

capabilityOf :: Text -> [(Text, Text)] -> Value
capabilityOf pkg rows = object ["hits" .= [object ["package" .= pkg, "api" .= api]]]
  where
    api =
        [object ["name" .= n, "module" .= moduleName, "type" .= ty] | (n, ty) <- rows]

spellings :: [Text]
spellings = ["alphaTry", "betaTry", "gammaTry", "deltaTry"]

-- ---------------------------------------------------------- the two filters

{- | The inhabitant of every goal that no signature can rule out. Named by
'Sabela.AI.FitRule', the oracle both producer paths are judged against.
-}
bottomName :: Text
bottomName = "undefined"

rowFit :: (Text, Text) -> HoleFit
rowFit (n, ty) = HoleFit n ty False Nothing []

keptNames :: Text -> [(Text, Text)] -> [Text]
keptNames goal rows =
    [n | (n, _, _) <- producersOfType goal (capabilityOf "any-pkg" rows)]

filterLawSpec :: Spec
filterLawSpec = describe "only a backed producer of the goal is named" $ do
    it "keeps every row of the answer that produces the goal" $
        property $
            forAll genGoalFreeRow $ \(goal, producers, _) ->
                let kept = keptNames goal producers
                 in counterexample (show kept) $
                        conjoin [n `elem` kept | (n, _) <- producers]
    it "names no row that produces something other than the goal" $
        property $
            forAll genGoalFreeRow $ \(goal, _, junk) ->
                keptNames goal junk === []
    it "names only rows the compiler-backed fit law calls informative" $
        property $
            forAll genGoalFreeRow $ \(goal, producers, junk) ->
                let rows = (bottomName, goal) : producers ++ junk
                    kept = producersOfType goal (capabilityOf "any-pkg" rows)
                 in conjoin
                        [ counterexample (T.unpack (n <> " :: " <> ty)) $
                            classifyFit (rowFit (n, ty)) === Informative
                        | (n, ty, _) <- kept
                        ]

-- ------------------------------------------------------ the rung decision

seedGoal :: Text -> Text -> SearchLedger
seedGoal goal pkg =
    fst (ledgerRecord consumerName (foundConsumer goal pkg) emptyLedger)

walkMisses :: SearchLedger -> [Text] -> (SearchLedger, [Value])
walkMisses led = foldl' step (led, [])
  where
    step (l, outs) q =
        let (l2, o) = ledgerRecord q (missFor q) l in (l2, outs ++ [o])

rungDecisionSpec :: Spec
rungDecisionSpec = describe "only a middle rung under an unsatisfied goal escalates" $ do
    it "rung 1 advises, rungs 2 and 3 escalate, rung 4 closes as before" $
        property $
            forAll ((,) <$> genGoalFreeRow <*> genPkgName) $ \((goal, _, _), pkg) ->
                let sg = Just (StandingGoal goal consumerName pkg)
                    at n = missAdvice [] Set.empty [] Nothing [] sg n "q" (missFor "q")
                 in conjoin
                        [ isEscalate (at 2) === True
                        , isEscalate (at 3) === True
                        , isEscalate (at 1) === False
                        , isEscalate (at 4) === False
                        ]
    it "no goal means no escalation on any rung" $
        property $
            forAll (elements [1 .. 6 :: Int]) $ \n ->
                isEscalate
                    (missAdvice [] Set.empty [] Nothing [] Nothing n "q" (missFor "q"))
                    === False
    it "the escalating rung's own payload never claims a query it did not run" $
        property $
            forAll ((,) <$> genGoalFreeRow <*> genPkgName) $ \((goal, _, _), pkg) ->
                let (_, outs) = walkMisses (seedGoal goal pkg) (take 3 spellings)
                    said = T.concat (map (textField "next") outs)
                 in counterexample (T.unpack said) $
                        not ("type query" `T.isInfixOf` said)

isEscalate :: MissOutcome -> Bool
isEscalate (EscalateType _ _) = True
isEscalate _ = False

-- -------------------------------------------------------- one spend, once

spendOnceSpec :: Spec
spendOnceSpec = describe "one type query per goal cluster" $ do
    it "the ladder records the spend the first time it arms" $
        property $
            forAll ((,) <$> genGoalFreeRow <*> genPkgName) $ \((goal, _, _), pkg) ->
                let (led, _) = walkMisses (seedGoal goal pkg) spellings
                 in Set.size (slGoalSpent led) === 1
    it "queries are package-scoped, held package first, capped at two" $
        property $
            forAll ((,,) <$> genGoalFreeRow <*> genPkgName <*> listOf genPkgName) $
                \((goal, _, _), pkg, held) ->
                    let qs = escalationQueries (StandingGoal goal consumerName pkg) held
                     in counterexample (show qs) $
                            length qs
                                <= 2
                                .&&. take 1 qs
                                    === ["+" <> pkg <> " :: " <> goal]
    it "falls back to the bare form only when no fact names a package" $
        property $
            forAll genGoalFreeRow $ \(goal, _, _) ->
                escalationQueries (StandingGoal goal consumerName "") []
                    === [":: " <> goal]

-- ------------------------------------------------- what the guard discloses

type Script = IORef [ToolCall]

scriptedDispatch ::
    Script ->
    Text ->
    Text ->
    Either Text ToolOutcome ->
    ToolCall ->
    IO (Either Text ToolOutcome)
scriptedDispatch seen goal pkg capOut tc = do
    modifyIORef' seen (++ [tc])
    pure $ case tcName tc of
        "search_capability" -> capOut
        _
            | queryOf tc == consumerName -> Right (ToolOk (foundConsumer goal pkg))
            | otherwise -> Right (ToolOk (missFor (queryOf tc)))

-- | The three shapes a type query comes back in: an answer, and two failures.
capAnswer :: Text -> [(Text, Text)] -> Either Text ToolOutcome
capAnswer pkg rows = Right (ToolOk (capabilityOf pkg rows))

capRefused :: Text -> Either Text ToolOutcome
capRefused why = Right (ToolErr (object ["error" .= why]))

capUnreachable :: Text -> Either Text ToolOutcome
capUnreachable = Left

queryOf :: ToolCall -> Text
queryOf (ToolCall _ args) = textField "query" args

runGuard :: Text -> Text -> Either Text ToolOutcome -> IO ([Value], [ToolCall])
runGuard goal pkg capOut = do
    seen <- newIORef []
    ref <- newSearchLedger
    let inner = scriptedDispatch seen goal pkg capOut
        one q = guardDiscover ref inner (ToolCall "discover" (object ["query" .= q]))
    outs <- mapM one (consumerName : take 3 spellings)
    calls <- readIORef seen
    pure ([v | Right (ToolOk v) <- outs], calls)

typeCalls :: [ToolCall] -> [Text]
typeCalls calls = [queryOf c | c <- calls, tcName c == "search_capability"]

nextOf :: [Value] -> Text
nextOf = T.concat . map (textField "next")

disclosureSpec :: Spec
disclosureSpec = describe "the escalation discloses what it spent and what came back" $ do
    it "spends exactly one type query and names it, with the producer it found" $ do
        (outs, calls) <-
            runGuard
                "Widget"
                "acme-kit"
                (capAnswer "acme-kit" [("makeWidget", "Int -> Widget")])
        typeCalls calls `shouldBe` ["+acme-kit :: Widget"]
        let said = nextOf outs
        said `shouldSatisfy` T.isInfixOf "the name is the wrong axis"
        said `shouldSatisfy` T.isInfixOf "+acme-kit :: Widget"
        said `shouldSatisfy` T.isInfixOf "makeWidget :: Int -> Widget"
        concatMap envelopeViolations outs `shouldBe` []
    it "says so, and names nothing, when the type query answers with junk" $ do
        (outs, calls) <-
            runGuard
                "Widget"
                "acme-kit"
                (capAnswer "acme-kit" [("nothingUseful", "a -> b"), ("bottomOut", "a")])
        length (typeCalls calls) `shouldBe` 1
        let said = nextOf outs
        said `shouldSatisfy` T.isInfixOf "no informative producer of Widget"
        said `shouldSatisfy` (not . T.isInfixOf "nothingUseful")
        said `shouldSatisfy` (not . T.isInfixOf "bottomOut")
    it "reports a type query that never ran as unrun, and claims no answer" $
        mapM_
            failureDisclosed
            [capRefused "index unavailable", capUnreachable "connection refused"]
    it "does not spend the goal's one query on a type query that never ran" $ do
        (_, calls) <- runGuard "Widget" "acme-kit" (capRefused "index unavailable")
        length (typeCalls calls) `shouldBe` 2
    it "never claims a query when there is nothing to spend one on" $
        escalationNext "q" "Widget" [] NoneFound `shouldBe` ""

{- | A failed probe is not a result: the disclosure names the failure and never
states what producers the index holds.
-}
failureDisclosed :: Either Text ToolOutcome -> Expectation
failureDisclosed capOut = do
    (outs, _) <- runGuard "Widget" "acme-kit" capOut
    let said = nextOf outs
    said `shouldSatisfy` T.isInfixOf "+acme-kit :: Widget"
    said `shouldSatisfy` T.isInfixOf "did not run"
    said `shouldSatisfy` (not . T.isInfixOf "no informative producer")
