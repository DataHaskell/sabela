{-# LANGUAGE OverloadedStrings #-}

{- | R8-T2 ranking and attachment (search-api.md 7.1, 8.3): goal-provenance
producer ranking (the held consumer's package outranks lexical winners,
non-producers rank below every true producer) and the same-envelope producer
attachment. Keyed on ledger evidence, never a library name.
-}
module Test.DiscoverGoalRankSpec (discoverGoalRankSpec) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), object, (.=))
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.Discover.Construct (constructEnvelope)
import Siza.Agent.Discover.Envelope (
    envelopeCharBudget,
    envelopeChars,
    envelopeViolations,
 )
import Siza.Agent.Discover.HistoryGuard (guardDiscover, newSearchLedger)
import Siza.Agent.Discover.Types (
    DHit (..),
    HackageInfo (..),
    Interpreted (..),
    MatchKind (..),
    NotebookEnv (..),
    StandingGoal (..),
    emptyScope,
    mkHit,
    okAnswer,
    seededBuiltins,
 )
import Siza.Agent.DiscoverTool (runDiscoverCall)
import Test.CatalogueSim (SimWorld (..), runWorldArgs, simWorldCall)
import Test.DiscoverFixtures (
    SynPkg (..),
    argText,
    field,
    hitText,
    hitsOf,
    installNamesFileWith,
    stateOf,
    textField,
 )

env0 :: NotebookEnv
env0 = seededBuiltins (NotebookEnv [] [] [] [] [] [])

interpFor :: Text -> Interpreted
interpFor t = Interpreted t t Nothing "construct" "" []

dh :: Text -> Text -> Text -> Text -> DHit
dh n ty m p =
    (mkHit n m p){dhType = ty, dhOrigin = "session"}

intField :: Text -> Value -> Int
intField k v = case field k v of
    Just (Number n) -> round n
    _ -> (-1)

names :: Value -> [Text]
names = map (hitText "name") . hitsOf

discoverGoalRankSpec :: Spec
discoverGoalRankSpec = describe "goal-provenance ranking + producer attachment (R8-T2)" $ do
    provenanceRankSpec
    crossPackageDemoteSpec
    provenanceLoopSpec
    attachmentSpec

-- Cross-package non-producer demotion (R5.6) ---------------------------------

{- | Two non-producing hits at the same stratum, one from the target package,
one foreign; the plain key orders the foreign one first (shorter package/name).
Once a target is established, the target hit must lead the non-producer tail.
-}
crossPackageDemoteSpec :: Spec
crossPackageDemoteSpec =
    describe "cross-package non-producers demote below the target package (R5.6)" $ do
        let mk n m p = (dh n "Text" m p){dhKind = MkSubstring}
            answers p =
                [okAnswer "session" [mk "aHelper" "A.A" "foreignx", mk "zHelper" "Z.Z" p]]
            rankedWith mSG p =
                names $
                    constructEnvelope
                        mSG
                        env0
                        (interpFor "Plot")
                        emptyScope
                        8
                        (answers p)
                        (HackageInfo True [])
        it "the target package's non-producer leads the tail once a goal is held" $
            rankBefore
                (rankedWith (Just (StandingGoal "Plot" "bars" "targetpkg")) "targetpkg")
                "zHelper"
                "aHelper"
        it "without a target the lexical winner leads the tail (control)" $
            rankBefore (rankedWith Nothing "targetpkg") "aHelper" "zHelper"
        it "library-name substitution leaves the demotion byte-identical" $ do
            let decide p = rankedWith (Just (StandingGoal "Plot" "bars" p)) p
            length (nub [decide "targetpkg", decide "granite", decide "z-kit"])
                `shouldBe` 1

-- Goal-provenance producer ranking (R3.2/R3.5) -------------------------------

{- | The planted catalogue: the true consumer-package producer sits in the
lexically LOSING module (Zzz.Deep), a foreign nullary producer wins lexically
(Aaa.A), and a foreign prefix imposter produces nothing.
-}
rankedNames :: Maybe StandingGoal -> (Text, Text) -> [Text]
rankedNames mSG (cPkg, fPkg) = names v
  where
    answers =
        [ okAnswer
            "session"
            [ (dh "defaultPlotLineStyle" "LineStyle" "Aaa.A" fPkg){dhKind = MkPrefix}
            , dh "aaaPlot" "Plot" "Aaa.A" fPkg
            , dh "zzzPlot" "Plot" "Zzz.Deep" cPkg
            , dh "mkThing" "Int -> Plot" "Zzz.Deep" cPkg
            ]
        ]
    v =
        constructEnvelope
            mSG
            env0
            (interpFor "Plot")
            emptyScope
            8
            answers
            (HackageInfo True [])

rankBefore :: [Text] -> Text -> Text -> Expectation
rankBefore ns a b = do
    ns `shouldSatisfy` elem a
    ns `shouldSatisfy` elem b
    let pos x = length (takeWhile (/= x) ns)
    pos a `shouldSatisfy` (< pos b)

provenanceRankSpec :: Spec
provenanceRankSpec = describe "the held consumer's package outranks lexical winners" $ do
    let sg p = Just (StandingGoal "Plot" "bars" p)
    it "goal provenance beats the lexically winning foreign module" $ do
        let ns = rankedNames (sg "plume") ("plume", "chartx")
        rankBefore ns "zzzPlot" "aaaPlot"
        -- Ahead of arity: the consumer package's unary producer, too.
        rankBefore ns "mkThing" "aaaPlot"
    it "without ledger provenance the foreign module wins lexically (control)" $ do
        let ns = rankedNames Nothing ("plume", "chartx")
        rankBefore ns "aaaPlot" "zzzPlot"
    it "every non-producing prefix hit ranks below every true producer" $
        forM_ [Just (StandingGoal "Plot" "bars" "plume"), Nothing] $ \mSG -> do
            let ns = rankedNames mSG ("plume", "chartx")
            forM_ ["zzzPlot", "aaaPlot", "mkThing"] $ \p ->
                rankBefore ns p "defaultPlotLineStyle"
    it "library-name substitution leaves the decision byte-identical" $ do
        let spellings = [("plume", "chartx"), ("granite", "chart"), ("aaa-kit", "zzz-kit")]
            decisions = [rankedNames (sg c) (c, f) | (c, f) <- spellings]
        length (nub decisions) `shouldBe` 1

-- The ledger-provenance loop (guard -> args -> rank) -------------------------

{- | Two installed packages: the consumer package holds @bars@ and a nullary
producer in a lexically losing module; the foreign package holds a lexically
winning nullary producer and the prefix imposter.
-}
provWorld :: SimWorld
provWorld = SimWorld pkgs pkgs
  where
    pkgs =
        [ SynPkg
            "plume"
            "1.0.0"
            False
            [
                ( "Zzz.Deep"
                ,
                    [ ("bars", "[(Text, Double)] -> Plot -> Text")
                    , ("zzzPlot", "Plot")
                    ]
                )
            ]
        , SynPkg
            "chartx"
            "2.0.0"
            False
            [("Aaa.A", [("aaaPlot", "Plot"), ("defaultPlotLineStyle", "LineStyle")])]
        ]

runGuard :: SimWorld -> [Value] -> IO [Value]
runGuard w argsList = do
    ref <- newSearchLedger
    let inner tc = case tcName tc of
            "discover" ->
                Right
                    <$> runDiscoverCall
                        True
                        (simWorldCall w)
                        (argText "query" (tcArgs tc))
                        (tcArgs tc)
            _ -> pure (Right (ToolOk (object [])))
        outOf r = case r of
            Right (ToolOk v) -> v
            Right (ToolErr v) -> v
            Left _ -> object []
    mapM (fmap outOf . guardDiscover ref inner . ToolCall "discover") argsList

constructPlot :: Value
constructPlot =
    object ["query" .= ("Plot" :: Text), "mode" .= ("construct" :: Text)]

provenanceLoopSpec :: Spec
provenanceLoopSpec = describe "provenance flows from the ledger's held consumer fact" $ do
    it "after the consumer fact lands, its package's producer ranks first" $ do
        installNamesFileWith ["plume", "chartx"]
        outs <- runGuard provWorld [object ["query" .= ("bars" :: Text)], constructPlot]
        stateOf (outs !! 1) `shouldBe` "found"
        take 1 (names (outs !! 1)) `shouldBe` ["zzzPlot"]
        envelopeViolations (outs !! 1) `shouldBe` []
    it "without the held consumer fact the lexical winner leads (control)" $ do
        installNamesFileWith ["plume", "chartx"]
        outs <- runGuard provWorld [constructPlot]
        map (take 1 . names) outs `shouldBe` [["aaaPlot"]]
    it "a junk wall mid-hunt is judged and steered to the facet by rung 2" $ do
        installNamesFileWith ["plume", "chartx"]
        outs <-
            runGuard
                provWorld
                [ object ["query" .= ("bars" :: Text)]
                , object ["query" .= ("default" :: Text)]
                , object ["query" .= ("defaultP" :: Text)]
                ]
        let sat o = field "goal" o >>= field "satisfied"
        sat (outs !! 1) `shouldBe` Just (Bool False)
        sat (outs !! 2) `shouldBe` Just (Bool False)
        textField "next" (outs !! 2) `shouldSatisfy` T.isInfixOf "mode=\"construct\""

-- Same-envelope producer attachment (R3.3/R3.9) ------------------------------

attachmentSpec :: Spec
attachmentSpec = describe "an unconstructible argument's producers ride the SAME envelope" $ do
    it "the consumer's envelope carries the top 1-2 producers, nullary-first" $ do
        installNamesFileWith ["plume", "framing"]
        v <- runWorldArgs attachWorld "bars" (object [])
        stateOf v `shouldBe` "found"
        take 1 (names v) `shouldBe` ["bars"]
        let uses = [hitText "use" h | h <- hitsOf v, hitText "name" h == "zzzPlot"]
        uses `shouldSatisfy` any (T.isInfixOf "produces Plot")
        -- Producer-distance selection: nullary and unary in, binary out.
        let attached = [hitText "name" h | h <- hitsOf v, T.isInfixOf "produces " (hitText "use" h)]
        length attached `shouldSatisfy` (<= 2)
        attached `shouldSatisfy` elem "zzzPlot"
        attached `shouldSatisfy` elem "mkPlot"
        attached `shouldSatisfy` notElem "farPlot"
    it "stays inside the one bounded envelope at every limit 1..8" $ do
        installNamesFileWith ["plume", "framing"]
        violations <- concat <$> mapM violationsAt [1 .. 8]
        violations `shouldBe` []
    it "conserves counts: shown + omitted = total, attachment included" $ do
        installNamesFileWith ["plume", "framing"]
        forM_ [1, 2, 8] $ \n -> do
            v <- runWorldArgs attachWorld "bars" (object ["limit" .= (n :: Int)])
            intField "shown" v + intField "omitted" v `shouldBe` intField "total" v
            intField "shown" v `shouldSatisfy` (<= n)
    it "a literal-argument consumer attaches nothing" $ do
        installNamesFileWith ["plume", "chartx"]
        v <- runWorldArgs literalWorld "gust" (object [])
        [h | h <- hitsOf v, T.isInfixOf "produces " (hitText "use" h)] `shouldBe` []
    it "no producer anywhere: the envelope is unchanged and still legal" $ do
        installNamesFileWith ["plume"]
        v <- runWorldArgs bareWorld "drizzle" (object [])
        stateOf v `shouldBe` "found"
        [h | h <- hitsOf v, T.isInfixOf "produces " (hitText "use" h)] `shouldBe` []
        envelopeViolations v `shouldBe` []
  where
    violationsAt :: Int -> IO [Text]
    violationsAt n = do
        v <- runWorldArgs attachWorld "bars" (object ["limit" .= n])
        pure
            ( envelopeViolations v
                ++ [ "over budget at limit " <> T.pack (show n)
                   | envelopeChars v > envelopeCharBudget
                   ]
            )

{- | The producers of the consumer's argument type live in a package the
consumer's own query never surfaces — the no-surfaced-producer class.
-}
attachWorld :: SimWorld
attachWorld = SimWorld pkgs pkgs
  where
    pkgs =
        [ SynPkg
            "plume"
            "1.0.0"
            False
            [("Zzz.Deep", [("bars", "[(Text, Double)] -> Plot -> Text")])]
        , SynPkg
            "framing"
            "2.0.0"
            False
            [
                ( "Fr.M"
                ,
                    [ ("zzzPlot", "Plot")
                    , ("mkPlot", "Text -> Plot")
                    , ("farPlot", "Int -> Int -> Plot")
                    ]
                )
            ]
        ]

-- | A consumer whose only argument is literal-constructible: never a goal.
literalWorld :: SimWorld
literalWorld = SimWorld pkgs pkgs
  where
    pkgs =
        [ SynPkg
            "plume"
            "1.0.0"
            False
            [("Zzz.Deep", [("gust", "Int -> Wind"), ("zzzPlot", "Plot")])]
        ]

-- | A consumer whose argument type has no producer anywhere in the universe.
bareWorld :: SimWorld
bareWorld = SimWorld pkgs pkgs
  where
    pkgs =
        [SynPkg "plume" "1.0.0" False [("Nimbus.Sky", [("drizzle", "Sky -> Rain")])]]
