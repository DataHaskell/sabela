{-# LANGUAGE OverloadedStrings #-}

{- | R8-T2 (search-api.md section 8.3): goal satisfaction is the verdict. The
standing goal derives from held consumer evidence, EVERY query spelling is
judged against it, and a found answer whose hits all fail the goal advances
the SAME miss-cluster ladder state as a not_found for the same cluster.
-}
module Test.DiscoverGoalSpec (discoverGoalSpec) where

import Control.Monad (forM_, when)
import Data.Aeson (Value (..), object, (.=))
import Data.List (foldl')
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Siza.Agent.Discover.Envelope (badRequest, envelopeViolations)
import Siza.Agent.Discover.Goal (standingGoal)
import Siza.Agent.Discover.History (
    SearchLedger,
    emptyLedger,
    heldFacts,
    ladderState,
    ledgerClose,
    ledgerPressure,
    ledgerRecord,
    ledgerShortcut,
 )
import Siza.Agent.Discover.Types (StandingGoal (..))
import Test.DiscoverFixtures (field, stateOf, textField)

-- Envelope builders ----------------------------------------------------------

hitJ :: Text -> Text -> Text -> Text -> Text -> Value
hitJ n ty m p kind =
    object
        [ "name" .= n
        , "type" .= ty
        , "module" .= m
        , "package" .= p
        , "version" .= ("1.0.0" :: Text)
        , "install" .= ("installed" :: Text)
        , "matchKind" .= kind
        , "origin" .= ("session" :: Text)
        ]

foundEnv :: Text -> [Value] -> Value
foundEnv q hits =
    object
        [ "query" .= q
        , "interpreted" .= object ["shape" .= ("name" :: Text)]
        , "state" .= ("found" :: Text)
        , "hits" .= hits
        , "shown" .= length hits
        , "omitted" .= (0 :: Int)
        , "total" .= length hits
        , "consulted" .= ([] :: [Value])
        , "next" .= ("act on the hits" :: Text)
        ]

-- | The held consumer evidence: an exact typed hit the ledger harvests.
consumerFound :: (Text, Text, Text, Text) -> Value
consumerFound (n, sig, m, p) = foundEnv n [hitJ n sig m p "exact"]

-- | The barChart wall: a found answer carrying only foreign prefix junk.
junkFound :: Text -> Value
junkFound q =
    foundEnv
        q
        [hitJ "defaultPlotLineStyle" "LineStyle" "Graphics.Rendering" "Chart" "prefix"]

missEnv :: Text -> Value
missEnv q =
    object
        [ "query" .= q
        , "interpreted" .= object ["shape" .= ("name" :: Text)]
        , "state" .= ("not_found" :: Text)
        , "next" .= ("No match for '" <> q <> "'." :: Text)
        , "total" .= (0 :: Int)
        ]

{- | The zephyr\/stratus\/cumulus consumer grid: (name, signature, module,
package) with the goal the signature derives — an unproduced nominal argument
type, or Nothing when every argument is literal-constructible or self-produced.
-}
consumerGrid :: [((Text, Text, Text, Text), Maybe Text)]
consumerGrid =
    [
        ( ("bars", "[(Text, Double)] -> Plot -> Text", "Cumulus.Plot", "cumulus")
        , Just "Plot"
        )
    , (("drizzle", "Sky -> Rain", "Nimbus.Sky", "nimbus"), Just "Sky")
    , (("gust", "Int -> Wind", "Zephyr.Core", "zephyr"), Nothing)
    , (("lull", "Wind -> Wind", "Stratus.Air", "stratus"), Nothing)
    ]

-- | The judged spellings: producer-prefix, bare word, unrelated token.
spellingsOf :: Text -> [Text]
spellingsOf goal = ["default" <> goal, "default", "unrelatedTok"]

seedConsumer :: (Text, Text, Text, Text) -> SearchLedger
seedConsumer c@(n, _, _, _) = fst (ledgerRecord n (consumerFound c) emptyLedger)

walk :: SearchLedger -> [(Text, Value)] -> (SearchLedger, [Value])
walk led0 = foldl' step (led0, [])
  where
    step (l, outs) (q, vv) = let (l2, o) = ledgerRecord q vv l in (l2, outs ++ [o])

adviceOf :: Value -> Text
adviceOf v = textField "next" v <> " " <> textField "summary" v

goalOf :: Value -> Maybe Value
goalOf = field "goal"

goalType :: Value -> Text
goalType v = maybe "" (textField "type") (goalOf v)

goalSat :: Value -> Maybe Value
goalSat v = goalOf v >>= field "satisfied"

discoverGoalSpec :: Spec
discoverGoalSpec = describe "goal satisfaction is the verdict (R8-T2, section 8.3)" $ do
    derivationSpec
    ladderEqualitySpec
    spellingSpec
    disclosureSpec
    barChartSpec

-- The evidence-derived standing goal ----------------------------------------

derivationSpec :: Spec
derivationSpec = describe "the standing goal derives from held consumer evidence" $ do
    it "derives the unproduced nominal argument type, with provenance" $
        forM_ consumerGrid $ \(c@(_, _, _, pkg), expected) -> do
            let sg = standingGoal (heldFacts (seedConsumer c))
            (c, fmap sgType sg) `shouldBe` (c, expected)
            forM_ sg $ \g -> sgPackage g `shouldBe` pkg
    it "derives no goal from an empty ledger" $
        standingGoal (heldFacts emptyLedger) `shouldBe` Nothing

-- Satisfaction legality: ladder-state equality (R1.1 dual / R5.6) -----------

ladderEqualitySpec :: Spec
ladderEqualitySpec = describe "a goal-miss advances the SAME ladder state as a not_found" $ do
    it "walks rung-for-rung equal over the (consumer x goal x spelling) grid" $
        forM_ [(c, g) | (c, Just g) <- consumerGrid] $ \(c, g) -> do
            let led0 = seedConsumer c
                qs = spellingsOf g
                (ledA, outsA) = walk led0 [(q, junkFound q) | q <- qs]
                (ledB, outsB) = walk led0 [(q, missEnv q) | q <- qs]
            ladderState ledA `shouldBe` ladderState ledB
            -- Rung parity at every step, not just at the end.
            forM_ [1 .. length qs] $ \n -> do
                let (lA, _) = walk led0 [(q, junkFound q) | q <- take n qs]
                    (lB, _) = walk led0 [(q, missEnv q) | q <- take n qs]
                ladderState lA `shouldBe` ladderState lB
            -- Rung markers land on the same calls in both arms.
            forM_ (zip outsA outsB) $ \(a, b) ->
                forM_ ["Already held", "write the deliverable"] $ \marker ->
                    (marker `T.isInfixOf` adviceOf a)
                        `shouldBe` (marker `T.isInfixOf` adviceOf b)
            adviceOf (outsA !! 1) `shouldSatisfy` T.isInfixOf "Already held"
            -- The third same-cluster call after satisfaction hits the R9-T2
            -- k=2 gate: the one-line held-facts write steer, both arms.
            adviceOf (outsA !! 2)
                `shouldSatisfy` T.isInfixOf "write the deliverable"
    it "dedup: a repeat of a goal-missed query short-circuits like a not_found's" $
        forM_ [(c, g) | (c, Just g) <- consumerGrid] $ \(c, g) -> do
            let q = "default" <> g
                (lA, _) = walk (seedConsumer c) [(q, junkFound q)]
                (lB, _) = walk (seedConsumer c) [(q, missEnv q)]
            fmap stateOf (ledgerShortcut lA q) `shouldBe` Just "duplicate"
            isJust (ledgerShortcut lB q) `shouldBe` True
    it "closure: a post-close goal-miss goes to the give-up rung like a not_found" $
        forM_ [(c, g) | (c, Just g) <- consumerGrid] $ \(c, g) -> do
            let led = ledgerClose (seedConsumer c)
                (lA, outA) = ledgerRecord "freshSpell" (junkFound "freshSpell") led
                (lB, outB) = ledgerRecord "freshSpell" (missEnv "freshSpell") led
            ladderState lA `shouldBe` ladderState lB
            adviceOf outA `shouldSatisfy` T.isInfixOf "cannot help"
            adviceOf outB `shouldSatisfy` T.isInfixOf "cannot help"
    it "budget: the R5.9 pressure floor binds a first goal-miss like a first miss" $
        forM_ [(c, g) | (c, Just g) <- consumerGrid] $ \(c, g) -> do
            let led = ledgerPressure 2 (seedConsumer c)
                q = "default" <> g
                (lA, outA) = ledgerRecord q (junkFound q) led
                (lB, outB) = ledgerRecord q (missEnv q) led
            ladderState lA `shouldBe` ladderState lB
            adviceOf outA `shouldSatisfy` T.isInfixOf "Already held"
            adviceOf outB `shouldSatisfy` T.isInfixOf "Already held"

-- Spelling independence: no query shape escapes the standing goal -----------

spellingSpec :: Spec
spellingSpec = describe "the standing goal outlives the query's spelling" $ do
    it "producer-prefix, bare word and unrelated token are ALL judged" $
        forM_ [(c, g) | (c, Just g) <- consumerGrid] $ \(c, g) ->
            forM_ (spellingsOf g) $ \q -> do
                let (_, out) = ledgerRecord q (junkFound q) (seedConsumer c)
                (q, goalType out) `shouldBe` (q, g)
                (q, goalSat out) `shouldBe` (q, Just (Bool False))
    it "all spellings advance ONE goal cluster, so the ladder converges" $
        forM_ [(c, g) | (c, Just g) <- consumerGrid] $ \(c, g) -> do
            let (led, _) = walk (seedConsumer c) [(q, junkFound q) | q <- spellingsOf g]
                (misses, _, _, _) = ladderState led
            -- Two answered misses; the third call hits the k=2 gate (R9-T2)
            -- and records no further miss.
            map snd misses `shouldBe` [length (spellingsOf g) - 1]
    it "no held consumer evidence: a plain word derives no goal, search unchanged" $ do
        let (led, out) = ledgerRecord "default" (junkFound "default") emptyLedger
            (misses, _, _, _) = ladderState led
        goalOf out `shouldBe` Nothing
        misses `shouldBe` []
        stateOf out `shouldBe` "found"
    it "a literal-argument consumer derives no standing goal" $ do
        let led = seedConsumer ("gust", "Int -> Wind", "Zephyr.Core", "zephyr")
            (_, out) = ledgerRecord "default" (junkFound "default") led
        goalOf out `shouldBe` Nothing
    it "the spelled producer-prefix trigger still fires without held evidence" $ do
        let (_, out) =
                ledgerRecord "defaultPlot" (junkFound "defaultPlot") emptyLedger
        goalType out `shouldBe` "Plot"
        goalSat out `shouldBe` Just (Bool False)
    it "a satisfying exact-name answer is judged satisfied and adds no miss" $ do
        let led =
                seedConsumer
                    ("bars", "[(Text, Double)] -> Plot -> Text", "Cumulus.Plot", "cumulus")
            v = foundEnv "lull" [hitJ "lull" "Wind -> Wind" "Stratus.Air" "stratus" "exact"]
            (led2, out) = ledgerRecord "lull" v led
            (misses, _, _, _) = ladderState led2
        goalSat out `shouldBe` Just (Bool True)
        misses `shouldBe` []

-- The one-envelope disclosure (R3.6) ----------------------------------------

disclosureSpec :: Spec
disclosureSpec = describe "the goal field rides the ONE declared envelope shape" $ do
    let bars = ("bars", "[(Text, Double)] -> Plot -> Text", "Cumulus.Plot", "cumulus")
    it "decode-validates on every outcome class" $ do
        let led = seedConsumer bars
            outs =
                [ snd (ledgerRecord "bars" (consumerFound bars) emptyLedger)
                , snd (ledgerRecord "default" (junkFound "default") led)
                , snd (ledgerRecord "colx" (missEnv "colx") led)
                , snd (ledgerRecord "bad" (badRequest "bad" "blank query") led)
                ]
        forM_ outs $ \o -> envelopeViolations o `shouldBe` []
    it "is present iff a goal stands, on found and not_found alike" $ do
        let led = seedConsumer bars
        isJust (goalOf (snd (ledgerRecord "default" (junkFound "default") led)))
            `shouldBe` True
        isJust (goalOf (snd (ledgerRecord "colx" (missEnv "colx") led)))
            `shouldBe` True
        isJust (goalOf (snd (ledgerRecord "colx" (missEnv "colx") emptyLedger)))
            `shouldBe` False
    it "an unsatisfied disclosure names the goal and the nearest hit" $ do
        let led = seedConsumer bars
            (_, out) = ledgerRecord "default" (junkFound "default") led
            note = maybe "" (textField "note") (goalOf out)
        note `shouldSatisfy` T.isInfixOf "Plot"
        note `shouldSatisfy` T.isInfixOf "defaultPlotLineStyle"

-- The barChart run-181440 fixture (secondary evidence) -----------------------

barChartSpec :: Spec
barChartSpec = describe "barChart: six default-spelling walls can no longer starve the ladder" $
    it "every wall is judged, the ladder closes, repeats dedup" $ do
        let bars = ("bars", "[(Text, Double)] -> Plot -> Text", "Cumulus.Plot", "cumulus")
            spellings =
                ["defaultPlot", "default", "plot default", "defaultP", "defaults", "default"]
            step (l, fresh) q = case ledgerShortcut l q of
                Just o -> (l, fresh ++ [(q, o, False)])
                Nothing ->
                    let (l2, o) = ledgerRecord q (junkFound q) l
                     in (l2, fresh ++ [(q, o, True)])
            (_, outs) = foldl' step (seedConsumer bars, []) spellings
            freshOuts = [o | (_, o, True) <- outs]
            judgedOuts = [o | o <- freshOuts, stateOf o `elem` ["found", "not_found"]]
        -- Every pre-cap fresh answer is judged unsatisfied in-band; past the
        -- give-up rung the terse duplicate reference caps the cluster.
        forM_ judgedOuts $ \o -> goalSat o `shouldBe` Just (Bool False)
        forM_ [o | o <- freshOuts, stateOf o `notElem` ["found", "not_found"]] $
            \o -> stateOf o `shouldBe` "duplicate"
        -- Past the k=2 gate the walls answer the write steer (R9-T2), so
        -- the hunt can never again spend 6+ calls searching.
        any (T.isInfixOf "write the deliverable" . adviceOf) freshOuts
            `shouldBe` True
        -- The repeated spelling dedups instead of re-walling.
        let dups = [o | (q, o, False) <- outs, q == "default"]
        when (null dups) $
            expectationFailure "expected the repeated spelling to dedup"
        forM_ dups $ \o -> stateOf o `shouldBe` "duplicate"
