{-# LANGUAGE OverloadedStrings #-}

{- | R9-T2 goal honesty (search-api.md section 8.3): satisfaction requires
goal-class evidence, a satisfied goal arms the k=2 hard gate, and a standing
goal cites its derivation (R1.1, R5.6, R5.9). The world-change and
producer-card halves live in "Test.WorldCardSpec".
-}
module Test.GoalHonestySpec (goalHonestySpec) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), object, (.=))
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Siza.Agent.Discover.Facts (compilerFact)
import Siza.Agent.Discover.Goal (goalSatisfied, standingGoal, withGoal)
import Siza.Agent.Discover.History (emptyLedger, ledgerRecord)
import Siza.Agent.Discover.Types (StandingGoal (..))
import Test.DiscoverFixtures (field, stateOf, textField)

-- Envelope builders ----------------------------------------------------------

hitJ :: Text -> Text -> Text -> Value
hitJ n ty p =
    object
        [ "name" .= n
        , "type" .= ty
        , "module" .= ("M" :: Text)
        , "package" .= p
        , "version" .= ("1.0.0" :: Text)
        , "install" .= ("installed" :: Text)
        , "matchKind" .= ("exact" :: Text)
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
        , "next" .= ("act" :: Text)
        ]

cardEnv :: Text -> Text -> Value
cardEnv q pkg =
    object
        [ "query" .= q
        , "state" .= ("found" :: Text)
        , "card" .= object ["package" .= pkg]
        , "total" .= (0 :: Int)
        ]

emptyEnv :: Text -> Value
emptyEnv q =
    object
        [ "query" .= q
        , "state" .= ("not_found" :: Text)
        , "next" .= ("no match" :: Text)
        , "total" .= (0 :: Int)
        ]

goalHonestySpec :: Spec
goalHonestySpec = describe "goal honesty (R9-T2)" $ do
    satisfactionGridSpec
    derivationSpec
    gateSpec

-- Satisfaction requires goal-class evidence ----------------------------------

-- | Rename every synthetic token consistently: library-name substitution.
renamings :: [Text -> Text]
renamings = [id, T.replace "Plot" "Blot" . T.replace "cumulus" "granilus"]

satisfactionGridSpec :: Spec
satisfactionGridSpec = describe "satisfaction holds iff goal-class evidence" $
    it "judges the (answer class x renaming) grid" $
        forM_ renamings $ \rn -> do
            let sg = StandingGoal (rn "Plot") "bars" (rn "cumulus")
                grid =
                    [ -- The target IS the goal's deriving consumer, so finding
                      -- it is the goal's own business.
                      ("exact hit", foundEnv "bars" [hitJ "bars" "X -> Y" "p"], True)
                    ,
                        ( "foreign target: exact hit on an unrelated query"
                        , foundEnv "shade" [hitJ "shade" "Html -> Html" "blaze"]
                        , False
                        )
                    ,
                        ( "producer hit"
                        , foundEnv "q" [hitJ "mkPlot" ("Ctx -> " <> rn "Plot") "p"]
                        , True
                        )
                    ,
                        ( "unrelated card"
                        , cardEnv "q" "rzk"
                        , False
                        )
                    ,
                        ( "provenance card"
                        , cardEnv "q" (rn "cumulus")
                        , True
                        )
                    ,
                        ( "false-goal-point: unrelated hit never satisfies"
                        , foundEnv "q" [hitJ "rzkThing" "Rzk" "rzk"]
                        , False
                        )
                    , ("empty", emptyEnv "q", False)
                    ]
            forM_ grid $ \(label, v, expected) ->
                (label :: Text, goalSatisfied sg (target v) v)
                    `shouldBe` (label, expected)
  where
    target = textField "query"

-- The goal cites its derivation ----------------------------------------------

-- | Held facts as 'harvestFacts' shapes them: earliest first.
consumerFact :: Text -> Text -> Text -> Text -> Text
consumerFact n sig m p =
    "`" <> n <> "` :: " <> sig <> " — found in " <> m <> " (" <> p <> ")"

derivationSpec :: Spec
derivationSpec = describe "a standing goal cites its deriving consumer" $ do
    it "a path argument is a literal, not a producer hunt" $
        standingGoal
            [consumerFact "readCsv" "FilePath -> IO DataFrame" "DataFrame" "df"]
            `shouldBe` Nothing
    {- live_test34_wine: the compiler had confirmed `defaultReadOptions ::
    ReadOptions`, but its provenance marker rode into the signature, so the
    goal hunted a producer the working cell already had. -}
    it "a compiler-confirmed producer closes the gap it fills" $ do
        let consumer =
                consumerFact
                    "readCsvWithOpts"
                    "ReadOptions -> FilePath -> IO DataFrame"
                    "DataFrame"
                    "dataframe"
            confirmed = compilerFact "defaultReadOptions" "ReadOptions"
        standingGoal [consumer]
            `shouldBe` Just (StandingGoal "ReadOptions" "readCsvWithOpts" "dataframe")
        standingGoal [consumer, confirmed] `shouldBe` Nothing
    it "derives from the most recent consumer, not the first-harvested" $ do
        let facts =
                [ consumerFact "agg" "Frame -> Expr" "Frame.Core" "framelib"
                , consumerFact
                    "bars"
                    "[(Text, Double)] -> Plot -> Text"
                    "Cumulus.Plot"
                    "cumulus"
                ]
        standingGoal facts
            `shouldBe` Just (StandingGoal "Plot" "bars" "cumulus")
    it "the disclosure names its derivation" $ do
        let sg = StandingGoal "Plot" "bars" "cumulus"
            out = withGoal sg "q" (foundEnv "q" [])
            g = field "goal" out
        fmap (textField "derivedFrom") g `shouldBe` Just "bars"
    it "a spelled (underived) goal carries no derivation claim" $ do
        let sg = StandingGoal "Plot" "" ""
            out = withGoal sg "q" (foundEnv "q" [])
        fmap (textField "derivedFrom") (field "goal" out) `shouldBe` Just ""

-- The k=2 hard gate ----------------------------------------------------------

-- | Junk answer: found-shaped, satisfies no goal.
junkFound :: Text -> Value
junkFound q = foundEnv q [hitJ "defaultLineStyle" "LineStyle" "chartlib"]

barsFound :: Value
barsFound =
    foundEnv "bars" [hitJ "bars" "[(Text, Double)] -> Plot -> Text" "cumulus"]

walk :: [(Text, Value)] -> [Value]
walk = snd . foldl' step (emptyLedger, [])
  where
    step (l, outs) (q, vv) =
        let (l2, o) = ledgerRecord q vv l in (l2, outs ++ [o])

renderAll :: Value -> Text
renderAll v =
    T.intercalate " " [textField k v | k <- ["next", "summary", "ref"]]

gateSpec :: Spec
gateSpec = describe "a satisfied goal discloses; it never replaces answers" $ do
    {- The k=2 gate was the last answer-replacing mechanism: after
    satisfaction it answered same-cluster searches with a citation of the
    whole held-facts list. live_test41: goal HeaderSpec, "satisfied" by a
    SELECTOR, collapsed the constructor hunt (HeaderNone, HeaderSpec, header)
    to "satisfied by held facts: `summarize` …" — a fact that says nothing
    about HeaderSpec, shown because it was first in the ledger. The model
    shipped without header options and the table lost a row. Session memory
    RANKS (the refinement band) and DISCLOSES (the goal field); it never
    withholds a result.
    -}
    it "every post-satisfaction call still answers an envelope" $ do
        let qs = ["defaultPlot", "default", "plot default", "defaultP"]
            outs =
                walk (("bars", barsFound) : [(q, junkFound q) | q <- qs])
        forM_ outs $ \o ->
            stateOf o `shouldSatisfy` (`elem` ["found", "not_found", "duplicate"])
        -- Never the gate's whole-ledger citation.
        forM_ outs $ \o ->
            renderAll o
                `shouldSatisfy` (not . T.isInfixOf "satisfied by held facts")
    it "an unsatisfied goal is never gated (negative control)" $ do
        -- The consumer fact arrives on a query it does NOT satisfy (prose
        -- query, non-producing hit), so the goal stands unsatisfied.
        let seed =
                foundEnv
                    "chart tools"
                    [hitJ "bars" "[(Text, Double)] -> Plot -> Text" "cumulus"]
            qs = ["w1", "w2", "w3", "w4", "w5"]
            outs =
                walk (("chart tools", seed) : [(q, junkFound q) | q <- qs])
        forM_ outs $ \o ->
            renderAll o `shouldSatisfy` (not . T.isInfixOf "goal satisfied")
