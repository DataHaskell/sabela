{-# LANGUAGE OverloadedStrings #-}

module Test.GoalHonestySpec (goalHonestySpec) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), encode, object, (.=))
import qualified Data.ByteString.Lazy as BL
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Siza.Agent.Discover.Advice (foundSummary, harvestFacts)
import Siza.Agent.Discover.Construct (constructEnvelope)
import Siza.Agent.Discover.Facts (compilerFact)
import Siza.Agent.Discover.Goal (goalSatisfied, standingGoal, withGoal)
import Siza.Agent.Discover.History (emptyLedger, ledgerRecord)
import Siza.Agent.Discover.Types (
    DHit (..),
    HackageInfo (..),
    Interpreted (..),
    NotebookEnv (..),
    StandingGoal (..),
    emptyScope,
    mkHit,
    okAnswer,
    seededBuiltins,
 )
import Test.DiscoverFixtures (field, hitText, hitsOf, stateOf, textField)
import Test.TruthGen (genPrefixed)

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
cardEnv q pkg = cardEnvWith q pkg []

cardEnvWith :: Text -> Text -> [Text] -> Value
cardEnvWith q pkg exports =
    object
        [ "query" .= q
        , "state" .= ("found" :: Text)
        , "card" .= object ["package" .= pkg, "exports" .= exports]
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
    declarationSpec

{- | Two type names disjoint by construction, so neither is a substring of the
other and no property passes on an accidental overlap.
-}
genTypePair :: Gen (Text, Text)
genTypePair = (,) <$> conName "u" <*> conName "t"
  where
    conName p = capitalise <$> genPrefixed p
    capitalise t = T.toUpper (T.take 1 t) <> T.drop 1 t

sigType :: Text -> Text
sigType g = g <> " -> " <> g

synDecl :: Text -> Text -> Text
synDecl g n = "type " <> n <> " = " <> sigType g

goalEnv :: Text -> [Value] -> Value
goalEnv g = withGoal (StandingGoal g "useIt" "") "q" . foundEnv "q"

satisfiedIn :: Value -> Maybe Value
satisfiedIn v = field "goal" v >>= field "satisfied"

noteOf :: Value -> Text
noteOf v = maybe "" (textField "note") (field "goal" v)

rankedNames :: Text -> [DHit] -> [Text]
rankedNames g hs = map (hitText "name") (hitsOf (builtEnvelope g hs))

builtEnvelope :: Text -> [DHit] -> Value
builtEnvelope g hs =
    constructEnvelope
        Nothing
        (seededBuiltins (NotebookEnv [] [] [] [] [] []))
        (Interpreted g g Nothing "construct" "" [])
        emptyScope
        8
        [okAnswer "session" hs]
        (HackageInfo True [])

wholeAnswer :: Value -> Text
wholeAnswer = TE.decodeUtf8 . BL.toStrict . encode

dhOf :: Text -> Text -> DHit
dhOf n ty = (mkHit n "M.M" "p"){dhType = ty, dhOrigin = "session"}

{- | Type text now states declarations beside signatures. A declaration
computes nothing, so reading one as a producer claims a goal satisfied by a
name, and introducing one with @::@ states a signature no source gave.
-}
declarationSpec :: Spec
declarationSpec = describe "a declaration produces nothing" $ do
    prop "a synonym expanding to the goal does not satisfy it" $
        forAll genTypePair $ \(g, n) ->
            satisfiedIn (goalEnv g [hitJ n (synDecl g n) "p"])
                === Just (Bool False)
    prop "a function of that very shape does satisfy it (control)" $
        forAll genTypePair $ \(g, n) ->
            satisfiedIn (goalEnv g [hitJ (fnOf n) (sigType g) "p"])
                === Just (Bool True)
    prop "a declaration never outranks a producer of the goal" $
        forAll genTypePair $ \(g, n) ->
            take 1 (rankedNames g [dhOf n (synDecl g n), dhOf (fnOf n) (sigType g)])
                === [fnOf n]
    prop "nothing the harness says states a declaration as a signature" $
        forAll genTypePair $ \(g, n) ->
            let env = foundEnv "q" [hitJ n (synDecl g n) "p"]
                said =
                    [ foundSummary env
                    , noteOf (goalEnv g [hitJ n (synDecl g n) "p"])
                    , T.unwords (harvestFacts env)
                    , wholeAnswer (builtEnvelope g [dhOf n (synDecl g n)])
                    ]
             in all (T.isInfixOf (synDecl g n)) said
                    && not (any (T.isInfixOf (":: " <> synDecl g n)) said)
  where
    fnOf = T.toLower

renamings :: [Text -> Text]
renamings = [id, T.replace "Plot" "Blot" . T.replace "cumulus" "granilus"]

satisfactionGridSpec :: Spec
satisfactionGridSpec = describe "satisfaction holds iff goal-class evidence" $
    it "judges the (answer class x renaming) grid" $
        forM_ renamings $ \rn -> do
            let sg = StandingGoal (rn "Plot") "bars" (rn "cumulus")
                grid =
                    [ ("exact hit", foundEnv "bars" [hitJ "bars" "X -> Y" "p"], True)
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
                    , -- Inverted 2026-07-30 (FR10). Coming from the goal's
                      -- package is provenance, not satisfaction: 23 of 81
                      -- live `satisfied` claims rested on this alone. The row
                      -- below is the positive control, red before this change.

                        ( "provenance card, nothing produces the goal"
                        , cardEnv "q" (rn "cumulus")
                        , False
                        )
                    ,
                        ( "provenance card whose export produces the goal"
                        , cardEnvWith
                            "q"
                            (rn "cumulus")
                            ["mkPlot :: Ctx -> " <> rn "Plot"]
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

consumerFact :: Text -> Text -> Text -> Text -> Text
consumerFact n sig m p =
    "`" <> n <> "` :: " <> sig <> " — found in " <> m <> " (" <> p <> ")"

derivationSpec :: Spec
derivationSpec = describe "a standing goal cites its deriving consumer" $ do
    it "a path argument is a literal, not a producer hunt" $
        standingGoal
            [consumerFact "readCsv" "FilePath -> IO DataFrame" "DataFrame" "df"]
            `shouldBe` Nothing
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
    it "every post-satisfaction call still answers an envelope" $ do
        let qs = ["defaultPlot", "default", "plot default", "defaultP"]
            outs =
                walk (("bars", barsFound) : [(q, junkFound q) | q <- qs])
        forM_ outs $ \o ->
            stateOf o `shouldSatisfy` (`elem` ["found", "not_found", "duplicate"])
        forM_ outs $ \o ->
            renderAll o
                `shouldSatisfy` (not . T.isInfixOf "satisfied by held facts")
    it "an unsatisfied goal is never gated (negative control)" $ do
        let seed =
                foundEnv
                    "chart tools"
                    [hitJ "bars" "[(Text, Double)] -> Plot -> Text" "cumulus"]
            qs = ["w1", "w2", "w3", "w4", "w5"]
            outs =
                walk (("chart tools", seed) : [(q, junkFound q) | q <- qs])
        forM_ outs $ \o ->
            renderAll o `shouldSatisfy` (not . T.isInfixOf "goal satisfied")
