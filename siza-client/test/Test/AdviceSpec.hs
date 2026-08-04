{-# LANGUAGE OverloadedStrings #-}

module Test.AdviceSpec (adviceSpec) where

import Control.Monad (forM_)
import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Siza.Agent.Discover.Advice (
    duplicateEnvelope,
    foundSummary,
    strongEvidence,
 )
import Siza.Agent.Discover.Beside (besideHits, elision, restateFloor)
import Siza.Agent.Discover.CardAuthority (stampCardAnswers)
import Siza.Agent.Discover.Closure (heldHitLine)
import Siza.Agent.Discover.Guidance (actionNext, actionNextCap, cabalLine)
import Siza.Agent.Discover.History (emptyLedger, ledgerRecord)
import Siza.Agent.Discover.Interpret (interpret)
import Siza.Agent.Discover.Merge (discoverEnvelope)
import Siza.Agent.Discover.Types (
    DHit (..),
    HackageInfo (..),
    InstallState (..),
    MatchKind (..),
    NotebookEnv (..),
    hitJson,
    matchKindText,
    mkHit,
    okAnswer,
    seededBuiltins,
 )
import Test.DiscoverFixtures (hitText, stateOf, textField)
import Test.DiscoverGen (genModulePair, genPkgPair, genValueName)

envT :: NotebookEnv
envT = seededBuiltins (NotebookEnv [] [] [] [] [] [])

hkT :: HackageInfo
hkT = HackageInfo True [] []

foundKind :: MatchKind -> Text -> Text -> Value
foundKind kind q name =
    discoverEnvelope envT (interpret envT q) 8 [okAnswer "session" [hit]] hkT
  where
    hit =
        (mkHit name "Syn.Mod" "synpkg")
            { dhKind = kind
            , dhType = "TraceKind"
            , dhVersion = "1.0"
            }

missFor :: Text -> Value
missFor q =
    discoverEnvelope
        envT
        (interpret envT q)
        8
        [okAnswer "session" [], okAnswer "hoogle" []]
        hkT

allKinds :: [MatchKind]
allKinds = [minBound .. maxBound]

adviceSpec :: Spec
adviceSpec = describe "dedup participation vs assertion strength (R8-T1)" $ do
    describe "every stratum dedups from its 2nd occurrence (section 10)" $
        forM_ allKinds $ \kind ->
            it (T.unpack (matchKindText kind) <> ": respelled repeat is a duplicate") $ do
                let wallFor q = foundKind kind q "swirlish"
                    (led1, out1) =
                        ledgerRecord "swirl chart" (wallFor "swirl chart") emptyLedger
                    (_, out2) =
                        ledgerRecord
                            "chart of swirls"
                            (wallFor "chart of swirls")
                            led1
                stateOf out1 `shouldBe` "found"
                stateOf out2 `shouldBe` "duplicate"
                textField "ref" out2 `shouldSatisfy` T.isInfixOf "call 1"

    describe "the reference discloses evidence strength" $ do
        it "a weak wall dedups labelled weak, never promoted to a fact" $ do
            let wallFor q = foundKind MkSubstring q "swirlish"
                (led1, _) =
                    ledgerRecord "swirl chart" (wallFor "swirl chart") emptyLedger
                (_, out2) =
                    ledgerRecord "chart of swirls" (wallFor "chart of swirls") led1
            textField "summary" out2 `shouldSatisfy` T.isInfixOf "weak"
        it "a strong answer keeps the ranked-answer reference" $ do
            let wallFor q = foundKind MkExact q "swirlish"
                (led1, _) =
                    ledgerRecord "swirl chart" (wallFor "swirl chart") emptyLedger
                (_, out2) =
                    ledgerRecord "chart of swirls" (wallFor "chart of swirls") led1
            textField "summary" out2 `shouldSatisfy` T.isInfixOf "same ranked answer"

    describe "assertion strength is unchanged (11.1 / R1.4)" $ do
        forM_ [MkPrefix, MkModule, MkType, MkSubstring, MkSynonym, MkSemantic] $
            \kind ->
                it
                    ( T.unpack (matchKindText kind)
                        <> ": dedup never blocks a later honest miss"
                    )
                    $ do
                        let wallFor q = foundKind kind q "swirlish"
                            (led1, _) =
                                ledgerRecord "swirl chart" (wallFor "swirl chart") emptyLedger
                            (led2, _) =
                                ledgerRecord
                                    "chart of swirls"
                                    (wallFor "chart of swirls")
                                    led1
                            (_, out3) = ledgerRecord "swirl chart" (missFor "swirl chart") led2
                        stateOf out3 `shouldBe` "not_found"
        it "exact evidence still protects its cluster (R1.4)" $ do
            let (led1, _) =
                    ledgerRecord "gust" (foundKind MkExact "gust" "gust") emptyLedger
                (_, out2) = ledgerRecord "gust" (missFor "gust") led1
            stateOf out2 `shouldNotBe` "not_found"

    besideHitsSpec
    cardAuthoritySpec
    nextStepSpec
    adviceTailSpec

{- | B4: an install step read off a top hit that is only similar to the query
tells the caller to add a package for a name the notebook may already hold.
-}
nextStepSpec :: Spec
nextStepSpec = describe "a next step names the hit it was derived from" $ do
    it "stays silent unless the top hit is what was asked for" $
        property $
            forAll ((,,) <$> genValueName <*> genModulePair <*> genPkgPair) $
                \(n, (m, _), (p, _)) ->
                    forAll (elements weakKinds) $ \kind ->
                        actionNext [installable n m p kind] === Nothing
    it "names the hit and the reason, within the cap" $
        property $
            forAll ((,,) <$> genValueName <*> genModulePair <*> genPkgPair) $
                \(n, (m, _), (p, _)) ->
                    forAll (elements [MkExact, MkModule]) $ \kind ->
                        case actionNext [installable n m p kind] of
                            Nothing -> counterexample "no step for an asked-for hit" False
                            Just t ->
                                counterexample (T.unpack t) $
                                    conjoin
                                        [ property (n `T.isInfixOf` t)
                                        , property (p `T.isInfixOf` t)
                                        , property (T.length t <= actionNextCap)
                                        ]
    it "never truncates the build line: over the cap the derivation goes" $
        property $
            forAll ((,,) <$> genValueName <*> genModulePair <*> genPkgPair) $
                \(n, (m, _), (p, _)) ->
                    forAll (choose (30, 60 :: Int)) $ \reps ->
                        let long = T.intercalate "-" (replicate reps p)
                         in case actionNext [installable n m long MkExact] of
                                Nothing -> counterexample "no step for an asked-for hit" False
                                Just t ->
                                    counterexample (show (T.length t, t)) $
                                        conjoin
                                            [ property (cabalLine long `T.isInfixOf` t)
                                            , property (T.length t > actionNextCap)
                                            , property (not ("top hit" `T.isInfixOf` t))
                                            ]

weakKinds :: [MatchKind]
weakKinds = [MkPrefix, MkType, MkSubstring, MkSynonym, MkSemantic]

installable :: Text -> Text -> Text -> MatchKind -> DHit
installable n m p kind =
    (mkHit n m p)
        { dhKind = kind
        , dhInstall = InstAbsentKnown
        , dhCabal = Just (cabalLine p)
        }

{- | B3: a replay that re-reads the hits riding beside it spends the caller's
budget saying twice what the structured payload already says once.
-}
besideHitsSpec :: Spec
besideHitsSpec = describe "a duplicate summary states what the hits do not" $ do
    it "restates no field of a hit it carries, and stays short" $
        property $
            forAll ((,,) <$> genValueName <*> genModulePair <*> genPkgPair) $
                \(n, (m, _), (p, _)) ->
                    let h = hitJson (heldHit n m p)
                        prose = heldHitLine h <> "; " <> foundSummary (found n h)
                        s = textField "summary" (duplicateEnvelope n "call 2" prose [h])
                        echoed =
                            [ t
                            | k <- ["module", "type", "cabal"]
                            , let t = hitText k h
                            , T.length t >= restateFloor
                            , t `T.isInfixOf` s
                            ]
                     in counterexample (T.unpack s) $
                            conjoin
                                [ echoed === []
                                , property (T.length s <= 200)
                                , property (not (T.null s))
                                ]
    it "leaves the prose alone when no hit rides to carry it" $
        property $
            forAll ((,,) <$> genValueName <*> genModulePair <*> genPkgPair) $
                \(n, (m, _), (p, _)) ->
                    let prose = heldHitLine (hitJson (heldHit n m p))
                     in textField "summary" (duplicateEnvelope n "call 2" prose [])
                            === prose
    it "cuts only where the field stands as its own token" $
        property $
            forAll genModulePair $ \(m, _) ->
                restated m ==>
                    let prose = "the note about z" <> m <> "z carries on to the end"
                     in besideHits [object ["module" .= m]] prose === prose
    it "marks a clause it cut, so a reader sees prose was taken" $
        property $
            forAll genModulePair $ \(m, _) ->
                restated m ==>
                    let prose = "keeps this head " <> m <> " and loses this tail"
                        out = besideHits [object ["module" .= m]] prose
                     in counterexample (T.unpack out) $
                            conjoin
                                [ property (elision `T.isSuffixOf` out)
                                , property (not (m `T.isInfixOf` out))
                                , property ("keeps this head" `T.isInfixOf` out)
                                ]

-- | A field short enough to occur inside prose by accident is never cut on.
restated :: Text -> Bool
restated t = T.length t >= restateFloor

{- | A card that stamped itself as not answering the query is evidence of that
and nothing else, so it cannot carry an assertion on its own.
-}
cardAuthoritySpec :: Spec
cardAuthoritySpec = describe "card authority feeds evidence strength" $ do
    it "a card that does not answer the query is not strong evidence" $
        strongEvidence (withCard (stampCardAnswers False bareCard)) `shouldBe` False
    it "a card that answers it still is" $
        strongEvidence (withCard bareCard) `shouldBe` True

bareCard :: Value
bareCard = object ["module" .= ("Some.Module" :: Text)]

withCard :: Value -> Value
withCard c =
    object ["state" .= ("found" :: Text), "hits" .= ([] :: [Value]), "card" .= c]

heldHit :: Text -> Text -> Text -> DHit
heldHit n m p =
    (mkHit n m p)
        { dhKind = MkExact
        , dhType = "Int -> Maybe Text"
        , dhVersion = "1.0"
        , dhCabal = Just ("-- cabal: build-depends: " <> p)
        }

found :: Text -> Value -> Value
found n h =
    object
        [ "state" .= ("found" :: Text)
        , "total" .= (3 :: Int)
        , "hits" .= [h]
        , "query" .= n
        ]

adviceTailSpec :: Spec
adviceTailSpec = describe "heldHitLine clamp discloses, never bare-truncates" $ do
    let longType = T.intercalate " -> " (replicate 20 "Maybe (Either Text Double)")
        longHit =
            (mkHit "bars" "Cumulus.Plot" "cumulus")
                { dhType = longType
                , dhVersion = "0.3.1"
                }
        shortHit =
            (mkHit "bars" "Cumulus.Plot" "cumulus")
                { dhType = "[(Text, Double)] -> Plot -> Text"
                , dhVersion = "0.3.1"
                }
    it "a clamped signature names the omission and its recovery call" $ do
        let line = heldHitLine (hitJson longHit)
        line `shouldSatisfy` T.isInfixOf "truncated"
        line `shouldSatisfy` T.isInfixOf "check_type bars"
    it "a short signature travels whole, with no disclosure noise" $ do
        let line = heldHitLine (hitJson shortHit)
        line `shouldSatisfy` T.isInfixOf "[(Text, Double)] -> Plot -> Text"
        line `shouldSatisfy` (not . T.isInfixOf "truncated")
