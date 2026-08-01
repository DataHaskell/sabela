{-# LANGUAGE OverloadedStrings #-}

{- | C2-6a/b/c/d and C1-11: a repeated question returns at least what the
first answer returned, held evidence is keyed on (entity, scope), and no rung
of the ladder states anything it did not compute.
-}
module Test.DiscoverDuplicatePayloadSpec (discoverDuplicatePayloadSpec) where

import Data.Aeson (Value)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Siza.Agent.Discover.Advice (answerKeyFields)
import Siza.Agent.Discover.Closure (bestHeldFor)
import Siza.Agent.Discover.Envelope (
    envelopeCharBudget,
    envelopeChars,
    envelopeViolations,
 )
import Siza.Agent.Discover.History (
    emptyLedger,
    heldEvidence,
    ledgerClose,
    ledgerRecord,
    ledgerShortcut,
 )
import Siza.Agent.Discover.Interpret (interpret)
import Siza.Agent.Discover.Merge (discoverEnvelope)
import Siza.Agent.Discover.Request (requestKey)
import Siza.Agent.Discover.Types (
    DHit (..),
    InstallState (..),
    MatchKind (..),
    okAnswer,
 )
import Test.DiscoverFixtures (hitText, hitsOf, stateOf, textField)
import Test.DiscoverGen (
    ScopeArgs (..),
    genModulePair,
    genPkgPair,
    genQualName,
    genScopeArgs,
    genValueName,
 )
import Test.LadderFixtures (
    encodeT,
    envT,
    exactHit,
    foundOver,
    hkT,
    missOver,
    projected,
    runLadder,
    scopedKey,
 )

discoverDuplicatePayloadSpec :: Spec
discoverDuplicatePayloadSpec =
    describe "a duplicate answers with the payload it deduped (C2-6, C1-11)" $ do
        payloadSpec
        mutatedQuerySpec
        scopeKeyedEvidenceSpec
        rankedEvidenceSpec
        computedRungSpec

-- | C2-6a: the same request key, asked twice.
payloadSpec :: Spec
payloadSpec = describe "C2-6a a duplicate is never less informative" $
    it "the repeat carries the first answer's ranked hits, and stays legal" $
        property $
            forAll ((,,) <$> genQualName <*> genModulePair <*> genPkgPair) $
                \(n, (m, _), (p, _)) ->
                    forAll (genScopeArgs m p) $ \sa ->
                        let key = requestKey n (saArgs sa)
                            v = foundOver n [exactHit n m p]
                            (led, out1) = ledgerRecord key v emptyLedger
                         in case ledgerShortcut led key of
                                Nothing ->
                                    counterexample
                                        "no duplicate for a byte-identical repeat"
                                        False
                                Just out2 ->
                                    counterexample (show (encodeT out1, encodeT out2)) $
                                        conjoin
                                            [ stateOf out2 === "duplicate"
                                            , not (null (hitsOf out1)) ==>
                                                not (null (hitsOf out2))
                                            , map projected (hitsOf out2)
                                                === map projected (hitsOf out1)
                                            , envelopeViolations out2 === []
                                            , property
                                                ( envelopeChars out2
                                                    <= envelopeCharBudget
                                                )
                                            ]

-- | C2-6d: a lexical mutation that yields the same answer still returns it.
mutatedQuerySpec :: Spec
mutatedQuerySpec = describe "C2-6d a mutated query with the same answer" $ do
    it "names the facets it compared, and claims nothing beyond them" $
        property $
            forAll ((,,) <$> genValueName <*> genModulePair <*> genPkgPair) $
                \(n, (m, _), (p, _)) ->
                    let v = foundOver n [exactHit n m p]
                        (led1, _) = ledgerRecord n v emptyLedger
                        (_, out2) = ledgerRecord (n <> " again") v led1
                        s = textField "summary" out2
                     in counterexample (T.unpack s) $
                            conjoin [property (k `T.isInfixOf` s) | k <- answerKeyFields]
    it "returns that answer's hits rather than a payload-free reference" $
        property $
            forAll ((,,) <$> genValueName <*> genModulePair <*> genPkgPair) $
                \(n, (m, _), (p, _)) ->
                    forAll (elements [" ::", " \"", " use"]) $ \suffix ->
                        let v = foundOver n [exactHit n m p]
                            (led1, _) = ledgerRecord n v emptyLedger
                            (_, out2) = ledgerRecord (n <> suffix) v led1
                         in conjoin
                                [ stateOf out2 === "duplicate"
                                , property (not (null (hitsOf out2)))
                                , map projected (take 1 (hitsOf out2))
                                    === map projected (take 1 (hitsOf v))
                                ]

{- | C2-6b: a fact recorded under one scope is never replayed under a scope
that excludes it, at any rung of the ladder.
-}
scopeKeyedEvidenceSpec :: Spec
scopeKeyedEvidenceSpec = describe "C2-6b held evidence is keyed on (entity, scope)" $ do
    it "a scope the caller excluded is never quoted back at any rung" $
        property $
            forAll ((,,) <$> genValueName <*> genModulePair <*> genPkgPair) $
                \(n, (m1, m2), (p1, p2)) ->
                    let rounds = [n, n <> " use", n <> " how", n <> " call", n <> " x"]
                     in noLeakOver n m1 p1 [scopedKey m2 p2 q | q <- rounds]
    it "the repeat limit quotes no fact the scope excludes either" $
        property $
            forAll ((,,) <$> genValueName <*> genModulePair <*> genPkgPair) $
                \(n, (m1, m2), (p1, p2)) ->
                    let q = scopedKey m2 p2 n
                     in noLeakOver n m1 p1 (replicate 5 q)
    it "the closed rung quotes no fact the scope excludes either" $
        property $
            forAll ((,,) <$> genValueName <*> genModulePair <*> genPkgPair) $
                \(n, (m1, m2), (p1, p2)) ->
                    let q = scopedKey m2 p2 n
                        (led, _) =
                            runLadder
                                emptyLedger
                                [ (n, foundOver n [exactHit n m1 p1])
                                , (q, missOver n)
                                ]
                        outs = maybeToList (ledgerShortcut (ledgerClose led) q)
                        leaked =
                            [ o
                            | o <- outs
                            , let t = encodeT o
                            , m1 `T.isInfixOf` t || p1 `T.isInfixOf` t
                            ]
                     in counterexample (show (map encodeT outs)) $
                            not (null outs) .&&. property (null leaked)
    it "a scope naming a prefix of the held module is not answered by it" $
        property $
            forAll ((,,) <$> genValueName <*> genModulePair <*> genPkgPair) $
                \(n, (m, _), (p1, p2)) ->
                    forAll (elements ["Strict", "Lazy", "Internal"]) $ \sub ->
                        let q = scopedKey m p2 n
                         in noLeakOver n (m <> "." <> sub) p1 (replicate 5 q)

{- | Answer once globally, then ask a differently scoped question the given
number of times through the real shortcut step, and assert the first answer's
module and package are never spoken back.
-}
noLeakOver :: Text -> Text -> Text -> [Text] -> Property
noLeakOver n m1 p1 keys =
    counterexample (show (map encodeT leaked)) (null leaked)
  where
    (_, outs) =
        runLadder
            emptyLedger
            ((n, foundOver n [exactHit n m1 p1]) : [(k, missOver n) | k <- keys])
    leaked =
        [ o
        | o <- drop 1 outs
        , let t = encodeT o
        , m1 `T.isInfixOf` t || p1 `T.isInfixOf` t
        ]

{- | C2-6c: the held hit is the response's own top-ranked hit, and a later
answer for the same question supersedes an earlier one.
-}
rankedEvidenceSpec :: Spec
rankedEvidenceSpec = describe "C2-6c the ranker decides what is held" $ do
    it "holds the response's own first hit, not the lowest matchKind" $
        property $
            forAll ((,,) <$> genValueName <*> genModulePair <*> genPkgPair) $
                \(n, (m1, m2), (p1, p2)) ->
                    let inScope =
                            (exactHit n m1 p1)
                                { dhKind = MkPrefix
                                , dhName = n <> "By"
                                }
                        absent =
                            (exactHit n m2 p2)
                                { dhInstall = InstAbsentUnknown
                                , dhOrigin = "hoogle"
                                }
                        v =
                            discoverEnvelope
                                envT
                                (interpret envT n)
                                10
                                [okAnswer "session" [inScope], okAnswer "hoogle" [absent]]
                                hkT
                        (led, _) = ledgerRecord n v emptyLedger
                        top = take 1 (hitsOf v)
                     in counterexample (show (encodeT v)) $
                            fmap projected (bestHeldFor (heldEvidence led) n)
                                === fmap projected (headMay top)
    it "a later answer for the same question supersedes the earlier one" $
        property $
            forAll ((,,) <$> genValueName <*> genModulePair <*> genPkgPair) $
                \(n, (m1, m2), (p1, p2)) ->
                    let v1 = foundOver n [exactHit n m1 p1]
                        v2 = foundOver n [exactHit n m2 p2]
                        (led1, _) = ledgerRecord n v1 emptyLedger
                        (led2, _) = ledgerRecord (n <> " again") v2 led1
                     in fmap (hitText "module") (bestHeldFor (heldEvidence led2) n)
                            === Just m2

headMay :: [a] -> Maybe a
headMay (x : _) = Just x
headMay [] = Nothing

{- | C1-11: every rung states a computed fact; none tells the caller to search
again or to narrow its query.
-}
computedRungSpec :: Spec
computedRungSpec = describe "C1-11 a rung reports, it does not advise" $
    it "no rung of the ladder carries an advisory sentence" $
        property $
            forAll ((,,) <$> genValueName <*> genModulePair <*> genPkgPair) $
                \(n, (m, _), (p, _)) ->
                    let rounds =
                            [ n
                            , n <> " use"
                            , n <> " how"
                            , n <> " call"
                            , n <> " x"
                            , n <> " y"
                            ]
                        (_, outs) =
                            runLadder
                                emptyLedger
                                ( (n, foundOver n [exactHit n m p])
                                    : [(q, missOver q) | q <- rounds]
                                )
                        offenders =
                            [ (phrase, encodeT o)
                            | o <- outs
                            , let t = T.toLower (advisoryText o)
                            , phrase <- advisoryPhrases
                            , phrase `T.isInfixOf` t
                            ]
                     in counterexample (show offenders) (null offenders)

advisoryText :: Value -> Text
advisoryText v =
    T.unwords [textField k v | k <- ["next", "summary", "narrow", "reason"]]

{- | Paraphrases of "go and search again". A rung that emits any of them is
spending the caller's turn on advice instead of on an answer.
-}
advisoryPhrases :: [Text]
advisoryPhrases =
    [ "try another query"
    , "rephrase"
    , "retry discover"
    , "search again"
    , "narrow with a module"
    , "narrow your search"
    , "try a different"
    , "trust the held fact and act"
    , "act on what is held"
    ]
