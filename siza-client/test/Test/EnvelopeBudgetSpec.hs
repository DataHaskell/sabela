{-# LANGUAGE OverloadedStrings #-}

{- | What 'boundEnvelope' actually guarantees. The budget is not absolute: the
last hit is never shed, because an envelope with no hits reports nothing, so
the weaker contract that replaces it is written down here.
-}
module Test.EnvelopeBudgetSpec (envelopeBudgetSpec) where

import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Data.Aeson (Value (..), object, toJSON, (.=))
import Data.Foldable (toList)
import Siza.Agent.Discover.Affordance (clashBudget)
import Siza.Agent.Discover.Envelope (
    boundEnvelope,
    envelopeCharBudget,
    envelopeChars,
    envelopeViolations,
    maxTypeChars,
    schemaPromise,
 )
import Siza.Agent.Discover.Evict (
    elidableFields,
    elidedEntries,
    evictionViolations,
    producerMarker,
 )
import Siza.Agent.Discover.Interpret (interpret)
import Siza.Agent.Discover.Merge (discoverEnvelope)
import Siza.Agent.Discover.Types (
    DHit (..),
    InstallState (..),
    MatchKind (..),
    mkHit,
    okAnswer,
 )
import Test.DiscoverFixtures (hitText, hitsOf)
import Test.DiscoverGen (genModulePair, genPkgPair, genQualName)
import Test.LadderFixtures (envT, hkT)

envelopeBudgetSpec :: Spec
envelopeBudgetSpec = describe "boundEnvelope states its real guarantee" $ do
    it "never sheds every hit: an answer with no hits answers nothing" $
        property $
            forAll genOversized $ \v ->
                counterexample (show (envelopeChars v)) $
                    not (null (hitsOf v)) ==>
                        not (null (hitsOf (boundEnvelope v)))
    it "fits the budget unless one clamped hit alone exceeds it" $
        property $
            forAll genOversized $ \v ->
                let out = boundEnvelope v
                    over = envelopeChars out > envelopeCharBudget
                 in counterexample (show (envelopeChars v, envelopeChars out, out)) $
                        property (not over)
                            .||. ( length (hitsOf out)
                                    === 1
                                    .&&. property (all typeClamped (hitsOf out))
                                 )
    evictionSpec

-- | Median width of the 13 payloads the wave-2 live rounds actually emitted.
waveTwoMedianChars :: Int
waveTwoMedianChars = 2307

{- | The budget is the regression backstop, so it has to bind — stated against
a payload of the measured width rather than against the number itself, which
would survive any wave that changes what a payload contains.
-}
budgetBindsSpec :: Spec
budgetBindsSpec = describe "the budget binds on a wave-2-sized payload" $ do
    it "caps below the width the live rounds actually emitted" $
        envelopeCharBudget `shouldSatisfy` (< waveTwoMedianChars)
    it "runs the ladder on a payload of that width, and discloses" $
        property $
            forAll (genWidth waveTwoMedianChars) $ \v ->
                let out = boundEnvelope v
                 in counterexample (show (envelopeChars v, envelopeChars out)) $
                        conjoin
                            [ property (envelopeChars out < envelopeChars v)
                            , property (not (null (elidedEntries out)))
                            ]

{- | An advisory-laden answer grown to a given character width by widening the
factual field — the card — so the ladder has to work down to it rather than
buying the whole deficit back from one advisory note.
-}
genWidth :: Int -> Gen Value
genWidth target = pad <$> genLaden
  where
    pad v
        | envelopeChars v >= target = v
        | otherwise = pad (overExports (<> [row (length v')]) v)
      where
        v' = cardExports v
    row i = String ("pad" <> tShow i <> " :: Int -> Text")

overExports :: ([Value] -> [Value]) -> Value -> Value
overExports f (Object o) = Object (over overCard "card" o)
  where
    overCard (Object c) = Object (over overList "exports" c)
    overCard x = x
    overList (Array es) = toJSON (f (toList es))
    overList x = x
    over g k m = maybe m (\x -> KM.insert (K.fromText k) (g x) m) (KM.lookup (K.fromText k) m)
overExports _ x = x

cardExports :: Value -> [Value]
cardExports (Object o) = case KM.lookup (K.fromText "card") o of
    Just (Object c) -> case KM.lookup (K.fromText "exports") c of
        Just (Array es) -> toList es
        _ -> []
    _ -> []
cardExports _ = []

{- | What the tool description tells a reader about eviction. A shed class the
description never names reads to the caller as a class that was never there.
-}
promiseSpec :: Spec
promiseSpec = describe "the description promises what the ladder does" $ do
    it "names the disclosure key and every shed-able class" $ do
        schemaPromise `shouldSatisfy` T.isInfixOf "`elided`"
        filter (not . (`T.isInfixOf` schemaPromise)) elidableFields `shouldBe` []
    it "names every class an evicted answer really reports" $
        property $
            forAll genLaden $ \v ->
                conjoin
                    [ counterexample (T.unpack f) (property (f `T.isInfixOf` schemaPromise))
                    | (f, _) <- elidedEntries (boundEnvelope v)
                    ]
    it "states the clash cap the marker is actually held to" $ do
        promisedClashCap `shouldBe` Just clashBudget
        schemaPromise `shouldSatisfy` T.isInfixOf "counts the others"

-- | The cap the description states for `ambiguousWith`, read back out of it.
promisedClashCap :: Maybe Int
promisedClashCap = case T.breakOn capTail schemaPromise of
    (pre, rest)
        | not (T.null rest)
        , digits <- T.takeWhileEnd isDigit pre
        , not (T.null digits) ->
            Just (read (T.unpack digits))
    _ -> Nothing
  where
    capTail = "-character cap per answer"

{- | Advisory fields may be shed; a shed field is named. The law is stated
over (input, output) because that is the claim, and 'boundEnvelope' is pure.
-}
evictionSpec :: Spec
evictionSpec = describe "eviction discloses what it shed" $ do
    budgetBindsSpec
    cardShrinkSpec
    promiseSpec
    it "every shed field class is named in `elided`, with its count" $
        property $
            forAll genLaden $ \v ->
                let out = boundEnvelope v
                 in counterexample (show (envelopeChars v, out)) $
                        evictionViolations v out === []
    it "names no class it did not shed" $
        property $
            forAll genLaden $ \v ->
                let out = boundEnvelope v
                 in conjoin
                        [ counterexample (T.unpack f) (property (shed f v out))
                        | (f, _) <- elidedEntries out
                        ]
    it "bounding an already-bounded answer discloses nothing further" $
        property $
            forAll genLaden $ \v ->
                let out = boundEnvelope v
                 in evictionViolations out (boundEnvelope out) === []
    it "an evicted envelope is still a legal envelope" $
        property $
            forAll genLaden $
                \v -> envelopeViolations (boundEnvelope v) === []
    it "only the declared classes are ever named" $
        property $
            forAll genLaden $ \v ->
                let out = boundEnvelope v
                 in conjoin
                        [ property (f `elem` elidableFields)
                        | (f, _) <- elidedEntries out
                        ]

-- | A class named in `elided` really is absent or shorter than it was.
shed :: Text -> Value -> Value -> Bool
shed f before after = width f after < width f before
  where
    width "producer" v =
        length [() | h <- hitsOf v, producerMarker `T.isInfixOf` hitText "use" h]
    width "ambiguousWith" v =
        length [() | h <- hitsOf v, not (T.null (hitText "ambiguousWith" h))]
    width k v = T.length (topStr k v)
    topStr k (Object o) = case KM.lookup (K.fromText k) o of
        Just (String s) -> s
        _ -> ""
    topStr _ _ = ""

{- | An answer carrying every advisory class at once plus the widest factual
one, over arbitrary names, so the ladder has something of each kind to shed.
-}
genLaden :: Gen Value
genLaden = do
    n <- genQualName
    (m1, m2) <- genModulePair
    (p, _) <- genPkgPair
    k <- choose (2, 12 :: Int)
    notes <- choose (2, 5 :: Int)
    exports <- choose (9, 40 :: Int)
    pure . object $
        [ "query" .= n
        , "state" .= ("found" :: Text)
        , "card" .= ladenCard n m1 p exports
        , "hits" .= [ladenHit n m1 m2 p i | i <- [1 .. k]]
        , "shown" .= k
        , "omitted" .= (0 :: Int)
        , "total" .= k
        , "narrow" .= T.intercalate "; " [noteAt m1 i | i <- [1 .. notes]]
        , "summary" .= T.intercalate "; " [noteAt m2 i | i <- [1 .. notes]]
        ]
  where
    noteAt m i = m <> " note " <> tShow i <> " " <> T.replicate 20 "z"

ladenCard :: Text -> Text -> Text -> Int -> Value
ladenCard n m p exports =
    object
        [ "module" .= m
        , "package" .= p
        , "exports" .= [n <> tShow i <> " :: Int -> Text" | i <- [1 .. exports]]
        ]

{- | A card is factual, so it is clamped rather than evicted, and the clamp
owes its own count. The widest field measured in the live rounds, and until now
the one ladder step no property watched.
-}
cardShrinkSpec :: Spec
cardShrinkSpec = describe "a clamped card accounts for the rows it dropped" $
    it "on a payload only the card can pay for, it loses rows and counts them" $
        property $
            forAll (genWidth (3 * envelopeCharBudget)) $ \v ->
                let out = boundEnvelope v
                 in counterexample (show (length (cardExports v), out)) $
                        conjoin
                            [ property (length (cardExports out) < length (cardExports v))
                            , evictionViolations v out === []
                            ]

ladenHit :: Text -> Text -> Text -> Text -> Int -> Value
ladenHit n m1 m2 p i =
    object
        [ "name" .= (n <> tShow i)
        , "install" .= ("installed" :: Text)
        , "matchKind" .= ("exact" :: Text)
        , "origin" .= ("session" :: Text)
        , "module" .= m1
        , "package" .= p
        , "type" .= T.replicate 3 "Int -> Text -> "
        , "use" .= ("import " <> m1 <> producerMarker <> n <> " builds it")
        , "ambiguousWith" .= ("2 other modules export this name, incl. " <> m2)
        ]

-- | A signature the bounder has already shrunk as far as it shrinks them.
typeClamped :: Value -> Bool
typeClamped h = T.length (hitText "type" h) <= maxTypeChars + 1

{- | Envelopes whose hits alone blow the budget, including the single-hit case
where there is nothing left to shed.
-}
genOversized :: Gen Value
genOversized = do
    n <- genQualName
    (m, _) <- genModulePair
    (p, _) <- genPkgPair
    k <- choose (1, 5 :: Int)
    len <- choose (400, 4000)
    let ty = T.replicate len "x"
        hs = [bigHit ty n m p i | i <- [1 .. k]]
    pure (discoverEnvelope envT (interpret envT n) 40 [okAnswer "session" hs] hkT)

bigHit :: Text -> Text -> Text -> Text -> Int -> DHit
bigHit ty n m p i =
    (mkHit (n <> tShow i) (m <> ".M" <> tShow i) p)
        { dhType = ty
        , dhKind = MkExact
        , dhVersion = "1.0"
        , dhInstall = InstInstalled
        , dhOrigin = "session"
        }

tShow :: Int -> Text
tShow = T.pack . show
