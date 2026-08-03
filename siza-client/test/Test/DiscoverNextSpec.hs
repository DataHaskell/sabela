{-# LANGUAGE OverloadedStrings #-}

{- | @next@ is advice, so it is bound by the two laws every advisory field is:
it is shed under budget and disclosed, and it never states again what the
answer's own hits already carry. Stated over generated names throughout.
-}
module Test.DiscoverNextSpec (discoverNextSpec) where

import Data.Aeson (Value)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Siza.Agent.Discover.Advice (setNext)
import Siza.Agent.Discover.Closure (heldHitLine)
import Siza.Agent.Discover.Envelope (
    boundEnvelope,
    envelopeCharBudget,
    envelopeChars,
    envelopeViolations,
 )
import Siza.Agent.Discover.Evict (elidedEntries, evictionViolations)
import Siza.Agent.Discover.History (duplicateEnvelope)
import Siza.Agent.Discover.MissLadder (
    MissOutcome (..),
    missAdvice,
    withCandidate,
 )
import Siza.Agent.Discover.Types (DHit (..), hitJson, mkHit)
import Test.DiscoverFixtures (textField)
import Test.DiscoverGen (genModulePair, genPkgPair, genValueName)

discoverNextSpec :: Spec
discoverNextSpec = describe "`next` is advice, and is bound like advice" $ do
    nextEvictionSpec
    nextReplaySpec

{- | A hit whose every restatable field is spelled from its own module and
package, so two hits from disjoint modules restate nothing of each other.
-}
heldHit :: Text -> Text -> Text -> Value
heldHit n m p =
    hitJson
        (mkHit n m p)
            { dhType = m <> " -> Text"
            , dhVersion = "1.0.0"
            , dhCabal = Just ("-- cabal: build-depends: " <> p)
            }

nextEvictionSpec :: Spec
nextEvictionSpec = describe "under budget pressure it sheds, and says so" $
    it "sheds a long next and discloses the shed under `elided`" $
        property $
            forAll ((,,) <$> genValueName <*> genModulePair <*> genPkgPair) $
                \(n, (m, _), (p, _)) ->
                    forAll (choose (100, 300 :: Int)) $ \k ->
                        let dup = duplicateEnvelope n "call 3" "s" [heldHit n m p]
                            note i = n <> " step " <> T.pack (show i) <> pad
                            v = setNext (T.intercalate "; " (map note [1 .. k])) dup
                            out = boundEnvelope v
                         in counterexample (show (envelopeChars v, envelopeChars out, out)) $
                                conjoin
                                    [ property (envelopeChars v > envelopeCharBudget)
                                    , property (envelopeChars out <= envelopeCharBudget)
                                    , property ("next" `elem` map fst (elidedEntries out))
                                    , evictionViolations v out === []
                                    , envelopeViolations out === []
                                    ]
  where
    pad = " " <> T.replicate 20 "z"

{- | The rung that writes a closure record. It is reached with no standing
goal, so what comes back is the advice itself and not an escalation.
-}
rungThree :: [Text] -> Maybe Value -> Text -> Value -> Text
rungThree facts bestHeld qn v =
    textField
        "next"
        (adviceOf (missAdvice [] Set.empty facts bestHeld [] Nothing 3 qn v))

adviceOf :: MissOutcome -> Value
adviceOf (Advise v) = v
adviceOf (EscalateType _ v) = v

nextReplaySpec :: Spec
nextReplaySpec = describe "it never restates what the same answer carries" $ do
    it "gives up on a carried hit in fields, and on an uncarried one in prose" $
        property $
            forAll ((,,) <$> genValueName <*> genModulePair <*> genPkgPair) $
                \(n, (m1, m2), (p1, p2)) ->
                    let carried = heldHit n m1 p1
                        elsewhere = heldHit n m2 p2
                        v = duplicateEnvelope n "call 3" "s" [carried]
                        said b = rungThree [heldHitLine carried] (Just b) n v
                     in conjoin
                            [ counterexample (T.unpack (said carried)) $
                                property (not (m1 `T.isInfixOf` said carried))
                            , counterexample (T.unpack (said elsewhere)) $
                                property (m2 `T.isInfixOf` said elsewhere)
                            ]

    it "leaves the step already there when it has nothing of its own to add" $
        property $
            forAll ((,,) <$> genValueName <*> genModulePair <*> genPkgPair) $
                \(n, (m, _), (p, _)) ->
                    let carried = heldHit n m p
                        step = "run " <> n
                        v = setNext step (duplicateEnvelope n "call 3" "s" [carried])
                     in rungThree [heldHitLine carried] (Just carried) n v === step

    it "states, at the hard stop, only the held facts its hits do not" $
        property $
            forAll ((,,) <$> genValueName <*> genModulePair <*> genPkgPair) $
                \(n, (m1, m2), (p, _)) ->
                    let carried = heldHit n m1 p
                        dup = duplicateEnvelope n "call 3" "s" [carried]
                        facts = [heldHitLine carried, m2 <> " holds one too"]
                        said = textField "next" (withCandidate mempty facts dup)
                     in counterexample (T.unpack said) $
                            property (m2 `T.isInfixOf` said)
                                .&&. property (not (m1 `T.isInfixOf` said))
