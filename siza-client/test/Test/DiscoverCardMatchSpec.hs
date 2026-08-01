{-# LANGUAGE OverloadedStrings #-}

{- | A4: a card is authority over what it states. A listing that reports the
query matched none of its entries is evidence of that, and a search cannot be
"found" on it. Stated over generated queries, modules and export rows.
-}
module Test.DiscoverCardMatchSpec (discoverCardMatchSpec) where

import Data.Aeson (Value (..), object, (.=))
import Data.Maybe (isJust)
import Data.Text (Text)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Siza.Agent.Discover.CardAuthority (cardAnswers)
import Siza.Agent.Discover.Interpret (interpret)
import Siza.Agent.Discover.Merge (discoverEnvelope)
import Siza.Agent.Discover.Types (
    HackageInfo (..),
    NotebookEnv (..),
    SourceAnswer (..),
    okAnswer,
    seededBuiltins,
 )
import Test.DiscoverFixtures (field, hitsOf, stateOf)
import Test.DiscoverGen (genMatchedStampCard, genModulePair, genValueName)

discoverCardMatchSpec :: Spec
discoverCardMatchSpec = describe "a card answers, or says it did not (A4)" $ do
    predicateSpec
    envelopeSpec

envT :: NotebookEnv
envT = seededBuiltins (NotebookEnv [] [] [] [] [] [])

hkT :: HackageInfo
hkT = HackageInfo True []

-- | The envelope a card-only answer produces: no hits, so only the card speaks.
cardEnvelope :: Text -> Value -> Value
cardEnvelope q c =
    discoverEnvelope
        envT
        (interpret envT q)
        8
        [(okAnswer "session" []){saCard = Just c}]
        hkT

stamp :: Value -> Maybe Value
stamp v = field "card" v >>= field "cardAnswers"

predicateSpec :: Spec
predicateSpec = describe "the predicate reads the card's own rows" $ do
    prop "a denial stands only while the rows do not name the query" $
        forAll genMatchedStampCard $ \(q, card, mentions) ->
            counterexample (show card) $
                cardAnswers (interpret envT q) card === mentions
    prop "a card that denies nothing answers" $
        forAll ((,) <$> genValueName <*> genModulePair) $
            \(q, (m, _)) ->
                property (cardAnswers (interpret envT q) (plainCard m))

envelopeSpec :: Spec
envelopeSpec = describe "the state a card is allowed to set" $ do
    prop "a card that denies the query, with no hits, is not found" $
        forAll genMatchedStampCard $ \(q, card, mentions) ->
            let v = cardEnvelope q card
             in counterexample (show v) $
                    conjoin
                        [ property (null (hitsOf v))
                        , stateOf v === (if mentions then "found" else "not_found")
                        ]
    prop "the card is carried, and stamped exactly when it does not answer" $
        forAll genMatchedStampCard $ \(q, card, mentions) ->
            let v = cardEnvelope q card
             in counterexample (show v) $
                    conjoin
                        [ property (isJust (field "card" v))
                        , stamp v === (if mentions then Nothing else Just (Bool False))
                        ]
    prop "a card with nothing to deny still answers, unstamped" $
        forAll ((,) <$> genValueName <*> genModulePair) $
            \(q, (m, _)) ->
                let v = cardEnvelope q (plainCard m)
                 in counterexample (show v) $
                        conjoin [stateOf v === "found", stamp v === Nothing]

plainCard :: Text -> Value
plainCard m =
    object
        [ "module" .= m
        , "status" .= ("ok" :: Text)
        , "exports" .= (["someExport :: Int"] :: [Text])
        ]
