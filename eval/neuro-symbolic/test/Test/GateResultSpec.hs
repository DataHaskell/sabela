{-# LANGUAGE OverloadedStrings #-}

module Test.GateResultSpec (spec) where

import Data.Aeson (decode, encode)
import qualified Data.Set as Set
import qualified Data.Text as T
import Test.Hspec

import Eval.GateResult (
    GateResult (..),
    SearchMode (..),
    gateKey,
    isDone,
    modeText,
    renderGateResults,
 )

sample :: GateResult
sample = GateResult "topRegions" 1 SearchOff True 5 3 "done" 12000

spec :: Spec
spec = describe "Eval.GateResult persistence/resume" $ do
    describe "JSON round-trip" $ do
        it "encodes the mode as off/on and round-trips" $ do
            decode (encode sample) `shouldBe` Just sample
            decode (encode (sample{grMode = SearchOn}))
                `shouldBe` Just (sample{grMode = SearchOn})
        it "round-trips grStopped, including the infra-error reason" $ do
            decode (encode (sample{grStopped = "error"}))
                `shouldBe` Just (sample{grStopped = "error"})
        it "round-trips grCtxChars (context spent per episode)" $
            decode (encode (sample{grCtxChars = 34567}))
                `shouldBe` Just (sample{grCtxChars = 34567})
        it "serialises grMode as the wire tag" $ do
            modeText SearchOff `shouldBe` "off"
            modeText SearchOn `shouldBe` "on"
        it "parses a hand-written results line, grStopped defaulting to \"\"" $
            decode
                "{\"grTask\":\"topRegions\",\"grSeed\":1,\"grMode\":\"off\",\"grPass\":true,\"grTurns\":5,\"grCalls\":3}"
                `shouldBe` Just sample{grStopped = "", grCtxChars = 0}

    describe "done-set / skip predicate" $ do
        let done = Set.fromList [gateKey sample]
        it "contains the recorded triple" $
            isDone done "topRegions" 1 SearchOff `shouldBe` True
        it "does not contain the other mode" $
            isDone done "topRegions" 1 SearchOn `shouldBe` False
        it "does not contain a different seed or task" $ do
            isDone done "topRegions" 2 SearchOff `shouldBe` False
            isDone done "other" 1 SearchOff `shouldBe` False

    describe "renderGateResults" $
        it "shows a per-task table and the overall comparison" $ do
            let r =
                    renderGateResults
                        [ sample
                        , sample{grMode = SearchOn, grPass = True}
                        ]
            ("Per task" `T.isInfixOf` r) `shouldBe` True
            ("topRegions" `T.isInfixOf` r) `shouldBe` True
            ("Overall" `T.isInfixOf` r) `shouldBe` True
            ("Cost to pass" `T.isInfixOf` r) `shouldBe` True
