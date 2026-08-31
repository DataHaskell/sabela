{-# LANGUAGE OverloadedStrings #-}

module Test.GateResultSpec (spec) where

import Data.Aeson (decode, encode)
import qualified Data.Text as T
import Test.Hspec

import Eval.GateResult (
    ContextMetric (..),
    GateResult (..),
    SearchMode (..),
    gateKeysForMetric,
    isDone,
    modeText,
    renderGateResults,
 )

sample :: GateResult
sample =
    GateResult
        "topRegions"
        1
        SearchOff
        True
        5
        3
        "done"
        12000
        EncodedRequestBodyBytes

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
        it "round-trips grCtxChars (legacy name for cumulative request bytes)" $
            decode (encode (sample{grCtxChars = 34567}))
                `shouldBe` Just (sample{grCtxChars = 34567})
        it "round-trips the request-body byte discriminator" $
            decode (encode sample) `shouldBe` Just sample
        it "serialises grMode as the wire tag" $ do
            modeText SearchOff `shouldBe` "off"
            modeText SearchOn `shouldBe` "on"
        it "parses a hand-written results line, grStopped defaulting to \"\"" $
            decode
                "{\"grTask\":\"topRegions\",\"grSeed\":1,\"grMode\":\"off\",\"grPass\":true,\"grTurns\":5,\"grCalls\":3}"
                `shouldBe` Just
                    sample
                        { grStopped = ""
                        , grCtxChars = 0
                        , grContextMetric = LegacyTranscriptChars
                        }

    describe "done-set / skip predicate" $ do
        let done = gateKeysForMetric EncodedRequestBodyBytes [sample]
        it "contains the recorded triple" $
            isDone done "topRegions" 1 SearchOff `shouldBe` True
        it "does not contain the other mode" $
            isDone done "topRegions" 1 SearchOn `shouldBe` False
        it "does not contain a different seed or task" $ do
            isDone done "topRegions" 2 SearchOff `shouldBe` False
            isDone done "other" 1 SearchOff `shouldBe` False

        it "reruns a legacy key to collect request-body bytes" $ do
            let legacy = sample{grContextMetric = LegacyTranscriptChars}
                measured = gateKeysForMetric EncodedRequestBodyBytes [legacy]
            isDone measured "topRegions" 1 SearchOff `shouldBe` False

        it "skips a key once the requested metric has been recorded" $ do
            let legacy = sample{grContextMetric = LegacyTranscriptChars}
                measured = gateKeysForMetric EncodedRequestBodyBytes [legacy, sample]
            isDone measured "topRegions" 1 SearchOff `shouldBe` True

    describe "renderGateResults" $ do
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
            ("mean cumulative bytes" `T.isInfixOf` r) `shouldBe` True

        it "excludes legacy rows from request-byte means and discloses them" $ do
            let legacy =
                    sample
                        { grCtxChars = 988000
                        , grPass = False
                        , grContextMetric = LegacyTranscriptChars
                        }
                r = renderGateResults [sample, legacy]
            r `shouldSatisfy` T.isInfixOf "12.0k"
            r `shouldSatisfy` (not . T.isInfixOf "500.0k")
            r `shouldSatisfy` T.isInfixOf "A 1/1"
            r `shouldSatisfy` (not . T.isInfixOf "A 1/2")
            r `shouldSatisfy` T.isInfixOf "Excluded legacy context rows: 1"

        it "reports legacy exclusion even when no request-byte rows exist" $ do
            let legacy = sample{grContextMetric = LegacyTranscriptChars}
                r = renderGateResults [legacy]
            r `shouldSatisfy` T.isInfixOf "Excluded legacy context rows: 1"
            r `shouldSatisfy` (not . T.isInfixOf "mean cumulative bytes")
