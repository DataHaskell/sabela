{-# LANGUAGE OverloadedStrings #-}

module Test.VerdictSurfaceSpec (verdictSurfaceSpec) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Verdict (VerdictClass (..), parseVerdict)
import Siza.Agent.Messages (
    VerifyFailure (..),
    doneSignalMsg,
    unconfirmedMsgWith,
    verifyFailureOf,
    verifyHeadline,
    verifyMessage',
    verifyMsgWith,
 )

content :: Value -> Text
content (Object o) = case KM.lookup (K.fromText "content") o of
    Just (String s) -> s
    _ -> ""
content _ = ""

verdictSurfaceSpec :: Spec
verdictSurfaceSpec = describe "verify-channel verdict totality (section 5.3)" $ do
    describe "check-verdict-contradiction (live_test8)" $ do
        it "classifies a no-cell state as a precondition, not a check failure" $
            verifyFailureOf 0 [] `shouldBe` NoDeliverable

        it "an undefined deliverable is its own class, not a check failure" $
            verifyFailureOf 2 ["sineWaveSvg"]
                `shouldBe` DeliverableUndefined ["sineWaveSvg"]

        it "only a written, defined deliverable can fail its check" $
            verifyFailureOf 2 [] `shouldBe` CheckFailed

        -- Was: this also asserted the message contains "not a check failure".
        -- That clause reads "the check has not run", which the routing
        -- contradicts — only a check that RAN and failed reaches this channel.
        -- The precondition-vs-logic distinction is kept; the false claim is not.
        it "the no-cell message never claims the check failed" $ do
            let msg = verifyMessage' 0 [] Nothing
            msg `shouldSatisfy` T.isInfixOf "no deliverable cell exists"
            msg `shouldNotSatisfy` T.isInfixOf "the deliverable's check still fails"

        it "no headline on the failed-check channel denies that the check ran" $
            [ h
            | c <- [NoDeliverable, DeliverableUndefined ["x"], CheckFailed]
            , let h = verifyHeadline c
            , any (`T.isInfixOf` h) ["has not run", "not a check failure"]
            ]
                `shouldBe` []

        it "a genuine check failure still says so plainly" $
            verifyMessage' 2 [] Nothing
                `shouldSatisfy` T.isInfixOf "the deliverable's check still fails"

        it "every class produces a distinct headline" $ do
            let hs =
                    map
                        verifyHeadline
                        [NoDeliverable, DeliverableUndefined ["x"], CheckFailed]
            length (nub hs) `shouldBe` 3

    it "the done signal decodes as ok" $
        parseVerdict (content (doneSignalMsg [1] "total == 42"))
            `shouldBe` Just VerdictOk
    it "every diagnostic re-prompt decodes as diagnostic, over the grid" $
        sequence_
            [ parseVerdict (content (verifyMsgWith owned missing ce))
                `shouldBe` Just VerdictDiagnostic
            | owned <- [0, 1, 2]
            , missing <- [[], ["evalExpr"]]
            , ce <- [Nothing, Just "This required example fails: `x == 1`."]
            ]
    it "every not-yet-confirmed re-prompt decodes as could-not-run, over the grid" $
        sequence_
            [ parseVerdict (content (unconfirmedMsgWith owned missing g))
                `shouldBe` Just VerdictCouldNotRun
            | owned <- [0, 1, 2]
            , missing <- [[], ["evalExpr"]]
            , g <- [Nothing, Just "print the best expression"]
            ]
    it "no verify-channel producer is undecodable (silence unrepresentable)" $
        mapM_
            (\v -> parseVerdict (content v) `shouldSatisfy` (/= Nothing))
            [ doneSignalMsg [1] "total == 42"
            , verifyMsgWith 0 [] Nothing
            , unconfirmedMsgWith 0 [] Nothing
            ]
    it "a transport-swallowed marker answer decodes as infra, never a pass" $ do
        let swallowed =
                "[infra] no response within 300s. The server is likely STILL \
                \WORKING; a write may have landed."
        parseVerdict swallowed `shouldBe` Just VerdictInfra
