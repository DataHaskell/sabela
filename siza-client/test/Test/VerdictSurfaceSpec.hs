{-# LANGUAGE OverloadedStrings #-}

{- | R6-T4 verdict totality on the verify channel (search-api.md section 5.3):
every verify-channel producer renders a decodable member of the closed verdict
vocabulary, matched to its outcome class — silence is undecodable, so a
marker-less answer is a lint issue, and a transport-swallowed marker-run
answer decodes as the no-verdict infra class, never a pass.
-}
module Test.VerdictSurfaceSpec (verdictSurfaceSpec) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import Test.Hspec

import Sabela.AI.Verdict (VerdictClass (..), parseVerdict)
import Siza.Agent.Messages (
    doneSignalMsg,
    unconfirmedMsgWith,
    verifyMsg,
    verifyMsgWith,
 )

content :: Value -> Text
content (Object o) = case KM.lookup (K.fromText "content") o of
    Just (String s) -> s
    _ -> ""
content _ = ""

verdictSurfaceSpec :: Spec
verdictSurfaceSpec = describe "verify-channel verdict totality (section 5.3)" $ do
    it "the done signal decodes as ok" $
        parseVerdict (content doneSignalMsg) `shouldBe` Just VerdictOk
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
    it "the generic keep-working re-prompt decodes as diagnostic" $
        parseVerdict (content verifyMsg) `shouldBe` Just VerdictDiagnostic
    it "no verify-channel producer is undecodable (silence unrepresentable)" $
        mapM_
            (\v -> parseVerdict (content v) `shouldSatisfy` (/= Nothing))
            [ doneSignalMsg
            , verifyMsg
            , verifyMsgWith 0 [] Nothing
            , unconfirmedMsgWith 0 [] Nothing
            ]
    it "a transport-swallowed marker answer decodes as infra, never a pass" $ do
        let swallowed =
                "[infra] no response within 300s. The server is likely STILL \
                \WORKING; a write may have landed."
        parseVerdict swallowed `shouldBe` Just VerdictInfra
