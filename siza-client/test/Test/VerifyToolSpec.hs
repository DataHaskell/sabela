{-# LANGUAGE OverloadedStrings #-}

{- | @verify@: a claim run against the live kernel in a scratch cell.
It is client-side, like discover — the server grows no new capability.
-}
module Test.VerifyToolSpec (verifyToolSpec) where

import Data.Aeson (object, (.=))
import Data.IORef (readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Sabela.AI.Capabilities.ToolName (ToolName (..), toolWireName)
import Sabela.AI.Types (toolOutcomeIsError)
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.ToolRoute (Route (..), routeCall)
import Siza.Agent.Tools (offeredNames)
import Siza.Agent.VerifyTool (runVerifyCall)
import Test.VerifyToolFixtures (
    Scenario (..),
    claimsValue,
    count,
    fieldOf,
    genDegenerateCheck,
    genDegenerateFalse,
    genDegenerateTrue,
    genScenario,
    kernel,
    refusingKernel,
    scVerdictReachable,
    scenarioKernel,
    verdictOf,
 )

verifyToolSpec :: Spec
verifyToolSpec = describe "the verify tool" $ do
    it "is on the offered surface" $
        offeredNames `shouldSatisfy` elem "verify"

    it "routes client-side, like discover" $
        case routeCall (ToolCall "verify" (object ["check" .= ("x == 1" :: Text)])) of
            RouteVerify chk _ -> chk `shouldBe` "x == 1"
            other -> expectationFailure ("expected RouteVerify, got " <> show other)

    it "passes a check the kernel confirms" $ do
        (call, _) <- kernel "total == 42" "GRADE_PASS"
        out <- runVerifyCall call "total == 42"
        verdictOf out `shouldBe` "pass"
        toolOutcomeIsError out `shouldBe` False

    it "fails with the conjunct that failed and the value computed" $ do
        (call, _) <- kernel "total == 42" "GRADE_FAIL"
        out <- runVerifyCall call "total == 42"
        verdictOf out `shouldBe` "fail"
        toolOutcomeIsError out `shouldBe` True
        fieldOf "counterexample" out `shouldSatisfy` T.isInfixOf "total == 42"

    it "discards a check that cannot fail rather than calling it a pass" $ do
        (call, _) <- kernel "True" "GRADE_PASS"
        out <- runVerifyCall call "True"
        verdictOf out `shouldBe` "not_applicable"

    it "runs nothing at all for an empty check" $ do
        (call, tape) <- kernel "total == 42" "GRADE_PASS"
        out <- runVerifyCall call "   "
        verdictOf out `shouldBe` "not_applicable"
        readIORef tape >>= \seen -> seen `shouldBe` []

    it "touches no cell at all when its scratch cell is refused" $ do
        (call, tape) <- refusingKernel
        out <- runVerifyCall call "total == 42"
        verdictOf out `shouldBe` "uncheckable"
        seen <- readIORef tape
        seen `shouldSatisfy` notElem (toolWireName DeleteCell)
        seen `shouldSatisfy` notElem (toolWireName ExecuteCell)

    it "cleans up after itself: every marker cell it inserts, it deletes" $ do
        (call, tape) <- kernel "total == 42" "GRADE_PASS"
        _ <- runVerifyCall call "total == 42"
        seen <- readIORef tape
        count InsertCell seen `shouldBe` count DeleteCell seen

    describe "C3-5: a verdict the tool could not reach is an error" $ do
        it "answers ok for the verdict pass and for no other" $
            property $
                forAll genScenario $ \sc -> ioProperty $ do
                    call <- scenarioKernel sc
                    out <- runVerifyCall call (scCheck sc)
                    pure
                        ( not (toolOutcomeIsError out)
                            `shouldBe` (verdictOf out == "pass")
                        )

        it "answers a check it never evaluated without claiming its value" $
            property $
                forAll genDegenerateCheck $ \chk -> ioProperty $ do
                    (call, tape) <- kernel "total == 42" "GRADE_PASS"
                    out <- runVerifyCall call chk
                    seen <- readIORef tape
                    pure $ do
                        verdictOf out `shouldBe` "not_applicable"
                        fieldOf "note" out `shouldSatisfy` (not . claimsValue)
                        seen `shouldBe` []

        it "says the same of a degenerate check whichever way it would go" $
            property $
                forAll ((,) <$> genDegenerateTrue <*> genDegenerateFalse) $
                    \(t, f) -> ioProperty $ do
                        (call, _) <- kernel "total == 42" "GRADE_PASS"
                        outT <- runVerifyCall call t
                        outF <- runVerifyCall call f
                        pure (fieldOf "note" outT `shouldBe` fieldOf "note" outF)

        it "reaches no verdict at all for every check that cannot carry one" $
            property $
                forAll (genScenario `suchThat` (not . scVerdictReachable)) $
                    \sc -> ioProperty $ do
                        call <- scenarioKernel sc
                        out <- runVerifyCall call (scCheck sc)
                        pure
                            ( verdictOf out
                                `shouldSatisfy` (`elem` ["not_applicable", "uncheckable"])
                            )

