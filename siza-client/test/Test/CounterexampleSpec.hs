{-# LANGUAGE OverloadedStrings #-}

{- | Intention specs for the counterexample-naming verify re-prompt.

Measured live (evalExpr gate 6): gemma's cells all ran, her own output showed
@10 / 4 - 1 = Nothing@, the verify rail said only \"re-read the task's
examples\" — and she declared success twice and tapped out. The re-prompt must
NAME the failing example (and the wrong value when it can be probed), so a
giving-up model is shown the exact contradiction.
-}
module Test.CounterexampleSpec (counterexampleSpec) where

import qualified Data.ByteString.Lazy as BL
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Test.Hspec

import Data.Aeson (Value (..), decode)
import Sabela.AI.Capabilities.ToolName (ToolName (..))
import Sabela.AI.Types (ToolOutcome (..))
import Siza.Agent.Check (
    caseScrutinee,
    ceMarkerSrc,
    checkVerdictWith,
    conjuncts,
    counterexampleLine,
    eqLhs,
    parseCeIndex,
    probeExpr,
 )
import Siza.Agent.Messages (verifyMessage')

-- | The real evalExpr gate check, verbatim from the corpus.
evalExprCheck :: Text
evalExprCheck =
    "evalExpr \"2 + 3 * 4\" == Just 14.0 && evalExpr \"(2 + 3) * 4\" == Just 20.0 \
    \&& (case evalExpr \"10 / 4 - 1\" of { Just v -> abs (v - 1.5) < 1e-9; Nothing -> False }) \
    \&& evalExpr \"2 +\" == Nothing"

counterexampleSpec :: Spec
counterexampleSpec = describe "counterexample-naming verify (intention)" $ do
    describe "conjuncts — top-level && split" $ do
        it "splits the real evalExpr check into its four examples" $
            conjuncts evalExprCheck
                `shouldBe` [ "evalExpr \"2 + 3 * 4\" == Just 14.0"
                           , "evalExpr \"(2 + 3) * 4\" == Just 20.0"
                           , "(case evalExpr \"10 / 4 - 1\" of { Just v -> abs (v - 1.5) < 1e-9; Nothing -> False })"
                           , "evalExpr \"2 +\" == Nothing"
                           ]
        it "does not split on && inside parens" $
            conjuncts "all (\\x -> x > 0 && x < 9) xs && null ys"
                `shouldBe` ["all (\\x -> x > 0 && x < 9) xs", "null ys"]
        it "does not split on && inside a string literal" $
            conjuncts "label == \"a && b\" && n == 1"
                `shouldBe` ["label == \"a && b\"", "n == 1"]
        it "a conjunction-free check is a single conjunct" $
            conjuncts "abs (revenueTotal - 600) < 0.001"
                `shouldBe` ["abs (revenueTotal - 600) < 0.001"]

    describe "eqLhs — the probe-able side of an == example" $ do
        it "takes the LHS of a top-level ==" $
            eqLhs "evalExpr \"10 / 4 - 1\" == Just 1.5"
                `shouldBe` Just "evalExpr \"10 / 4 - 1\""
        it "ignores == inside brackets (case-of conjunct has no probe)" $
            eqLhs
                "(case evalExpr \"x\" of { Just v -> v == 1.5; Nothing -> False })"
                `shouldBe` Nothing
        it "ignores == inside a string" $
            eqLhs "label /= \"a == b\"" `shouldBe` Nothing

    describe "caseScrutinee — probe the scrutinee of a case-shaped example" $ do
        it "extracts the scrutinee from the real evalExpr tolerance conjunct" $
            caseScrutinee
                "(case evalExpr \"10 / 4 - 1\" of { Just v -> abs (v - 1.5) < 1e-9; Nothing -> False })"
                `shouldBe` Just "evalExpr \"10 / 4 - 1\""
        it "is Nothing for a non-case conjunct" $
            caseScrutinee "evalExpr \"2 +\" == Nothing" `shouldBe` Nothing
        it "probeExpr prefers == LHS and falls back to the scrutinee" $ do
            probeExpr "f 1 == Just 2" `shouldBe` Just "f 1"
            probeExpr "(case f 1 of { Just v -> v > 0; Nothing -> False })"
                `shouldBe` Just "f 1"
            probeExpr "null xs" `shouldBe` Nothing

    describe "ceMarkerSrc — one marker names the first failing example" $ do
        it "embeds every conjunct and a CE_NONE fallback" $ do
            let src = ceMarkerSrc (conjuncts evalExprCheck)
            src `shouldSatisfy` T.isInfixOf "CE_NONE"
            src `shouldSatisfy` T.isInfixOf "evalExpr \"2 +\" == Nothing"
            src `shouldSatisfy` T.isInfixOf "zip [(0 :: Int) ..]"
        it "parses the failing index back out of the output" $ do
            parseCeIndex "\"CE_2\\n\"" `shouldBe` Just 2
            parseCeIndex "CE_0" `shouldBe` Just 0
            parseCeIndex "CE_NONE\n" `shouldBe` Nothing
            parseCeIndex "<interactive>:1:1: error: boom" `shouldBe` Nothing

    describe "counterexampleLine — the sentence the model sees" $ do
        it "names the example and the computed value" $ do
            let l =
                    counterexampleLine
                        "evalExpr \"10 / 4 - 1\" == Just 1.5"
                        (Just "Nothing")
            l `shouldSatisfy` T.isInfixOf "`evalExpr \"10 / 4 - 1\" == Just 1.5`"
            l `shouldSatisfy` T.isInfixOf "`Nothing`"
        it "still names the example when no value could be probed" $ do
            let l = counterexampleLine "(case f 1 of { _ -> False })" Nothing
            l `shouldSatisfy` T.isInfixOf "(case f 1 of { _ -> False })"

    describe "checkVerdictWith — verdict + counterexample in one call" $ do
        it "a passing check carries no counterexample" $ do
            call <- scriptedCaller ["GRADE_PASS"]
            checkVerdictWith call evalExprCheck
                >>= (`shouldBe` (True, Nothing))
        it "a failing check names the first failing example with its value" $ do
            call <- scriptedCaller ["GRADE_FAIL", "CE_0", "Nothing"]
            (ok, mCe) <- checkVerdictWith call evalExprCheck
            ok `shouldBe` False
            mCe `shouldSatisfy` maybe False (T.isInfixOf "2 + 3 * 4")
            mCe `shouldSatisfy` maybe False (T.isInfixOf "`Nothing`")
        it "a case-shaped conjunct probes its scrutinee for the value" $ do
            call <- scriptedCaller ["GRADE_FAIL", "CE_2", "Nothing"]
            (ok, mCe) <- checkVerdictWith call evalExprCheck
            ok `shouldBe` False
            mCe `shouldSatisfy` maybe False (T.isInfixOf "10 / 4 - 1")
            mCe `shouldSatisfy` maybe False (T.isInfixOf "`Nothing`")
        it "extracts the bare printed value from the execute-cell envelope" $ do
            -- Measured live (gate 8): the raw tool JSON leaked into the line.
            let envelope =
                    "{\"cellId\":4,\"ok\":true,\"outcome\":{\"tag\":\"Succeeded\"},\
                    \\"outputs\":[{\"oiMime\":\"text/plain\",\"oiOutput\":\"Nothing\\n\"}],\
                    \\"warnings\":[]}"
            call <- scriptedCaller ["GRADE_FAIL", "CE_2", envelope]
            (_, mCe) <- checkVerdictWith call evalExprCheck
            mCe `shouldSatisfy` maybe False (T.isInfixOf "computes `Nothing`")
            mCe `shouldSatisfy` maybe False (not . T.isInfixOf "cellId")
        it "an unparseable ce output degrades to no counterexample" $ do
            call <- scriptedCaller ["GRADE_FAIL", "error: boom"]
            checkVerdictWith call evalExprCheck
                >>= (`shouldBe` (False, Nothing))

    describe "verifyMessage' carries the counterexample" $ do
        it "shows the failing example instead of the generic sentence" $ do
            let msg =
                    verifyMessage'
                        2
                        []
                        ( Just
                            "This required example fails: `evalExpr \"10 / 4 - 1\" == Just 1.5` — your code computes `Nothing`."
                        )
            msg `shouldSatisfy` T.isInfixOf "10 / 4 - 1"
            msg `shouldSatisfy` (not . T.isInfixOf "re-read the task's examples")
        it "a missing deliverable still takes precedence" $ do
            let msg = verifyMessage' 2 ["evalExpr"] (Just "This required example fails: `x == 1`.")
            msg `shouldSatisfy` T.isInfixOf "not defined"

{- | A tool caller that answers every ExecuteCell with the next scripted
output (JSON envelopes decode as production does); inserts/lists/deletes are
acknowledged inertly.
-}
scriptedCaller ::
    [Text] -> IO (ToolName -> Value -> IO (Either Text ToolOutcome))
scriptedCaller outs = do
    ref <- newIORef outs
    pure $ \tn _ -> case tn of
        ExecuteCell -> do
            xs <- readIORef ref
            case xs of
                (x : rest) -> do
                    modifyIORef' ref (const rest)
                    pure (Right (ToolOk (asValue x)))
                [] -> pure (Right (ToolOk (String "")))
        _ -> pure (Right (ToolOk (String "ok")))
  where
    asValue x =
        fromMaybe (String x) (decode (BL.fromStrict (encodeUtf8 x)))
