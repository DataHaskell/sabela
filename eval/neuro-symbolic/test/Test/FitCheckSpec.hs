{-# LANGUAGE OverloadedStrings #-}

module Test.FitCheckSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Eval.FitCheck (
    FitOutcome (..),
    evalAt,
    extractCandidates,
    extractExpression,
    fitOutcome,
    fitResult,
    fitsSample,
    parseExpr,
 )

-- | The symbolicRegression sample: y = x^2 on four points.
points :: [(Double, Double)]
points = [(1, 1), (2, 4), (3, 9), (4, 16)]

tol :: Double
tol = 1e-6

{- | The R5-T5 rendering grid: the SAME correct expression as tuple (quoted and
bare), markdown, plain prose, and alternate error wording (SSE vs total squared
error). Every rendering must extract and confirm.
-}
correctRenderings :: [Text]
correctRenderings =
    [ "Best expression: (x * x), total squared error: 0.0"
    , "Best expression: (x*x), SSE: 0.0"
    , "(\"x*x\", 0.0)"
    , "(x*x, 0.0)"
    , "**Best expression:** `x*x`, SSE 0"
    , "the best fit is x^2 with total squared error 0"
    ]

-- | The same grid with a genuinely wrong expression: every rendering refutes.
wrongRenderings :: [Text]
wrongRenderings =
    [ "Best expression: (x + 1), total squared error: 0.0"
    , "Best expression: (x+1), SSE: 0.0"
    , "(\"x+1\", 0.0)"
    , "(x+1, 0.0)"
    , "**Best expression:** `x+1`, SSE 0"
    ]

{- | Extraction-failure outcomes: no reported expression at all, probe errors,
and prose that merely echoes the task's grammar. None may refute.
-}
expressionFreeOutputs :: [Text]
expressionFreeOutputs =
    [ ""
    , "the computation finished"
    , "no expression could be printed"
    , "error: kernel timed out before printing"
    , "SSE: 4.2"
    , "search over simple arithmetic expressions built from x, +, *"
    ]

spec :: Spec
spec = describe "Eval.FitCheck (symbolic-regression covering verifier)" $ do
    describe "extractExpression" $ do
        it "takes the expression and drops the reported-error tail" $
            extractExpression "Best expression: (x*x), total squared error 0.0"
                `shouldBe` Just "(x*x)"
        it "handles an 'expression is …' phrasing" $
            extractExpression "best expression is x^2 with error 0"
                `shouldBe` Just "x^2"
        it "is Nothing when no expression is reported" $
            extractExpression "the total squared error was 4.0" `shouldBe` Nothing

    describe "extractCandidates (format-agnostic, boundary-checked)" $ do
        it "finds the expression in a quoted tuple rendering" $
            extractCandidates "(\"x*x\", 0.0)" `shouldSatisfy` elem "x*x"
        it "finds the expression in a bare tuple rendering" $
            extractCandidates "(x*x, 0.0)" `shouldSatisfy` elem "x*x"
        it "finds the expression in a markdown rendering" $
            extractCandidates "**Best expression:** `x*x`" `shouldSatisfy` elem "x*x"
        it "never harvests the x inside a prose word (\"expression\")" $
            extractCandidates "no expression could be printed"
                `shouldSatisfy` (not . any (T.any (== 'x')))

    describe "fitOutcome (three-valued: confirm / refute / not-yet-confirmed)" $ do
        it "confirms EVERY rendering of the correct expression (R5-T5 grid)" $
            mapM_
                (\r -> (r, isConfirmed (fitOutcome points tol r)) `shouldBe` (r, True))
                correctRenderings
        it "refutes EVERY rendering of a wrong expression, with recomputed error" $
            mapM_
                ( \r -> case fitOutcome points tol r of
                    FitRefuted err _ -> (r, err > 1) `shouldBe` (r, True)
                    o -> expectationFailure (show (r, o))
                )
                wrongRenderings
        it "routes every expression-free output to not-yet-confirmed, NEVER refuted" $
            mapM_
                ( \r -> case fitOutcome points tol r of
                    FitUnconfirmed _ -> pure ()
                    o -> expectationFailure (show (r, o))
                )
                expressionFreeOutputs
        it "a denial is emitted iff a recomputed nonzero error exists (grid law)" $
            mapM_
                ( \r -> case fitOutcome points tol r of
                    FitRefuted err e -> do
                        (r, err > tol) `shouldBe` (r, True)
                        -- the evidence names a real x-expression, recomputable
                        parseExpr e `shouldSatisfy` (/= Nothing)
                    _ -> pure ()
                )
                (correctRenderings <> wrongRenderings <> expressionFreeOutputs)
        it "confirmation dominates: a text carrying both a fit and a non-fit confirms" $
            fitOutcome points tol "tried (x+1) with SSE 148.0, best (x*x) with SSE 0.0"
                `shouldSatisfy` isConfirmed
        it "still refutes a wrong expression that self-reports zero error" $
            case fitOutcome points tol "Best expression: (x+1), total squared error: 0.0" of
                FitRefuted err _ -> err `shouldSatisfy` (> 1)
                o -> expectationFailure (show o)

    describe "fitsSample (recomputed over the sample, not the reported number)" $ do
        it "PASSES the known-correct trajectory output" $
            fitsSample points tol "Best expression: (x*x), total squared error 0.0"
                `shouldBe` True
        it "PASSES the run-20260720-085948 live wording (SSE)" $
            fitsSample points tol "Best expression: (x * x), SSE: 0.0"
                `shouldBe` True
        it "accepts an equivalent zero-error expression, not the literal string" $
            fitsSample points tol "Best expression: x^2, total squared error 0.0"
                `shouldBe` True
        it "accepts a spaced/summed equivalent (x*x written as x*x + 0)" $
            fitsSample points tol "Best expression: x * x + 0.0" `shouldBe` True
        it "FAILS a wrong expression even if it self-reports zero error" $
            fitsSample points tol "Best expression: (x+1), total squared error 0.0"
                `shouldBe` False
        it "FAILS a cubic that misses the quadratic sample" $
            fitsSample points tol "Best expression: x^3" `shouldBe` False
        it "does not surface when the output carries no expression" $
            fitsSample points tol "no answer here" `shouldBe` False

    describe "fitResult" $ do
        it "recomputes zero error for the correct expression" $
            fitResult points "Best expression: (x*x)" `shouldBe` Right 0
        it "recomputes the true positive error for a wrong expression" $
            case fitResult points "Best expression: (x+1)" of
                Right e -> e `shouldSatisfy` (> 1)
                Left r -> expectationFailure ("unexpected Left: " <> show r)
        it "is Left when no parseable expression is present" $
            case fitResult points "the fit was great" of
                Left _ -> pure ()
                Right e -> expectationFailure ("unexpected Right: " <> show e)

    describe "parseExpr" $
        it "parses parenthesised and power forms to the same value at x" $ do
            (evalTwo <$> parseExpr "(x*x)") `shouldBe` Just (1, 16)
            (evalTwo <$> parseExpr "x^2") `shouldBe` Just (1, 16)
  where
    evalTwo e = (evalAt 1 e, evalAt 4 e)

isConfirmed :: FitOutcome -> Bool
isConfirmed (FitConfirmed _ _) = True
isConfirmed _ = False
