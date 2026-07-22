{-# LANGUAGE OverloadedStrings #-}

{- | R7-T3 seam tests: check_type distils at the emitting seam. The
run-181440 defect (recordDecl false-positives on a GHC JSON error, riding
the blob back after a correct signature) is unrepresentable.
-}
module Test.QueryDistillSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Capabilities.Query (recordDecl, typeConstructors)
import Sabela.AI.VerifierDistill (answerVerdict, distillInfo, distillTypeAnswer)

-- | The exact GHC 9.12 diagnostics-as-JSON error the run-181440 blob carried.
ghcJsonError :: Text
ghcJsonError =
    "{\"version\":\"1.1\",\"ghcVersion\":\"ghc-9.12.2\",\"span\":{\"file\":\
    \\"<interactive>\"},\"severity\":\"Error\",\"code\":76037,\"message\":\
    \[\"Not in scope: data constructor `Columnable'\"],\"hints\":[]}"

spec :: Spec
spec = describe "check_type distillation at the emitting seam (R7-T3)" $ do
    describe "recordDecl never accepts leak-shaped text" $ do
        it "the GHC JSON error is NOT a record declaration (run-181440 bug)" $
            recordDecl ghcJsonError `shouldBe` Nothing
        it "a real record declaration still passes" $
            recordDecl
                ( T.unlines
                    [ "data TreeConfig"
                    , "  = TreeConfig {maxTreeDepth :: Int}"
                    ]
                )
                `shouldNotBe` Nothing

    describe "distillInfo scrubs the :info channel before recordDecl" $ do
        it "reduces the JSON error to nothing a recordDecl could keep" $
            recordDecl (distillInfo ghcJsonError) `shouldBe` Nothing
        it "keeps a legitimate declaration, minus provenance lines" $ do
            let info =
                    T.unlines
                        [ "data TreeConfig = TreeConfig {maxTreeDepth :: Int}"
                        , "  -- Defined in \8216DataFrame.DecisionTree.Types\8217"
                        ]
            distillInfo info `shouldSatisfy` T.isInfixOf "maxTreeDepth"
            distillInfo info `shouldSatisfy` (not . T.isInfixOf "Defined in")

    describe "the composed check_type answer" $ do
        it "signature + trailing blob distills to the signature alone" $
            distillTypeAnswer
                ( "D.sum :: (D.Columnable a, Num a) => D.Expr a -> \
                  \D.DataFrame -> a\n\n"
                    <> ghcJsonError
                )
                `shouldBe` "D.sum :: (D.Columnable a, Num a) => D.Expr a -> D.DataFrame -> a"
        it "a legitimate record append survives distillation" $ do
            let out =
                    distillTypeAnswer
                        "cfg :: TreeConfig\n\ndata TreeConfig = TreeConfig \
                        \{maxTreeDepth :: Int}"
            out `shouldSatisfy` T.isInfixOf "cfg :: TreeConfig"
            out `shouldSatisfy` T.isInfixOf "maxTreeDepth"
        it "typeConstructors still reads the distilled signature" $
            typeConstructors (distillTypeAnswer "x :: Maybe Int\n\n{\"v\":1}")
                `shouldBe` ["Maybe", "Int"]

    describe "verdicts are total on the distilled surface (5.3)" $ do
        it "a clean signature is ok" $
            answerVerdict "sum :: Num a => [a] -> a" `shouldBe` "ok"
        it "a distilled diagnostic is diagnostic" $
            answerVerdict (distillTypeAnswer ghcJsonError)
                `shouldBe` "diagnostic"
        it "emptiness is could-not-run, never a silent pass" $
            answerVerdict "  " `shouldBe` "could-not-run"
