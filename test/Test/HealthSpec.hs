{-# LANGUAGE OverloadedStrings #-}

{- | The diagnostic-set health model that decides whether a repair candidate is
kept. Pins the acceptance rule that comparing SETS (not error counts) rejects a
candidate that trades one error for another.
-}
module Test.HealthSpec (spec) where

import qualified Data.Set as Set
import Data.Text (Text)
import Test.Hspec

import Sabela.AI.Health (
    Health (..),
    healthOfResult,
    healthOfTypeQuery,
    improvesHealth,
    isClean,
    normalizeMsg,
 )
import Sabela.AI.Types (ExecutionResult (..))
import Sabela.Model (CellError (..))

cellErr :: Text -> CellError
cellErr = CellError Nothing Nothing

result :: Maybe Text -> [CellError] -> Either Text ExecutionResult
result err errs = Right (ExecutionResult [] err errs [])

spec :: Spec
spec = describe "Sabela.AI.Health" $ do
    describe "healthOfResult" $ do
        it "a Left is not clean and carries a sentinel diagnostic" $ do
            let h = healthOfResult (Left "Cancelled")
            healthCompileOk h `shouldBe` False
            Set.size (healthDiagnostics h) `shouldBe` 1
        it "a clean Right compiles ok with no diagnostics" $ do
            let h = healthOfResult (result Nothing [])
            isClean h `shouldBe` True
        it "folds the holistic error plus structured errors, excluding warnings" $ do
            let er = ExecutionResult [] (Just "boom") [cellErr "a", cellErr "b"] [cellErr "w"]
                h = healthOfResult (Right er)
            healthCompileOk h `shouldBe` False
            Set.size (healthDiagnostics h) `shouldBe` 3

    describe "improvesHealth" $ do
        it "keeps a candidate that removes an error and adds none" $
            improvesHealth
                (healthOfResult (result Nothing [cellErr "a", cellErr "b"]))
                (healthOfResult (result Nothing [cellErr "a"]))
                `shouldBe` True
        it "keeps a candidate that compiles clean" $
            improvesHealth
                (healthOfResult (result Nothing [cellErr "a"]))
                (healthOfResult (result Nothing []))
                `shouldBe` True
        it "REJECTS trading one error for a different one (count would accept)" $
            improvesHealth
                (healthOfResult (result Nothing [cellErr "a", cellErr "b"]))
                (healthOfResult (result Nothing [cellErr "a", cellErr "c"]))
                `shouldBe` False
        it "rejects an unchanged non-clean result" $
            improvesHealth
                (healthOfResult (result Nothing [cellErr "a"]))
                (healthOfResult (result Nothing [cellErr "a"]))
                `shouldBe` False
        it "rejects a regression to an abort (Left)" $
            improvesHealth
                (healthOfResult (result Nothing [cellErr "a"]))
                (healthOfResult (Left "Cancelled"))
                `shouldBe` False

    describe "healthOfTypeQuery" $ do
        it "treats a returned type signature as clean" $
            isClean (healthOfTypeQuery "foldr (+) 0 :: (Foldable t, Num b) => t b -> b")
                `shouldBe` True
        it "treats a structured json diagnostic as red" $
            isClean
                ( healthOfTypeQuery
                    "{\"severity\":\"Error\",\"message\":[\"No instance for IsString Int\"]}"
                )
                `shouldBe` False
        it "treats a textual error: as red" $
            isClean (healthOfTypeQuery "<interactive>:1:1: error: not in scope: foo")
                `shouldBe` False

    describe "normalizeMsg" $
        it "collapses whitespace so reflowed messages compare equal" $
            normalizeMsg "not   in\nscope" `shouldBe` normalizeMsg "not in scope"
