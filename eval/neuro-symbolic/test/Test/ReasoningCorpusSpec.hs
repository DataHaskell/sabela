{-# LANGUAGE OverloadedStrings #-}

module Test.ReasoningCorpusSpec (spec) where

import Data.List (nub)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Test.Hspec

import qualified Eval.Corpus as Corpus
import Eval.Corpus.Reasoning (
    Category (..),
    reasoningCorpus,
    reasoningTasks,
    selectReasoning,
 )
import Eval.Task (Task (..), taskTest)

ids :: [Task] -> [T.Text]
ids = map taskId

categoryOf :: Category -> [Task]
categoryOf c = [t | (k, t) <- reasoningCorpus, k == c]

-- | The ByValue check string of a named task (empty if absent / not ByValue).
checkOf :: T.Text -> T.Text
checkOf tid =
    case [taskTest t | t <- reasoningTasks, taskId t == tid] of
        (Just c : _) -> c
        _ -> ""

spec :: Spec
spec = describe "Eval.Corpus.Reasoning" $ do
    it "has fourteen tasks with unique ids" $ do
        length reasoningCorpus `shouldBe` 14
        let allIds = ids reasoningTasks
        nub allIds `shouldBe` allIds

    it "splits 6 competition / 4 design / 4 logic" $ do
        length (categoryOf Competition) `shouldBe` 6
        length (categoryOf Design) `shouldBe` 4
        length (categoryOf Logic) `shouldBe` 4

    it "every task is graded by an objective ByValue check" $
        length (mapMaybe taskTest reasoningTasks) `shouldBe` 14

    it "selectReasoning filters by category name (case-insensitive)" $ do
        ids (selectReasoning (Just "Competition"))
            `shouldBe` ids (categoryOf Competition)
        ids (selectReasoning (Just " logic "))
            `shouldBe` ids (categoryOf Logic)

    it "selectReasoning yields the whole corpus by default" $ do
        length (selectReasoning Nothing) `shouldBe` 14
        length (selectReasoning (Just "all")) `shouldBe` 14

    it "the gate's SIZA_GATE_FOLD=reasoning selects exactly the reasoning corpus" $
        ids (Corpus.selectFold (Just "reasoning")) `shouldBe` ids reasoningTasks

    it "pins the live-validated competition check values" $ do
        checkOf "queens"
            `shouldBe` "queens 6 == 4 && queens 8 == 92 && queens 1 == 1 && queens 4 == 2"
        checkOf "countPrimes"
            `shouldBe` "countPrimes 100 == 25 && countPrimes 10 == 4 \
                       \&& countPrimes 2 == 0 && countPrimes 0 == 0"

    it "pins the live-validated logic answers" $ do
        checkOf "fishOwner" `shouldBe` "fishOwner == \"Dane\""
        checkOf "minPours" `shouldBe` "minPours == 6"
        checkOf "seating" `shouldBe` "seating == [\"A\", \"C\", \"B\", \"D\"]"
