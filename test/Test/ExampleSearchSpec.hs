{-# LANGUAGE OverloadedStrings #-}

module Test.ExampleSearchSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.Examples (Example (..), exampleIndex, searchExamples)
import Test.Hspec

topTitle :: Text -> Text
topTitle q = case searchExamples q of
    (e : _) -> exTitle e
    [] -> ""

spec :: Spec
spec = describe "Sabela.AI.Examples.searchExamples (shape idioms only)" $ do
    it "routes 'read csv' to the dataframe reader" $
        topTitle "read csv" `shouldBe` "Read a CSV into a DataFrame"

    it
        "routes a column-total task to the typed example, which uses the declareColumns TH splice"
        $ case searchExamples "sum a numeric column" of
            (e : _) -> do
                exTitle e
                    `shouldBe` "Total a DataFrame column with compile-checked names (typed)"
                ("declareColumns" `T.isInfixOf` exCode e) `shouldBe` True
                ("dataframe-th" `T.isInfixOf` exCode e) `shouldBe` True
                ("$(" `T.isInfixOf` exCode e) `shouldBe` True
            [] -> expectationFailure "expected a typed column example"

    it "returns nothing for a plotting query (granite bars retired)" $
        searchExamples "plotting" `shouldBe` []

    it
        "returns nothing for a regression query (the linear/logistic mis-tag is gone)"
        $ searchExamples "linear regression" `shouldBe` []

    it "returns nothing for parquet or cassava (retired alternate loaders)" $ do
        searchExamples "parquet" `shouldBe` []
        searchExamples "cassava" `shouldBe` []

    it "returns nothing for an unrelated query" $
        searchExamples "xyzzy unrelated nonsense" `shouldBe` []

    it "every example is a runnable cell beginning with a -- cabal: line" $
        all (("-- cabal:" `T.isPrefixOf`) . firstLine . exCode) exampleIndex
            `shouldBe` True
  where
    firstLine = T.takeWhile (/= '\n')
