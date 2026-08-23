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

    it "routes a plotting query to the drawing that ships with every notebook" $ do
        topTitle "plotting" `shouldBe` "Draw a bar chart of labelled values"
        topTitle "bar chart" `shouldBe` "Draw a bar chart of labelled values"
        topTitle "histogram" `shouldBe` "Draw points, a line, or a distribution"

    it "routes an interactive query to a widget" $ do
        topTitle "widget with a button"
            `shouldBe` "A widget with a button that counts presses"
        ("mkWidget" `T.isInfixOf` exCode (head (searchExamples "widget")))
            `shouldBe` True

    it
        "returns nothing for a regression query (the linear/logistic mis-tag is gone)"
        $ searchExamples "linear regression" `shouldBe` []

    it "returns nothing for parquet or cassava (retired alternate loaders)" $ do
        searchExamples "parquet" `shouldBe` []
        searchExamples "cassava" `shouldBe` []

    it "returns nothing for an unrelated query" $
        searchExamples "xyzzy unrelated nonsense" `shouldBe` []

    it "every example is a cell that runs as it stands" $
        all (paste . firstLine . exCode) exampleIndex `shouldBe` True
  where
    firstLine = T.takeWhile (/= '\n')
    -- Either it declares what it needs, or it imports what already ships with
    -- the notebook.
    paste l = "-- cabal:" `T.isPrefixOf` l || "import Sabela.Notebook" `T.isPrefixOf` l
