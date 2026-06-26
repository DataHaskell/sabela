{-# LANGUAGE OverloadedStrings #-}

module Test.LibDiscoverSpec (spec) where

import Data.Text (Text)
import Sabela.Discover (LibSuggestion (..), cabalLine, discoverLibraries)
import Test.Hspec

topPackages :: Text -> [Text]
topPackages q = case discoverLibraries q of
    (s : _) -> lsPackages s
    [] -> []

spec :: Spec
spec = describe "Sabela.Discover.discoverLibraries" $ do
    it "routes a regression task to dataframe-learn" $
        topPackages "train a small linear regression model"
            `shouldContain` ["dataframe-learn"]

    it "routes a csv/load task to dataframe" $
        topPackages "read the housing csv dataset" `shouldContain` ["dataframe"]

    it "routes a plotting task to granite" $
        topPackages "plot a bar chart of revenue" `shouldContain` ["granite"]

    it "ranks by keyword overlap (regression beats a bare dataframe mention)" $
        topPackages "fit a logistic regression on the dataframe"
            `shouldContain` ["dataframe-learn"]

    it "a type-safe column query routes to the compile-checked dataframe-th stack" $
        case discoverLibraries "type-safe column names in a dataframe" of
            (s : _) -> do
                lsPackages s `shouldContain` ["dataframe-th"]
                ("DataFrame.TH.Records" `elem` lsModules s) `shouldBe` True
            [] -> expectationFailure "expected a dataframe suggestion"

    it "returns nothing for an unrelated query" $
        discoverLibraries "quantum teleportation schedule" `shouldBe` []

    it "renders a ready -- cabal: line for the top hit" $
        (cabalLine <$> take 1 (discoverLibraries "linear regression"))
            `shouldBe` ["-- cabal: build-depends: dataframe, dataframe-learn"]
