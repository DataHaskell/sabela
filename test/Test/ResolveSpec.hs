{-# LANGUAGE OverloadedStrings #-}

{- | The pure core of Phase-0 name→module resolution: exact-name lookup over a
capability index, and the import + package a resolution needs. The browse half is
verified live; these pin the decisions the resolve tool makes.
-}
module Test.ResolveSpec (spec) where

import Sabela.AI.Capabilities.Resolve (lookupByName, resolutionImport)
import Sabela.AI.Capability (Capability (..))
import Test.Hspec

caps :: [Capability]
caps =
    [ Capability "DataFrame" "readCsv" "FilePath -> IO DataFrame"
    , Capability "DataFrame" "columnAsList" "Text -> DataFrame -> [a]"
    , Capability "Other.Mod" "readCsv" "X -> Y"
    ]

spec :: Spec
spec = describe "Sabela.AI.Capabilities.Resolve" $ do
    describe "lookupByName" $ do
        it "returns every module that exports the exact name" $
            map capModule (lookupByName "readCsv" caps)
                `shouldBe` ["DataFrame", "Other.Mod"]

        it "matches whole names only, not substrings" $
            lookupByName "readCs" caps `shouldBe` []

        it "is empty for an unknown name" $
            lookupByName "nope" caps `shouldBe` []

    describe "resolutionImport" $ do
        it "builds the import line and resolves the package via the curated table" $
            resolutionImport (Capability "DataFrame" "readCsv" "FilePath -> IO DataFrame")
                `shouldBe` ("import DataFrame", Just "dataframe-core")

        it "yields no package for an uncurated module (dep fixer backstops at runtime)" $
            resolutionImport (Capability "Some.Random.Mod" "foo" "X")
                `shouldBe` ("import Some.Random.Mod", Nothing)
