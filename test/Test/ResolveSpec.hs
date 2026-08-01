{-# LANGUAGE OverloadedStrings #-}

module Test.ResolveSpec (spec) where

import Sabela.AI.Capabilities.Resolve (lookupByName, resolutionImport)
import Sabela.AI.Capability (Capability (..))
import Test.Hspec

caps :: [Capability]
caps =
    [ Capability "DataFrame" "readCsv" "FilePath -> IO DataFrame" Nothing
    , Capability "DataFrame" "columnAsList" "Text -> DataFrame -> [a]" Nothing
    , Capability "Other.Mod" "readCsv" "X -> Y" Nothing
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
        it "builds the import line and resolves the package from the store" $ do
            (line, pkg) <-
                resolutionImport
                    (Capability "Data.Text" "pack" "String -> Text" Nothing)
            line `shouldBe` "import Data.Text"
            pkg `shouldBe` Just "text"

        it "yields no package for a module nothing installed exposes" $ do
            (line, pkg) <-
                resolutionImport (Capability "Some.Random.Mod" "foo" "X" Nothing)
            line `shouldBe` "import Some.Random.Mod"
            pkg `shouldBe` Nothing
