{-# LANGUAGE OverloadedStrings #-}

module Test.PackageIndexSpec (spec) where

import Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import Test.Hspec

import Sabela.AI.Capabilities.ModuleCard (candidateModules, packageCardOf)
import Sabela.AI.Capabilities.ModuleSearch (usableCard)
import Sabela.AI.ModuleResolve (closestModules)
import Sabela.AI.PackageIndex (
    PackageEntry (..),
    modulesMatching,
    modulesOfPackage,
    packagesExposingModule,
    parsePackageDump,
 )

dump :: Text
dump =
    "name:                 dataframe\n\
    \version:              2.3.0.0\n\
    \exposed:              True\n\
    \exposed-modules:\n\
    \    DataFrame, DataFrame.Operations.Statistics,\n\
    \    DataFrame.Display from dtfrm-vz-1.0.3.0-60b765f1:DataFrame.Display\n\
    \depends:              base-4.21.0.0\n\
    \---\n\
    \name:                 granite\n\
    \version:              0.7.3.0\n\
    \exposed:              True\n\
    \exposed-modules:\n\
    \    Granite.Svg\n\
    \---\n\
    \name:                 hspec\n\
    \version:              2.11.17\n\
    \exposed:              True\n\
    \exposed-modules:\n\
    \    Test.Hspec Test.Hspec.Discover Test.Hspec.Formatters\n\
    \    Test.Hspec.QuickCheck Test.Hspec.Runner\n\
    \hidden-modules:       Paths_hspec\n"

spec :: Spec
spec = do
    packageCardSpec
    usableCardSpec
    nearSpellingSpec
    componentSpec
    indexSpec

componentSpec :: Spec
componentSpec = describe "a module component resolves to its module" $ do
    let pool =
            [ "Graphics.Hgg"
            , "Graphics.Hgg.DAG"
            , "Graphics.Hgg.DAG.Internal.Sugiyama"
            , "Granite.Svg"
            , "DataFrame"
            , "DataFrame.Internal.Statistics"
            , "DataFrame.Operations.Statistics"
            ]
    it "resolves Hgg to Graphics.Hgg" $
        candidateModules 1 "Hgg" pool `shouldBe` ["Graphics.Hgg"]
    it "prefers the module NAMED by the component over one merely carrying it" $
        candidateModules 1 "Svg" pool `shouldBe` ["Granite.Svg"]
    it "ranks a public component match ahead of an Internal one" $
        take 1 (candidateModules 3 "Statistics" pool)
            `shouldBe` ["DataFrame.Operations.Statistics"]
    it "still resolves a near spelling when no component matches" $
        candidateModules 1 "Data.Frame" pool `shouldBe` ["DataFrame"]
    it "invents nothing for an unrelated name" $
        candidateModules 1 "Zzznope" pool `shouldBe` []
    it "a dotted query is judged whole, never by its last component" $
        candidateModules 1 "Zzz.Svg" pool `shouldBe` []

indexSpec :: Spec
indexSpec = describe "Sabela.AI.PackageIndex" $ do
    describe "parsePackageDump" $ do
        it "reads every record, not just the first" $
            map peName (parsePackageDump dump)
                `shouldBe` ["dataframe", "granite", "hspec"]

        it "keeps name and version off the single-line fields" $
            map peVersion (parsePackageDump dump)
                `shouldBe` ["2.3.0.0", "0.7.3.0", "2.11.17"]

        it "parses the space-separated exposed-modules form" $
            modulesOfPackage (parsePackageDump dump) "hspec"
                `shouldBe` [ "Test.Hspec"
                           , "Test.Hspec.Discover"
                           , "Test.Hspec.Formatters"
                           , "Test.Hspec.QuickCheck"
                           , "Test.Hspec.Runner"
                           ]

        it "an exactly-spelled module is never corrected" $
            map peName (packagesExposingModule (parsePackageDump dump) "Test.Hspec")
                `shouldBe` ["hspec"]

        it "coalesces a wrapped exposed-modules list" $
            modulesOfPackage (parsePackageDump dump) "dataframe"
                `shouldBe` [ "DataFrame"
                           , "DataFrame.Operations.Statistics"
                           , "DataFrame.Display"
                           ]

        it "drops a re-export's origin, keeping the name a caller writes" $
            modulesOfPackage (parsePackageDump dump) "dataframe"
                `shouldSatisfy` elem "DataFrame.Display"

        it "stops the module list at the next field" $
            modulesOfPackage (parsePackageDump dump) "dataframe"
                `shouldSatisfy` notElem "base-4.21.0.0"

        it "is empty for a package that is not installed" $
            modulesOfPackage (parsePackageDump dump) "nosuchpkg" `shouldBe` []

    describe "lookups" $ do
        it "finds the package exposing a module" $
            map peName (packagesExposingModule (parsePackageDump dump) "Granite.Svg")
                `shouldBe` ["granite"]

        it "matches a module by keyword, case-insensitively" $
            map fst (modulesMatching (parsePackageDump dump) "statistics")
                `shouldBe` ["DataFrame.Operations.Statistics"]

        it "names the package a keyword-matched module came from" $
            map (peName . snd) (modulesMatching (parsePackageDump dump) "svg")
                `shouldBe` ["granite"]

        it "an empty query matches nothing, rather than everything" $
            modulesMatching (parsePackageDump dump) "  " `shouldBe` []

nearSpellingSpec :: Spec
nearSpellingSpec = describe "a module name resolves by near spelling" $ do
    let mods = ["DataFrame", "DataFrame.Operations.Statistics", "Granite.Svg"]
        near q = closestModules 1 0.4 q mods
    it "resolves Data.Frame to DataFrame" $
        near "Data.Frame" `shouldBe` ["DataFrame"]
    it "never returns the query itself" $
        near "Granite.Svg" `shouldBe` []
    it "resolves a dropped dot the other way too" $
        near "GraniteSvg" `shouldBe` ["Granite.Svg"]
    it "does not invent a match for an unrelated name" $
        near "Zzznope" `shouldBe` []

usableCardSpec :: Spec
usableCardSpec = describe "a card answers, or it falls through" $ do
    let card st = object ["module" .= ("DataFrame" :: Text), "status" .= (st :: Text)]
    it "an ok listing answers" $
        usableCard (card "ok") `shouldBe` True
    it "a hidden-package card does not answer: it names the wall" $
        usableCard (card "installed-not-loaded") `shouldBe` False
    it "an error card does not answer" $
        usableCard (card "error") `shouldBe` False
    it "a statusless value is not a card" $
        usableCard (object ["module" .= ("DataFrame" :: Text)]) `shouldBe` False

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (Key.fromText k) o
field _ _ = Nothing

str :: Text -> Value -> Maybe Text
str k v = case field k v of
    Just (String t) -> Just t
    _ -> Nothing

packageCardSpec :: Spec
packageCardSpec = describe "an installed-but-hidden package is not absent" $ do
    let pkgs = parsePackageDump dump
        cardFor n = packageCardOf pkgs n "A synopsis"
    it "names the package the store has" $
        fmap (str "package") (cardFor "granite") `shouldBe` Just (Just "granite")
    it "reports hidden-package, which is the state the classifier reads" $
        fmap (str "status") (cardFor "granite") `shouldBe` Just (Just "installed-not-loaded")
    it "carries the cabal line that exposes it" $
        fmap (str "cabal") (cardFor "granite")
            `shouldBe` Just (Just "-- cabal: build-depends: granite")
    it "lists the modules, so the caller knows what to import" $
        fmap (field "modules") (cardFor "granite")
            `shouldBe` Just (Just (toJSON (["Granite.Svg"] :: [Text])))
    it "a package the store does not have stays absent" $
        cardFor "nosuchpkg" `shouldBe` Nothing
