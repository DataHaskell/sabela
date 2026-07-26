{-# LANGUAGE OverloadedStrings #-}

{- | The @ghc-pkg dump@ parser behind the installed-package module index.
Fixtures are the real shapes the store emits: wrapped @exposed-modules@ lists
and re-exports carrying a @from pkg:Module@ origin.
-}
module Test.PackageIndexSpec (spec) where

import Data.Aeson (object, (.=))
import Data.Text (Text)
import Test.Hspec

import Sabela.AI.Capabilities.ModuleSearch (usableCard)
import Sabela.AI.ModuleResolve (closestModules)
import Sabela.AI.PackageIndex (
    PackageEntry (..),
    modulesMatching,
    modulesOfPackage,
    packagesExposingModule,
    parsePackageDump,
 )

{- | Three records as @ghc-pkg dump@ writes them, @---@ separated — BOTH
separator forms: comma-with-origins (dataframe) and plain space-separated
(hspec). The space form's whole module list was once dropped, so
@Test.Hspec@ missed exact match and near-spelling "corrected" it to a module
of a different package.
-}
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
    usableCardSpec
    nearSpellingSpec
    indexSpec

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

        {- live_test33_wine: `statistics` had to reach the hidden dataframe
        package's stats module without the model first committing to install. -}
        it "matches a module by keyword, case-insensitively" $
            map fst (modulesMatching (parsePackageDump dump) "statistics")
                `shouldBe` ["DataFrame.Operations.Statistics"]

        it "names the package a keyword-matched module came from" $
            map (peName . snd) (modulesMatching (parsePackageDump dump) "svg")
                `shouldBe` ["granite"]

        it "an empty query matches nothing, rather than everything" $
            modulesMatching (parsePackageDump dump) "  " `shouldBe` []

{- | live_test32_wine queried `Data.Frame` — one dot from the real
`DataFrame` — and was told nothing exists, so it hand-rolled the work. Exact
module matching cannot answer a one-character miss.
-}
nearSpellingSpec :: Spec
nearSpellingSpec = describe "a module name resolves by near spelling" $ do
    let mods = ["DataFrame", "DataFrame.Operations.Statistics", "Granite.Svg"]
        near q = closestModules 1 0.4 q mods
    it "resolves Data.Frame to DataFrame" $
        near "Data.Frame" `shouldBe` ["DataFrame"]
    -- The exact name is resolved before this runs (closestModules is the
    -- did-you-mean path and never returns the query itself).
    it "never returns the query itself" $
        near "Granite.Svg" `shouldBe` []
    it "resolves a dropped dot the other way too" $
        near "GraniteSvg" `shouldBe` ["Granite.Svg"]
    it "does not invent a match for an unrelated name" $
        near "Zzznope" `shouldBe` []

{- | live_test38 asked discover for `DataFrame` and got a four-field card with
no exports: the session cannot browse a hidden package, and its
`status: hidden-package` card counted as a usable ANSWER, so the store lookup
that does list the exports was never reached. A card that reports why the
query could not be answered here is not an answer.
-}
usableCardSpec :: Spec
usableCardSpec = describe "a card answers, or it falls through" $ do
    let card st = object ["module" .= ("DataFrame" :: Text), "status" .= (st :: Text)]
    it "an ok listing answers" $
        usableCard (card "ok") `shouldBe` True
    it "a hidden-package card does not answer: it names the wall" $
        usableCard (card "hidden-package") `shouldBe` False
    it "an error card does not answer" $
        usableCard (card "error") `shouldBe` False
    it "a statusless value is not a card" $
        usableCard (object ["module" .= ("DataFrame" :: Text)]) `shouldBe` False
