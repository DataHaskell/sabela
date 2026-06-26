{-# LANGUAGE OverloadedStrings #-}

{- | The diagnostic interpreter: GHC error text in, structured 'Guidance' out.
These pin the rules that let a weak model self-correct from a tool result
instead of needing the recipe pre-loaded into its prompt.
-}
module Test.DiagnoseSpec (diagnoseSpec) where

import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.CellResult (CellOutcome (..), CellResult (..))
import Sabela.Diagnose (
    Guidance (..),
    diagnose,
    guidanceForCell,
    guidancePairs,
    neededExtension,
 )
import Sabela.Diagnose.Packages (
    packageForModule,
    packageNameIndex,
    resolvePackageToken,
    table,
 )
import Sabela.Model (CellError (..))
import Test.Hspec

cats :: [Guidance] -> [Text]
cats = map gCategory

msgs :: [Guidance] -> Text
msgs = T.intercalate " | " . map gMessage

diagnoseSpec :: Spec
diagnoseSpec = describe "Sabela.Diagnose" $ do
    describe "packageForModule" $ do
        it "maps Granite modules to granite" $
            packageForModule "Granite.Svg" `shouldBe` Just "granite"
        it "prefers the umbrella for DataFrame.Display, core otherwise" $ do
            packageForModule "DataFrame.Display.Web.Plot" `shouldBe` Just "dataframe"
            packageForModule "DataFrame" `shouldBe` Just "dataframe-core"
        it "returns Nothing for an unknown / base module" $
            packageForModule "Data.List" `shouldBe` Nothing

    describe "neededExtension (auto-fix detector)" $ do
        it "reads the extension from GHC's bare 'intended to use' hint" $
            neededExtension "Perhaps you intended to use TemplateHaskell"
                `shouldBe` Just "TemplateHaskell"
        it "handles GHC's real backquoted `Ext' phrasing and the :set -X hint" $ do
            neededExtension "Perhaps you intended to use the `LambdaCase' extension"
                `shouldBe` Just "LambdaCase"
            neededExtension "You may enable this with:\n  :set -XTupleSections"
                `shouldBe` Just "TupleSections"
        it "reads it from a quoted suggestion" $
            neededExtension
                "Suggested fix: Perhaps you intended to use \8216OverloadedStrings\8217"
                `shouldBe` Just "OverloadedStrings"
        it "ignores a suggestion outside the known-extension allow-list" $
            neededExtension "Perhaps you intended to use NotARealExtension"
                `shouldBe` Nothing
        it "is Nothing when no extension is suggested" $
            neededExtension "Couldn't match Int with Bool" `shouldBe` Nothing

    describe "resolvePackageToken (fuzzy package-name index)" $ do
        it "passes a real package token through by exact membership" $ do
            resolvePackageToken "dataframe" `shouldBe` Just "dataframe"
            resolvePackageToken "dataframe-core" `shouldBe` Just "dataframe-core"
            resolvePackageToken "granite" `shouldBe` Just "granite"
        it "repairs a versioned typo to the real package" $
            resolvePackageToken "dataframe-2" `shouldBe` Just "dataframe"
        it "repairs an abbreviated typo to the real package" $
            resolvePackageToken "df-core" `shouldBe` Just "dataframe-core"
        it "falls through cleanly for a token that resembles nothing" $ do
            resolvePackageToken "frobnicator" `shouldBe` Nothing
            resolvePackageToken "numpy" `shouldBe` Nothing
        it "keeps its keys consistent with the curated module table" $
            -- every package the module table maps to must be a member of the
            -- name index, so the two resolvers never disagree.
            map snd table `shouldSatisfy` all (`elem` packageNameIndex)

    describe "missing module (the plotting failure)" $ do
        it "foregrounds the dep from GHC's hidden-package wall, version stripped" $ do
            -- the real GHC message, repeated as GHC repeats it
            let raw =
                    "<no location info>: error: [GHC-87110]\n\
                    \    Could not load module \8216Granite.Svg\8217.\n\
                    \    It is a member of the hidden package \8216granite-0.7.3.0\8217.\n\
                    \    Perhaps you need to add \8216granite\8217 to the build-depends in your .cabal file.\n\
                    \    It is a member of the hidden package \8216granite-0.6.0.0\8217."
                g = diagnose raw
            cats g `shouldBe` ["missing-dependency"]
            msgs g `shouldSatisfy` T.isInfixOf "build-depends: granite"
            msgs g `shouldSatisfy` T.isInfixOf "-- cabal:"
            -- the version must be stripped: 'granite', never 'granite-0.7.3.0'
            msgs g `shouldNotSatisfy` T.isInfixOf "granite-0"
        it "resolves a truly-not-found module through the table" $ do
            let g = diagnose "Could not find module \8216DataFrame.Display.Web.Plot\8217"
            cats g `shouldBe` ["missing-dependency"]
            msgs g `shouldSatisfy` T.isInfixOf "build-depends: dataframe"
        it "still teaches the mechanism for an unknown module" $ do
            let g = diagnose "Could not find module \8216Foo.Bar\8217"
            cats g `shouldBe` ["missing-dependency"]
            msgs g `shouldSatisfy` T.isInfixOf "build-depends"

    describe "did-you-mean passthrough" $
        it "surfaces GHC's own suggestion" $ do
            let g =
                    diagnose "Variable not in scope: lenght\n    Perhaps you meant \8216length\8217"
            cats g `shouldBe` ["did-you-mean"]
            msgs g `shouldSatisfy` T.isInfixOf "length"

    describe "other classes" $ do
        it "flags a top-level let parse error" $
            cats (diagnose "<interactive>:1:5: error: parse error on input \8216let\8217")
                `shouldBe` ["top-level-let"]
        it "advises an annotation on an ambiguous type" $
            cats
                ( diagnose
                    "Ambiguous type variable \8216a0\8217 arising from a use of \8216show\8217"
                )
                `shouldBe` ["ambiguous-type"]
        it "surfaces a type mismatch line" $ do
            let g =
                    diagnose
                        "Couldn't match expected type \8216Int\8217 with actual type \8216[Char]\8217"
            cats g `shouldBe` ["type-mismatch"]
            msgs g `shouldSatisfy` T.isInfixOf "Int"

    describe "clean output" $
        it "produces no guidance for a benign or empty message" $ do
            diagnose "" `shouldBe` []
            diagnose "Just a normal line of output" `shouldBe` []

    describe "guidanceForCell + guidancePairs" $ do
        it "diagnoses a Rejected outcome's compile errors" $ do
            let cr =
                    CellResult
                        ( Rejected
                            [CellError (Just 2) (Just 1) "Could not find module \8216Granite.Svg\8217"]
                        )
                        []
                        []
            cats (guidanceForCell cr) `shouldBe` ["missing-dependency"]
        it "diagnoses a Raised runtime message" $ do
            let cr = CellResult (Raised "Ambiguous type variable \8216a0\8217") [] []
            cats (guidanceForCell cr) `shouldBe` ["ambiguous-type"]
        it "yields no guidance and no wire pair for a clean success" $ do
            let cr = CellResult Succeeded [] []
            guidanceForCell cr `shouldBe` []
            guidancePairs (guidanceForCell cr) `shouldBe` []
        it "emits a guidance pair only when there is guidance" $ do
            let g = diagnose "Could not find module \8216Granite.Svg\8217"
            length (guidancePairs g) `shouldBe` 1
