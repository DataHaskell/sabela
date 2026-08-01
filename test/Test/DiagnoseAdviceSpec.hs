{-# LANGUAGE OverloadedStrings #-}

{- | What the classifier advises, per category, and how that advice reaches a
cell result. The parsers it reads the facts with are in "Test.DiagnoseSpec".
-}
module Test.DiagnoseAdviceSpec (diagnoseAdviceSpec) where

import Data.Aeson (Value (..), toJSON)
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.Capabilities.Edit.CompileGate (exposingPackage)
import Sabela.AI.CellResult (CellOutcome (..), CellResult (..))
import Sabela.Diagnose (
    Guidance (..),
    cellResultWithExtraGuidance,
    cellResultWithGuidance,
    diagnoseWith,
    guidanceForCell,
    guidancePairs,
 )
import Sabela.Model (bareCellError)
import Test.Hspec

{- | The classifier over a cell that declares nothing. Every caller must hand
over a source; these cases are about the diagnostic, so the source is empty.
-}
diagnoseBare :: Text -> [Guidance]
diagnoseBare = diagnoseWith Nothing ""

-- | A cell that imports a module and declares no package.
importOnly :: Text
importOnly = "import Granite.Svg\n"

cats :: [Guidance] -> [Text]
cats = map gCategory

msgs :: [Guidance] -> Text
msgs = T.intercalate " | " . map gMessage

diagnoseAdviceSpec :: Spec
diagnoseAdviceSpec = describe "Sabela.Diagnose (advice)" $ do
    describe "missing module (the plotting failure)" $ do
        it "foregrounds the dep from GHC's hidden-package wall, version stripped" $ do
            let raw =
                    "<no location info>: error: [GHC-87110]\n\
                    \    Could not load module \8216Granite.Svg\8217.\n\
                    \    It is a member of the hidden package \8216granite-0.7.3.0\8217.\n\
                    \    Perhaps you need to add \8216granite\8217 to the build-depends in your .cabal file.\n\
                    \    It is a member of the hidden package \8216granite-0.6.0.0\8217."
                g = diagnoseBare raw
            cats g `shouldBe` ["missing-dependency"]
            msgs g `shouldSatisfy` T.isInfixOf "build-depends: granite"
            msgs g `shouldSatisfy` T.isInfixOf "-- cabal:"
            msgs g `shouldNotSatisfy` T.isInfixOf "granite-0"
        it "names the exposing package when the store knows it" $ do
            exposedBy <-
                exposingPackage "Could not find module \8216Bluefin.State\8217"
            let g =
                    diagnoseWith
                        exposedBy
                        "import Bluefin.State"
                        "Could not find module \8216Bluefin.State\8217"
            cats g `shouldBe` ["missing-dependency"]
            msgs g `shouldSatisfy` T.isInfixOf "build-depends: bluefin"
        it "still teaches the mechanism for an unknown module" $ do
            let g = diagnoseBare "Could not find module \8216Foo.Bar\8217"
            cats g `shouldBe` ["missing-dependency"]
            msgs g `shouldSatisfy` T.isInfixOf "build-depends"

    describe "did-you-mean passthrough" $
        it "surfaces GHC's own suggestion" $ do
            let g =
                    diagnoseBare
                        "Variable not in scope: lenght\n    Perhaps you meant \8216length\8217"
            cats g `shouldBe` ["did-you-mean"]
            msgs g `shouldSatisfy` T.isInfixOf "length"

    describe "other classes" $ do
        it "flags a top-level let parse error against a cell that wrote one" $
            cats (diagnoseWith Nothing "let x = 5\n" "parse error on input \8216let\8217")
                `shouldBe` ["top-level-let"]

        it "advises an annotation on an ambiguous type" $
            cats
                ( diagnoseBare
                    "Ambiguous type variable \8216a0\8217 arising from a use of \8216show\8217"
                )
                `shouldBe` ["ambiguous-type"]
        it "surfaces a type mismatch line" $ do
            let g =
                    diagnoseBare
                        "Couldn't match expected type \8216Int\8217 with actual type \8216[Char]\8217"
            cats g `shouldBe` ["type-mismatch"]
            msgs g `shouldSatisfy` T.isInfixOf "Int"

    describe "unshowable result (the live_gemma plotting failure)" $ do
        it "names the wrap for the package-qualified Show-at-print failure" $ do
            let g =
                    diagnoseBare
                        "No instance for `Show\n\
                        \  sabela-notebook-0.2.0.0:Sabela.Notebook.Picture.Internal.Picture'\n\
                        \  arising from a use of `print'"
            cats g `shouldBe` ["unshowable-result"]
            msgs g `shouldSatisfy` T.isInfixOf "displayPicture ("
            msgs g `shouldSatisfy` T.isInfixOf "Sabela.Notebook"
        it "points an unknown type at the goal-type discover query" $ do
            let g = diagnoseBare "No instance for `Show Wind' arising from a use of `print'"
            cats g `shouldBe` ["unshowable-result"]
            msgs g `shouldSatisfy` T.isInfixOf "Wind -> IO ()"
        it "stays silent for a Show failure away from the interactive print" $
            diagnoseBare "No instance for `Show Foo'" `shouldBe` []
        it "stays silent for a Fractional no-instance failure" $
            cats
                ( diagnoseBare
                    "No instance for \8216Fractional Int\8217 arising from a use of \8216/\8217"
                )
                `shouldNotContain` ["unshowable-result"]

    describe "clean output" $
        it "produces no guidance for a benign or empty message" $ do
            diagnoseBare "" `shouldBe` []
            diagnoseBare "Just a normal line of output" `shouldBe` []

    describe "guidanceForCell + guidancePairs" $ do
        it "diagnoses a Rejected outcome's compile errors" $ do
            let cr =
                    CellResult
                        ( Rejected
                            [bareCellError (Just 2) (Just 1) "Could not find module \8216Granite.Svg\8217"]
                        )
                        []
                        []
            cats (guidanceForCell importOnly cr) `shouldBe` ["missing-dependency"]
        it "diagnoses a Raised runtime message" $ do
            let cr = CellResult (Raised "Ambiguous type variable \8216a0\8217") [] []
            cats (guidanceForCell importOnly cr) `shouldBe` ["ambiguous-type"]
        it "yields no guidance and no wire pair for a clean success" $ do
            let cr = CellResult Succeeded [] []
            guidanceForCell importOnly cr `shouldBe` []
            guidancePairs (guidanceForCell importOnly cr) `shouldBe` []
        it "emits a guidance pair only when there is guidance" $ do
            let g = diagnoseBare "Could not find module \8216Granite.Svg\8217"
            length (guidancePairs g) `shouldBe` 1

    describe "cellResultWithExtraGuidance" $ do
        it "matches cellResultWithGuidance when there is no extra guidance" $ do
            let cr = CellResult (Raised "Ambiguous type variable \8216a0\8217") [] []
            cellResultWithExtraGuidance importOnly [] cr
                `shouldBe` cellResultWithGuidance importOnly cr
        it "appends extra guidance alongside the diagnosed guidance" $ do
            let cr = CellResult Succeeded [] []
                extra = Guidance "file-not-found" "no similar file was found"
            case cellResultWithExtraGuidance importOnly [extra] cr of
                Object o -> KM.lookup "guidance" o `shouldBe` Just (toJSON [extra])
                _ -> expectationFailure "expected an object"
