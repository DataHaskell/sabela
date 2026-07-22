{-# LANGUAGE OverloadedStrings #-}

{- | The typed error-index interface: GHC codes → reference URL + remediation
class, and how a rejected cell's diagnostics surface to the model. Pins the
wire shape the model consumes ('Sabela.AI.ErrorIndex').
-}
module Test.ErrorIndexSpec (spec) where

import Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.Aeson.KeyMap as KM
import Test.Hspec

import Sabela.AI.CellResult (CellOutcome (..), CellResult (..))
import Sabela.AI.ErrorIndex
import Sabela.Model (CellError (..), bareCellError)

coded :: Int -> CellError
coded n = CellError (Just 1) (Just 1) "boom" (Just n)

rejected :: [CellError] -> CellResult
rejected ds = CellResult (Rejected ds) [] []

spec :: Spec
spec = describe "Sabela.AI.ErrorIndex" $ do
    describe "renderGhcCode / errorIndexUrl" $ do
        it "renders the canonical zero-padded GHC-NNNNN key" $
            renderGhcCode (GhcCode 88464) `shouldBe` "GHC-88464"
        it "zero-pads a short code to five digits" $
            renderGhcCode (GhcCode 1) `shouldBe` "GHC-00001"
        it "builds the errors.haskell.org message URL" $
            errorIndexUrl (GhcCode 93557)
                `shouldBe` "https://errors.haskell.org/messages/GHC-93557/"

    describe "remediationFor (the automatic/manual axis)" $ do
        it "a hidden-package error is deterministically auto-repaired" $
            remediationFor (GhcCode 87110) `shouldBe` AutoRepaired "dependency"
        it "a not-in-scope name yields model-picked candidates" $
            remediationFor (GhcCode 88464) `shouldBe` ModelCandidates
        it "a type mismatch is a manual fix" $
            remediationFor (GhcCode 83865) `shouldBe` ManualFix
        it "an unseeded code defaults to manual, never a false auto-promise" $
            remediationFor (GhcCode 99999) `shouldBe` ManualFix

    describe "errorInfoOf" $ do
        it "builds the typed diagnostic from a coded cell error" $
            errorInfoOf (coded 88464)
                `shouldBe` Just
                    ( ErrorInfo
                        (GhcCode 88464)
                        "https://errors.haskell.org/messages/GHC-88464/"
                        ModelCandidates
                    )
        it "is Nothing for an uncoded (synthetic) error" $
            errorInfoOf (bareCellError Nothing Nothing "synthetic") `shouldBe` Nothing

    describe "errorInfoForCell" $ do
        it "surfaces one typed diagnostic per distinct code of a rejected cell" $
            map eiCode (errorInfoForCell (rejected [coded 88464, coded 83865]))
                `shouldBe` [GhcCode 88464, GhcCode 83865]
        it "dedupes repeated codes" $
            length (errorInfoForCell (rejected [coded 87110, coded 87110]))
                `shouldBe` 1
        it "ignores uncoded diagnostics" $
            errorInfoForCell (rejected [bareCellError Nothing Nothing "x"])
                `shouldBe` []
        it "a clean success carries no diagnostics" $
            errorInfoForCell (CellResult Succeeded [] []) `shouldBe` []
        it "a runtime Raised carries no compiler code" $
            errorInfoForCell (CellResult (Raised "boom") [] []) `shouldBe` []

    describe "wire shape" $ do
        it "an ErrorInfo encodes code/reference/remediation" $
            toJSON (ErrorInfo (GhcCode 88464) "u" ModelCandidates)
                `shouldBe` object
                    [ "code" .= ("GHC-88464" :: Value)
                    , "reference" .= ("u" :: Value)
                    , "remediation" .= object ["kind" .= ("candidates" :: Value)]
                    ]
        it "an automatic remediation names its fixer" $
            toJSON (AutoRepaired "dependency")
                `shouldBe` object
                    [ "kind" .= ("automatic" :: Value)
                    , "fixer" .= ("dependency" :: Value)
                    ]
        it "a manual remediation is just its kind" $
            toJSON ManualFix `shouldBe` object ["kind" .= ("manual" :: Value)]

    describe "errorInfoPairs / withErrorInfo" $ do
        it "omits the pair when there are no diagnostics" $
            errorInfoPairs [] `shouldBe` []
        it "emits a single diagnostics pair when there are some" $
            length (errorInfoPairs (errorInfoForCell (rejected [coded 88464])))
                `shouldBe` 1
        it "merges a diagnostics key into the result object" $
            case withErrorInfo (rejected [coded 88464]) (object ["ok" .= False]) of
                Object o -> KM.member "diagnostics" o `shouldBe` True
                other -> expectationFailure ("expected object, got " <> show other)
        it "leaves the object untouched when there is nothing to add" $
            withErrorInfo (CellResult Succeeded [] []) (object ["ok" .= True])
                `shouldBe` object ["ok" .= True]
