{-# LANGUAGE OverloadedStrings #-}

module Test.DiagnosticMitigationLiveSpec (spec) where

import Data.Aeson (Value (..))
import qualified Data.Text as T
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Test.DiagnosticMitigationFixtures (
    classesOf,
    field,
    insertSrc,
    mitigate,
    newFixture,
    requireLiveIntegration,
    textField,
 )

spec :: Spec
spec = describe "G6 seed-class mitigations (live)" $ do
    it "missing-extension: TupleSections is enabled and the cell compiles clean" $ do
        requireLiveIntegration
        withSystemTempDirectory "sabela-mitigate-ext" $ \dir -> do
            (clean, mitigations, post) <-
                mitigate dir "tsVal = (1,) (2 :: Int) :: (Int, Int)"
            clean `shouldBe` True
            case mitigations of
                Just v -> do
                    textField "status" v `shouldBe` Just "complete"
                    classesOf v `shouldBe` ["missing-extension"]
                Nothing -> expectationFailure "expected a mitigations disclosure"
            case post of
                Just s -> s `shouldSatisfy` T.isInfixOf "TupleSections"
                Nothing -> expectationFailure "cell vanished"

    it "ambiguous-occurrence: the one qualification that compiles is chosen" $ do
        requireLiveIntegration
        withSystemTempDirectory "sabela-mitigate-ambig" $ \dir -> do
            (clean, mitigations, post) <-
                mitigate
                    dir
                    "import Data.List (uncons)\n\
                    \import Data.List.NonEmpty (uncons)\n\
                    \ambigResult = uncons [1, 2, 3 :: Int]"
            clean `shouldBe` True
            fmap classesOf mitigations `shouldBe` Just ["ambiguous-occurrence"]
            case post of
                Just s -> s `shouldSatisfy` T.isInfixOf "Data.List.uncons"
                Nothing -> expectationFailure "cell vanished"

    it "did-you-mean: a one-letter typo heals to the real in-scope name" $ do
        requireLiveIntegration
        withSystemTempDirectory "sabela-mitigate-typo" $ \dir -> do
            (clean, mitigations, post) <-
                mitigate dir "greetLen = lengthh \"hello\""
            clean `shouldBe` True
            fmap classesOf mitigations `shouldBe` Just ["did-you-mean"]
            case post of
                Just s -> s `shouldSatisfy` T.isInfixOf "length \"hello\""
                Nothing -> expectationFailure "cell vanished"

    it "missing-import: an unimported Sabela.Notebook export gains its import" $ do
        requireLiveIntegration
        withSystemTempDirectory "sabela-mitigate-import" $ \dir -> do
            (clean, mitigations, post) <-
                mitigate dir "shape = never"
            clean `shouldBe` True
            fmap classesOf mitigations `shouldBe` Just ["missing-import"]
            case post of
                Just s -> s `shouldSatisfy` T.isInfixOf "import"
                Nothing -> expectationFailure "cell vanished"

    it
        "fractional-int-sine: the live_test5 specimen mitigates to Double and compiles clean"
        $ do
            requireLiveIntegration
            withSystemTempDirectory "sabela-mitigate-sine" $ \dir -> do
                (clean, mitigations, post) <-
                    mitigate dir "w = 400 :: Int\nsineY = pi / w"
                clean `shouldBe` True
                fmap classesOf mitigations `shouldBe` Just ["fractional-int"]
                case post of
                    Just s -> s `shouldSatisfy` T.isInfixOf "Double"
                    Nothing -> expectationFailure "cell vanished"

    it
        "multi-green ambiguity: nothing applied, the fact list names every compiling candidate"
        $ do
            requireLiveIntegration
            withSystemTempDirectory "sabela-mitigate-multigreen" $ \dir -> do
                (_clean, mitigations, post) <-
                    mitigate
                        dir
                        "import Data.List (uncons)\n\
                        \import Data.List.NonEmpty (uncons)\n\
                        \ambigResult = uncons"
                case mitigations of
                    Just v -> do
                        textField "status" v `shouldBe` Just "partial"
                        field "resolved" v `shouldBe` Just (Number 0)
                        case field "factLists" v of
                            Just (Array facts) -> length facts `shouldSatisfy` (>= 1)
                            _ -> expectationFailure "expected a non-empty factLists"
                    Nothing -> expectationFailure "expected a mitigations disclosure"
                case post of
                    Just s -> s `shouldSatisfy` T.isInfixOf "ambigResult = uncons"
                    Nothing -> expectationFailure "cell vanished"

    it
        "unlisted class: a genuine type mismatch is a plain gate rejection, no mitigation attempted"
        $ do
            requireLiveIntegration
            withSystemTempDirectory "sabela-mitigate-unlisted" $ \dir -> do
                (app, store, rn) <- newFixture dir
                ack <- insertSrc app store rn "badMatch = (5 :: Int) == \"five\""
                field "mitigations" ack `shouldBe` Nothing
                field "error" ack `shouldNotBe` Nothing
