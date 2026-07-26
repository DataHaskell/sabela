{-# LANGUAGE OverloadedStrings #-}

{- | G6 task 7's definitive proof: one cell seeded with FOUR independent
defects at once. Asserts the loop iterates across rounds rather than
stopping after one fix, and that a fifth unrepairable defect stops honestly.
-}
module Test.DiagnosticMitigationCompoundSpec (spec) where

import Data.Aeson (Value (..))
import qualified Data.Text as T
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Test.DiagnosticMitigationFixtures (
    classesOf,
    field,
    mitigate,
    requireLiveIntegration,
    textField,
 )

{- | Four independent defects — missing TupleSections, a typo, an ambiguous
@uncons@, a fractional-int mismatch — none a knock-on of another.
-}
compoundFourSrc :: T.Text
compoundFourSrc =
    "import Data.List (uncons)\n\
    \import Data.List.NonEmpty (uncons)\n\
    \\n\
    \w = 400 :: Int\n\
    \greetLen = lengthh \"hi\"\n\
    \ambigResult = uncons [1, 2, 3 :: Int]\n\
    \sineY = pi / w\n\
    \tup = (1,) (2 :: Int)"

{- | 'compoundFourSrc' plus a fifth, genuinely unrelated defect with no
matching table row — a plain type mismatch (@Couldn't match type@).
-}
roundCapSrc :: T.Text
roundCapSrc = compoundFourSrc <> "\nbadMatch = (5 :: Int) == \"five\""

spec :: Spec
spec = describe "G6 compound-four — the iteration proof" $ do
    it
        "resolves all four independent defects across successive rounds, never stopping after one"
        $ do
            requireLiveIntegration
            withSystemTempDirectory "sabela-mitigate-compound4" $ \dir -> do
                (clean, mitigations, post) <- mitigate dir compoundFourSrc
                case mitigations of
                    Nothing -> expectationFailure "expected a mitigations disclosure"
                    Just v -> do
                        let order = classesOf v
                        order
                            `shouldBe` [ "missing-extension"
                                       , "ambiguous-occurrence"
                                       , "did-you-mean"
                                       , "fractional-int"
                                       ]
                        textField "status" v `shouldBe` Just "complete"
                        field "resolved" v `shouldBe` Just (Number 4)
                clean `shouldBe` True
                case post of
                    Just s -> do
                        s `shouldSatisfy` T.isInfixOf "TupleSections"
                        s `shouldSatisfy` T.isInfixOf "Data.List.uncons"
                        s `shouldSatisfy` T.isInfixOf "length \"hi\""
                        s `shouldSatisfy` T.isInfixOf "Double"
                    Nothing -> expectationFailure "cell vanished"

    it "round-cap sibling: stops honestly at 4 of 5, never claims full success" $ do
        requireLiveIntegration
        withSystemTempDirectory "sabela-mitigate-compound5" $ \dir -> do
            (clean, mitigations, _post) <- mitigate dir roundCapSrc
            clean `shouldBe` False
            case mitigations of
                Nothing -> expectationFailure "expected a mitigations disclosure"
                Just v -> do
                    classesOf v
                        `shouldBe` [ "missing-extension"
                                   , "ambiguous-occurrence"
                                   , "did-you-mean"
                                   , "fractional-int"
                                   ]
                    textField "status" v `shouldBe` Just "partial"
                    field "resolved" v `shouldBe` Just (Number 4)
                    field "total" v `shouldBe` Just (Number 5)
                    case textField "note" v of
                        Just note ->
                            note `shouldSatisfy` T.isInfixOf "resolved 4 of 5 diagnostics; 5 remains"
                        Nothing -> expectationFailure "note field missing"
