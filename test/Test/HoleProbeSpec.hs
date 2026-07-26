{-# LANGUAGE OverloadedStrings #-}

module Test.HoleProbeSpec (spec) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.HoleProbe (
    HoleProbeAnswer (..),
    holeProbeAnswers,
    holeProbeFacts,
    holeProbeJson,
    holeProbeProvenance,
 )

twoHoleBlob :: Text
twoHoleBlob =
    T.unlines
        [ "<interactive>:2:11: error: [GHC-88464]"
        , "    • Found hole: _ :: Int"
        , "    • In the first argument of `max', namely `(_ :: Int)'"
        , "    • Relevant bindings include x :: Int (bound at <interactive>:2:1)"
        , "      Valid hole fits include"
        , "        maxBound :: forall a. Bounded a => a"
        , "          with maxBound @Int"
        , "          (imported from `Prelude')"
        , "        minBound :: forall a. Bounded a => a"
        , "          with minBound @Int"
        , "          (imported from `Prelude')"
        , ""
        , "<interactive>:2:22: error: [GHC-88464]"
        , "    • Found hole: _ :: Int"
        , "    • In the second argument of `max', namely `(_ :: Int)'"
        , "      Valid hole fits include"
        , "        maxBound :: forall a. Bounded a => a"
        , "          with maxBound @Int"
        ]

namedTypeBlob :: Text
namedTypeBlob =
    T.unlines
        [ "<interactive>:17:14: error: [GHC-88464]"
        , "    • Found hole: _ :: Plot"
        , "    • In the first argument of `render', namely `(_ :: Plot)'"
        , "    • Relevant bindings include"
        , "        z :: String (bound at <interactive>:17:1)"
        , "      Valid hole fits include"
        , "        Plot :: Plot (defined at <interactive>:2:13)"
        , "        defPlot :: Plot (defined at <interactive>:10:1)"
        ]

noProducerBlob :: Text
noProducerBlob =
    T.unlines
        [ "<interactive>:5:11: error: [GHC-88464]"
        , "    • Found hole: _ :: Point"
        , "    • In the first argument of `line', namely `(_ :: Point)'"
        , "      In the expression: line (_ :: Point) (_ :: Point)"
        , "    • Relevant bindings include"
        , "        x :: Picture (bound at <interactive>:5:1)"
        ]

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (Key.fromText k) o
field _ _ = Nothing

spec :: Spec
spec = describe "G3 hole-probe conclusions" $ do
    it "pairs each hole's goal type with the producers GHC listed" $
        holeProbeAnswers namedTypeBlob
            `shouldBe` [HoleProbeAnswer "Plot" ["Plot", "defPlot"]]

    it "reports one answer per distinct goal type, not one per hole site" $
        holeProbeAnswers twoHoleBlob
            `shouldBe` [HoleProbeAnswer "Int" ["maxBound", "minBound"]]

    it "an unproducible goal type is a real answer, not a missing one" $
        holeProbeAnswers noProducerBlob `shouldBe` [HoleProbeAnswer "Point" []]

    it "a diagnostic with no hole yields no answers" $
        holeProbeAnswers "error: Variable not in scope: foo" `shouldBe` []

    it "states producers plainly, with provenance" $ do
        let facts = holeProbeFacts namedTypeBlob
        facts
            `shouldBe` ["`Plot` is produced by: `Plot`, `defPlot` (" <> holeProbeProvenance <> ")"]

    it "states an empty answer plainly, never as a recommendation" $ do
        let facts = holeProbeFacts noProducerBlob
        facts
            `shouldBe` ["no producer of `Point` found in scope (" <> holeProbeProvenance <> ")"]
        head facts `shouldSatisfy` (not . T.isInfixOf "insert_cell")

    it "no rendered fact hands back hole-bearing code" $
        concatMap holeProbeFacts [twoHoleBlob, namedTypeBlob, noProducerBlob]
            `shouldSatisfy` (not . any (T.isInfixOf "_ ::"))

    it "the wire block carries holes, facts and provenance; nothing without a hole" $ do
        case holeProbeJson namedTypeBlob of
            Nothing -> expectationFailure "expected a hole-probe block"
            Just v -> do
                field "provenance" v `shouldBe` Just (String holeProbeProvenance)
                case field "holes" v of
                    Just (Array hs) -> length hs `shouldBe` 1
                    _ -> expectationFailure "expected a holes array"
                case field "facts" v of
                    Just (Array fs) -> length fs `shouldBe` 1
                    _ -> expectationFailure "expected a facts array"
        holeProbeJson "no holes here" `shouldBe` Nothing
