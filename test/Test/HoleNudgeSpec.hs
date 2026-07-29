{-# LANGUAGE OverloadedStrings #-}

module Test.HoleNudgeSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.Capabilities.Edit.HoleNudge (
    attachPairs,
    holeNudgePairs,
    inventedNames,
 )

overlaySrc :: Text
overlaySrc =
    "animatedPlot t = Sabela.Notebook.overlay (plot a) (plot b)\n\
    \main1 = animateWith defaultAnim 5.0 animatedPlot"

overlayDiag :: Text
overlayDiag =
    "<interactive>:364:8: error: [GHC-76037]\n\
    \    Not in scope: \8216Sabela.Notebook.overlay\8217"

holeBlob :: Text
holeBlob =
    "<interactive>:364:8: error: [GHC-88464]\n\
    \    Found hole: _ :: Picture -> Picture -> Picture\n\
    \    Valid hole fits include\n\
    \      (<>) :: Picture -> Picture -> Picture\n\
    \      const :: Picture -> Picture -> Picture"

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (Key.fromText k) o
field _ _ = Nothing

spec :: Spec
spec = describe "typed-hole nudge on gate rejections" $ do
    describe "inventedNames" $ do
        it "extracts a qualified invented function name" $
            inventedNames overlayDiag overlaySrc
                `shouldBe` ["Sabela.Notebook.overlay"]
        it "skips names the candidate itself defines (knock-ons)" $
            inventedNames
                "Variable not in scope: animatedPlot :: Time -> Picture"
                overlaySrc
                `shouldBe` []
        it "skips type-level names (a hole cannot stand in for them)" $
            inventedNames
                "Not in scope: type constructor or class \8216Time\8217"
                overlaySrc
                `shouldBe` []
        it "is empty for non-scope diagnostics" $
            inventedNames "Couldn't match type Int with Bool" overlaySrc
                `shouldBe` []

    describe "holeNudgePairs" $ do
        it "probes with the invented name replaced by a hole" $ do
            probed <- newIORef []
            _ <-
                holeNudgePairs
                    (\s -> modifyIORef' probed (s :) >> pure holeBlob)
                    overlayDiag
                    overlaySrc
            sent <- readIORef probed
            sent `shouldSatisfy` any (T.isInfixOf "_ (plot a) (plot b)")
            sent
                `shouldSatisfy` ( not
                                    . any (T.isInfixOf "Sabela.Notebook.overlay (plot a)")
                                )

        it "carries the hole type and the compiler's fits" $ do
            pairs <- holeNudgePairs (const (pure holeBlob)) overlayDiag overlaySrc
            let v = object pairs
            case field "typeDirected" v of
                Nothing -> expectationFailure "no typeDirected block"
                Just td -> do
                    field "holeType" td
                        `shouldBe` Just (String "Picture -> Picture -> Picture")
                    field "invented" td
                        `shouldBe` Just (String "Sabela.Notebook.overlay")
                    case field "holeFits" td of
                        Just (Array fits) -> length fits `shouldBe` 2
                        _ -> expectationFailure "no holeFits"

        it "yields nothing when the probe finds no hole (name was resolvable)" $ do
            pairs <-
                holeNudgePairs
                    (const (pure "some unrelated diagnostic"))
                    overlayDiag
                    overlaySrc
            pairs `shouldBe` []

        it "yields nothing off-trigger, without probing" $ do
            probed <- newIORef (0 :: Int)
            pairs <-
                holeNudgePairs
                    (\_ -> modifyIORef' probed (+ 1) >> pure holeBlob)
                    "Couldn't match type Int with Bool"
                    overlaySrc
            pairs `shouldBe` []
            n <- readIORef probed
            n `shouldBe` 0

    describe "attachPairs" $ do
        it "adds the pairs to an object rejection" $ do
            let v = attachPairs ["extra" .= (1 :: Int)] (object ["a" .= (2 :: Int)])
            field "extra" v `shouldBe` Just (Number 1)
        it "passes non-objects and empty pairs through" $ do
            attachPairs [] (object ["a" .= (2 :: Int)])
                `shouldBe` object ["a" .= (2 :: Int)]
            attachPairs ["x" .= (1 :: Int)] (String "s") `shouldBe` String "s"
