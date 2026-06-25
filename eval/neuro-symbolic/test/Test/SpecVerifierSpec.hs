{-# LANGUAGE OverloadedStrings #-}

module Test.SpecVerifierSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Eval.Task (
    Grader (..),
    Task (..),
    Verdict (..),
    proposeTest,
    taskTest,
    verifyDiff,
 )

tested :: Task
tested = Task "double" "Define double." (ByValue "double 21 == 42")

untested :: Task
untested = Task "scratch" "Sketch something." Untested

spec :: Spec
spec = describe "C3 tests as the spec" $ do
    describe "taskTest" $ do
        it "exposes the covering test of a tested task" $
            taskTest tested `shouldBe` Just "double 21 == 42"

        it "is Nothing for a task with no test" $
            taskTest untested `shouldBe` Nothing

    describe "verifyDiff (tests as a verifier alongside GHC)" $ do
        it "surfaces the diff when it compiles and the covering test is green" $
            verifyDiff True (Just "double 21 == 42") True
                `shouldBe` Surfaced

        it "withholds the diff when the test is red even though it compiled" $ do
            let v = verifyDiff True (Just "double 21 == 42") False
            isWithheld v `shouldBe` True

        it "withholds the diff when it does not even compile" $ do
            let v = verifyDiff False (Just "double 21 == 42") False
            isWithheld v `shouldBe` True

        it "never surfaces on a compile pass alone when a test exists but is red" $
            verifyDiff True (Just "double 21 == 42") False
                `shouldNotBe` Surfaced

        it "proposes a test when the task has none, even if it compiled" $ do
            let v = verifyDiff True Nothing True
            isProposeTest v `shouldBe` True

        it "withholds rather than proposing when an untested diff fails to compile" $ do
            let v = verifyDiff False Nothing False
            isWithheld v `shouldBe` True

    describe "proposeTest" $ do
        it "produces a runnable check cell naming the task binding" $ do
            let cell = proposeTest untested
            ("scratch" `T.isInfixOf` cell) `shouldBe` True
            (T.length cell > 0) `shouldBe` True

isWithheld :: Verdict -> Bool
isWithheld (Withheld _) = True
isWithheld _ = False

isProposeTest :: Verdict -> Bool
isProposeTest (ProposeTest _) = True
isProposeTest _ = False
