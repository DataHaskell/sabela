{-# LANGUAGE OverloadedStrings #-}

{- | The @resource@ runaway diagnostic class (R3.9/R6.5): ONE bounded line,
triggered as a function of wall\/heap\/progress evidence only — never cell
content — over generated (elapsed, progress-evidence) grids. The line lets
the proposer shrink its own proposal; it never recognises a task.
-}
module Test.ResourceSpec (spec) where

import Control.Monad (forM_)
import Data.Maybe (isJust)
import qualified Data.Text as T

import Sabela.AI.Resource (
    ResourceEvidence (..),
    heapClimbing,
    resourceLine,
    resourceTriggered,
 )
import Test.Hspec

budget :: Int
budget = 30000

-- | The full evidence grid: elapsed × heap shape × progress events.
evidenceGrid :: [ResourceEvidence]
evidenceGrid =
    [ ResourceEvidence elapsed heap events
    | elapsed <- [0, budget - 1, budget, budget + 1, 10 * budget, 999999999]
    , heap <-
        [ []
        , [512000]
        , [512000, 512000, 512000]
        , [512000, 700000, 901000]
        , [901000, 700000, 512000]
        ]
    , events <- [0, 1, 50]
    ]

spec :: Spec
spec = describe "resource runaway diagnostic (R3.9/R6.5)" $ do
    it "any emitted line is ONE line and <= 200 chars, over the full grid" $
        forM_ evidenceGrid $ \e ->
            forM_ [Nothing, Just 0, Just 3, Just 99999] $ \mCid ->
                case resourceLine budget mCid e of
                    Nothing -> pure ()
                    Just line -> do
                        T.length line <= 200 `shouldBe` True
                        T.isInfixOf "\n" line `shouldBe` False

    it "triggers iff the wall budget is exceeded AND evidence shows a runaway" $
        forM_ evidenceGrid $ \e -> do
            let expected =
                    reElapsedMs e >= budget
                        && (heapClimbing (reHeapBytes e) || reEventsSeen e == 0)
            resourceTriggered budget e `shouldBe` expected
            isJust (resourceLine budget Nothing e) `shouldBe` expected

    it "heapClimbing: monotone growth only" $ do
        heapClimbing [512000, 700000, 901000] `shouldBe` True
        heapClimbing [901000, 700000, 512000] `shouldBe` False
        heapClimbing [512000, 512000, 512000] `shouldBe` False
        heapClimbing [] `shouldBe` False
        heapClimbing [512000] `shouldBe` False

    it "the line is a function of the evidence alone (task-independence)" $ do
        -- Two 'tasks' with identical resource evidence produce the identical
        -- line: the function takes no cell content, and equal evidence maps
        -- to equal text — only the cell id may differ.
        let e = ResourceEvidence (4 * budget) [512000, 901000] 0
        resourceLine budget (Just 2) e `shouldBe` resourceLine budget (Just 2) e
        let lA = resourceLine budget (Just 2) e
            lB = resourceLine budget (Just 5) e
        fmap (T.replace "cell 5" "cell 2") lB `shouldBe` lA

    it "names the interrupt-or-shrink action, never a library or task" $ do
        let e = ResourceEvidence (4 * budget) [512000, 901000] 0
        case resourceLine budget (Just 0) e of
            Nothing -> expectationFailure "expected a resource line"
            Just line -> do
                T.isInfixOf "interrupt" (T.toLower line) `shouldBe` True
                T.isInfixOf "shrink" (T.toLower line) `shouldBe` True

    it "states the evidence it fired on" $ do
        let climbing = ResourceEvidence (2 * budget) [512000, 901000] 5
            silent = ResourceEvidence (2 * budget) [] 0
        case resourceLine budget (Just 1) climbing of
            Nothing -> expectationFailure "expected a resource line"
            Just line -> T.isInfixOf "heap climbing" line `shouldBe` True
        case resourceLine budget (Just 1) silent of
            Nothing -> expectationFailure "expected a resource line"
            Just line -> T.isInfixOf "no output" line `shouldBe` True
