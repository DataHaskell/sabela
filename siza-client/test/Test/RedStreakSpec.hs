{-# LANGUAGE OverloadedStrings #-}

module Test.RedStreakSpec (redStreakSpec) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Siza.Agent.Streak (bumpStreak, streakContrast)

diag :: Text
diag =
    T.unlines
        [ "cell 0, line 25: Variable not in scope:"
        , "  takeWhile1"
        , "    :: (Char -> Bool)"
        , "       -> ParsecT Void String Identity String"
        , "Perhaps use one of these:"
        , "  `takeWhileP' (imported from Text.Megaparsec)"
        ]

redStreakSpec :: Spec
redStreakSpec = describe "repeated-red-signature contrast (intention)" $ do
    describe "bumpStreak — count consecutive identical diagnostics per cell" $ do
        it "counts a repeated diagnostic" $ do
            let (m1, c1) = bumpStreak Map.empty 0 diag
                (m2, c2) = bumpStreak m1 0 diag
                (_, c3) = bumpStreak m2 0 diag
            (c1, c2, c3) `shouldBe` (1, 2, 3)
        it "a changed diagnostic resets the streak" $ do
            let (m1, _) = bumpStreak Map.empty 0 diag
                (m2, _) = bumpStreak m1 0 diag
                (_, c) = bumpStreak m2 0 "cell 0: some different error"
            c `shouldBe` 1
        it "streaks are per cell" $ do
            let (m1, _) = bumpStreak Map.empty 0 diag
                (_, c) = bumpStreak m1 1 diag
            c `shouldBe` 1

    describe "streakContrast — fire once, exactly at the threshold" $ do
        it "is silent below the threshold" $ do
            streakContrast 1 diag `shouldBe` Nothing
            streakContrast 2 diag `shouldBe` Nothing
        it "fires the wrong-vs-real contrast at three" $
            case streakContrast 3 diag of
                Nothing -> expectationFailure "no contrast at threshold"
                Just line -> do
                    line `shouldSatisfy` T.isInfixOf "takeWhile1"
                    line `shouldSatisfy` T.isInfixOf "takeWhileP"
        it "does not re-fire past the threshold (once per streak)" $
            streakContrast 4 diag `shouldBe` Nothing
        it "is silent when the diagnostic has no did-you-mean" $
            streakContrast 3 "cell 0: Variable not in scope: frobnicate"
                `shouldBe` Nothing
