{-# LANGUAGE OverloadedStrings #-}

{- | The re-enter (health_gate) message must CONTRAST the wrong name with the
real candidates from the cell's own diagnostic — gemma re-submitted
@takeWhile1@ three times past GHC's own \"Perhaps use: takeWhileP\" hint when
the rail only said \"fix the error\".
-}
module Test.ReenterContrastSpec (reenterContrastSpec, reenterAlarmSpec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Siza.Agent.Messages (reenterAlarmMessage, reenterMessage)

-- | A shortened real gemma diagnostic: wrong name + GHC's did-you-mean.
diag :: Text
diag =
    T.unlines
        [ "cell 0, line 24: Variable not in scope:"
        , "  takeWhile1"
        , "    :: (Char -> Bool)"
        , "       -> ParsecT Void String Identity String"
        , "Perhaps use one of these:"
        , "  `takeWhileP' (imported from Text.Megaparsec),"
        , "  `takeWhile_' (imported from Text.Megaparsec)"
        ]

reenterContrastSpec :: Spec
reenterContrastSpec = describe "reenterMessage — wrong-vs-real contrast" $ do
    it "still names the red cells" $
        reenterMessage [(0, diag)] `shouldSatisfy` T.isInfixOf "0"
    it "contrasts the wrong name with the real candidates" $ do
        let msg = reenterMessage [(0, diag)]
        msg `shouldSatisfy` T.isInfixOf "takeWhile1"
        msg `shouldSatisfy` T.isInfixOf "takeWhileP"
    it "omits the contrast when the diagnostic has no did-you-mean" $ do
        let msg = reenterMessage [(1, "cell 1: some unrelated error")]
        msg `shouldSatisfy` T.isInfixOf "1"
        msg `shouldSatisfy` (not . T.isInfixOf "does not exist")

{- | G1: a compile-class ('Rejected') red cell reaching the health gate is now
structurally impossible, so it is reframed as an invariant alarm rather than
a routine fix-it nudge — 'reenterAlarmMessage' is the canary.
-}
reenterAlarmSpec :: Spec
reenterAlarmSpec = describe "reenterAlarmMessage — G1 invariant canary" $ do
    it "leads with an invariant-alarm line when a red cell is compile-class" $ do
        let msg = reenterAlarmMessage [(2, "cell 2: some error", True)]
        msg `shouldSatisfy` T.isInfixOf "INVARIANT ALARM"
        msg `shouldSatisfy` T.isInfixOf "2"

    it "still tells the model to fix the cell (not just report a bug and stop)" $ do
        let msg = reenterAlarmMessage [(2, "cell 2: some error", True)]
        msg `shouldSatisfy` T.isInfixOf "replace_cell_source"

    it "an ordinary runtime-red cell (not compile-class) keeps the routine framing" $ do
        let msg = reenterAlarmMessage [(3, "cell 3: divide by zero", False)]
        msg `shouldSatisfy` (not . T.isInfixOf "INVARIANT ALARM")
        msg `shouldSatisfy` T.isInfixOf "Not done yet"

    it "mixed batch: the alarm line names only the compile-class cells" $ do
        let msg =
                reenterAlarmMessage
                    [(2, "cell 2: some error", True), (3, "cell 3: divide by zero", False)]
        msg `shouldSatisfy` T.isInfixOf "INVARIANT ALARM: cell(s) 2"
        msg `shouldSatisfy` T.isInfixOf "3"
