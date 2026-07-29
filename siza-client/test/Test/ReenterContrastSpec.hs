{-# LANGUAGE OverloadedStrings #-}

module Test.ReenterContrastSpec (reenterContrastSpec, reenterAlarmSpec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Siza.Agent.Messages (reenterAlarmMessage, reenterMessage)

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

unshowableDiag :: Text
unshowableDiag =
    "cell 7, line 15: No instance for `Show\n\
    \                   sabela-notebook-0.2.0.0:Sabela.Notebook.Picture.Internal.Picture'\n\
    \  arising from a use of `print'"

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
    it "quotes each red cell's diagnostic so the model need not read_cell" $ do
        let msg = reenterMessage [(7, unshowableDiag)]
        msg `shouldSatisfy` T.isInfixOf "No instance for"
        msg `shouldSatisfy` T.isInfixOf "Picture"
    it "flattens the quoted diagnostic onto one line and clamps it" $ do
        let long = "cell 9: " <> T.replicate 80 "verylongtoken "
            msg = reenterMessage [(9, long)]
        msg `shouldSatisfy` (not . T.isInfixOf (T.unwords (T.words long)))
    it "names the unshowable wrap on the gate rail" $ do
        let msg = reenterMessage [(7, unshowableDiag)]
        msg `shouldSatisfy` T.isInfixOf "displayPicture"

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
