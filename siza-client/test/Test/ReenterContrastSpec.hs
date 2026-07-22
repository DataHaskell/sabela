{-# LANGUAGE OverloadedStrings #-}

{- | The re-enter (health_gate) message must CONTRAST the wrong name with the
real candidates from the cell's own diagnostic — gemma re-submitted
@takeWhile1@ three times past GHC's own \"Perhaps use: takeWhileP\" hint when
the rail only said \"fix the error\".
-}
module Test.ReenterContrastSpec (reenterContrastSpec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Siza.Agent.Messages (reenterMessage)

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
