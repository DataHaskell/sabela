{-# LANGUAGE OverloadedStrings #-}

{- | The enrichment a read of an errored cell carries. GHC already puts the goal
type in the message for the two commonest error classes, so the goal costs no
compile; only the fits do.
-}
module Test.ReadCellFitsSpec (spec) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import Test.Hspec

import Sabela.AI.Capabilities.Notebook (cellGoal, goalPairs)

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (Key.fromText k) o
field _ _ = Nothing

spec :: Spec
spec = describe "reading an errored cell states the goal it needs filled" $ do
    it "takes the name and type straight from the message, with no compile" $
        cellGoal
            "cell 8, line 1: Variable not in scope: combine :: Picture -> Picture -> Picture"
            `shouldBe` Just ("combine", "Picture -> Picture -> Picture")

    it "reads the qualified form too" $
        cellGoal
            "Not in scope: \8216DataFrame.readParquet\8217 :: FilePath -> IO DataFrame"
            `shouldBe` Just ("DataFrame.readParquet", "FilePath -> IO DataFrame")

    it "has no goal when the error names no type" $
        cellGoal "cell 3, line 2: parse error on input \8216]\8217" `shouldBe` Nothing

    it "has no goal for a cell that never errored" $
        cellGoal "" `shouldBe` Nothing

    it "states the goal as a fact, with no instruction attached" $
        case goalPairs (Just ("combine", "Picture -> Picture -> Picture")) [] of
            ps -> case field "goal" (Object (KM.fromList ps)) of
                Just (String g) -> do
                    g `shouldBe` "combine :: Picture -> Picture -> Picture"
                _ -> expectationFailure "expected the goal to be stated"

    it "adds nothing at all when there is no goal" $
        goalPairs Nothing [] `shouldBe` []
