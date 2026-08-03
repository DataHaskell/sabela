{-# LANGUAGE OverloadedStrings #-}

module Test.BridgeGraphSpec (spec) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Sabela.Bridge (bridgeIdentifier)
import Sabela.Reactivity (bridgeConsumers, changedBridgeValues)
import Test.CellFixture (mkCell)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "changedBridgeValues" $ do
        let before = M.fromList [("x", "1"), ("y", "2")]
        it "sees nothing when the exports are identical" $
            changedBridgeValues before before `shouldBe` S.empty
        it "names only the value that changed, not the whole store" $
            changedBridgeValues before (M.insert "x" "9" before)
                `shouldBe` S.fromList ["x"]
        it "counts a newly exported value" $
            changedBridgeValues before (M.insert "z" "3" before)
                `shouldBe` S.fromList ["z"]
        it "counts a value that disappeared" $
            changedBridgeValues before (M.delete "y" before)
                `shouldBe` S.fromList ["y"]

    describe "bridgeConsumers" $ do
        let uses = mkCell 0 "total = _bridge_x + 1"
            other = mkCell 1 "label = _bridge_y"
            mentionsInString = mkCell 2 "note = \"_bridge_x is exported\""
            mentionsInComment = mkCell 3 "-- _bridge_x comes from python\nk = 1"
            cells = [uses, other, mentionsInString, mentionsInComment]
        it "picks the cell that uses the changed value" $
            bridgeConsumers (S.fromList ["x"]) cells `shouldBe` S.fromList [0]
        it
            "leaves consumers of an unchanged value alone, so one export does\
            \ not re-run every bridge cell"
            $ bridgeConsumers (S.fromList ["y"]) cells `shouldBe` S.fromList [1]
        it "ignores a mention inside a string literal" $
            S.member 2 (bridgeConsumers (S.fromList ["x"]) cells) `shouldBe` False
        it "ignores a mention inside a comment" $
            S.member 3 (bridgeConsumers (S.fromList ["x"]) cells) `shouldBe` False
        it "finds nothing when no export changed" $
            bridgeConsumers S.empty cells `shouldBe` S.empty

    describe "bridgeIdentifier" $
        it "is the one place the binding prefix is written" $
            bridgeIdentifier "x" `shouldBe` "_bridge_x"
