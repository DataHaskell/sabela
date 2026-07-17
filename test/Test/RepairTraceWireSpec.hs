{-# LANGUAGE OverloadedStrings #-}

{- | The repair-counter JSONL shape. Pinned because the kill criterion is read
off these lines after a bench run: if the keys drift, a null result becomes
uninterpretable again.
-}
module Test.RepairTraceWireSpec (spec) where

import Data.Aeson (Value (..), decode, encode)
import qualified Data.Aeson.KeyMap as KM
import Sabela.AI.RepairTrace (
    RepairEvent (..),
    repairEventJSON,
    repairTracePath,
 )
import Test.Hspec

spec :: Spec
spec = describe "Sabela.AI.RepairTrace wire shape" $ do
    let ev = RepairEvent 2 [("arity", 2), ("holeSearch", 0)] (Just "arity")
        obj = case repairEventJSON ev of
            Object o -> o
            _ -> KM.empty

    it "carries the cell id" $
        KM.lookup "cellId" obj `shouldBe` Just (Number 2)

    it "carries a per-source count object, so 0 is distinguishable from absent" $
        KM.lookup "counts" obj
            `shouldBe` Just (Object (KM.fromList [("arity", Number 2), ("holeSearch", Number 0)]))

    it "names the winning source" $
        KM.lookup "winner" obj `shouldBe` Just (String "arity")

    it "emits winner null when nothing was selected (the selector-bug signal)" $
        let noWin = repairEventJSON (RepairEvent 1 [("arity", 3)] Nothing)
         in case noWin of
                Object o -> KM.lookup "winner" o `shouldBe` Just Null
                _ -> expectationFailure "expected an object"

    it "round-trips through JSON as one line" $
        decode (encode (repairEventJSON ev)) `shouldBe` Just (repairEventJSON ev)

    it "writes beside the notebook state, not the cwd" $
        repairTracePath "/tmp/wd" `shouldBe` "/tmp/wd/.sabela/repair-trace.jsonl"
