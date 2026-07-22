{-# LANGUAGE OverloadedStrings #-}

{- | R9-T4 repair guard: 'goalFromErrorInCell' never yields a (name, goal) pair
when the not-in-scope name is one the submitted cell declares a signature for
without an equation — the topMonth false-goal, killed as a generated property,
not a replay. 'selfDeclaredSigs' supplies the names from the real parser.
-}
module Test.RepairGuardSpec (repairGuardSpec) where

import Control.Monad (forM_)
import Data.Text (Text)
import Test.Hspec

import Sabela.AI.HoleRepair (goalFromError)
import Siza.Agent.RepairGuard (goalFromErrorInCell, selfDeclaredSigs)

names :: [Text]
names = ["topMonth", "revenueTotal", "foo"]

repairGuardSpec :: Spec
repairGuardSpec = describe "repair guard (R9-T4)" $ do
    describe "selfDeclaredSigs — sig-without-equation from the real parser" $ do
        it "reports a bare signature name" $
            selfDeclaredSigs "topMonth :: String" `shouldBe` ["topMonth"]
        it "reports nothing once the equation is present" $
            selfDeclaredSigs "topMonth :: String\ntopMonth = \"x\""
                `shouldBe` []
        it "reports nothing for an equation with no signature" $
            selfDeclaredSigs "topMonth = \"x\"" `shouldBe` []
        it "reports a signature whose equation names another binding" $
            selfDeclaredSigs "foo :: Int\nbar = 1" `shouldBe` ["foo"]

    describe "goalFromErrorInCell — no producer hunt for a self-declared sig" $ do
        it "suppresses the target for every self-declared signature name" $
            forM_ names $ \n ->
                goalFromErrorInCell
                    [n]
                    ("Variable not in scope: " <> n <> " :: String")
                    `shouldBe` Nothing
        it "still yields a target for a name the cell does not self-declare" $
            forM_ names $ \n ->
                goalFromErrorInCell
                    ["somethingElse"]
                    ("Variable not in scope: " <> n <> " :: Int -> Int")
                    `shouldBe` Just (n, "Int -> Int")
        it "equals goalFromError when no names are self-declared" $
            forM_ names $ \n ->
                let err = "Variable not in scope: " <> n <> " :: Parser Double"
                 in goalFromErrorInCell [] err `shouldBe` goalFromError err
