{-# LANGUAGE OverloadedStrings #-}

module Test.JsonDiagSpec (jsonDiagSpec) where

import Sabela.Session.GhcProbe (parseVersion, versionAtLeast)
import Test.Hspec

jsonDiagSpec :: Spec
jsonDiagSpec = describe "JSON-diagnostics version gate" $ do
    describe "parseVersion" $ do
        it "reads ghc --numeric-version output, trailing newline and all" $
            parseVersion "9.12.2\n" `shouldBe` [9, 12, 2]
        it "handles a two-component version" $
            parseVersion "9.8" `shouldBe` [9, 8]
        it "is empty for a non-numeric string" $
            parseVersion "" `shouldBe` []

    describe "versionAtLeast [9,8]" $ do
        it "accepts the current 9.12.2" $
            versionAtLeast [9, 8] [9, 12, 2] `shouldBe` True
        it "accepts the first supported release 9.8.1" $
            versionAtLeast [9, 8] [9, 8, 1] `shouldBe` True
        it "rejects 9.6.5 (no -fdiagnostics-as-json)" $
            versionAtLeast [9, 8] [9, 6, 5] `shouldBe` False
        it "rejects an unparseable (empty) version" $
            versionAtLeast [9, 8] [] `shouldBe` False
