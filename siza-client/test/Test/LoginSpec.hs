{-# LANGUAGE OverloadedStrings #-}

{- | The @siza login@ transport guard: a token is only sent over HTTPS, or over
plain HTTP when the hub is loopback (local dev). Pins 'isSecureHub' so a future
change can't silently start shipping the token in cleartext.
-}
module Test.LoginSpec (loginSpec) where

import Siza.Login (isSecureHub)
import Test.Hspec

loginSpec :: Spec
loginSpec = describe "Siza.Login.isSecureHub" $ do
    it "accepts https hubs" $
        isSecureHub "https://sabela.datahaskell.com" `shouldBe` True

    it "accepts loopback over http (local dev)" $ do
        isSecureHub "http://localhost:8080" `shouldBe` True
        isSecureHub "http://127.0.0.1:3000" `shouldBe` True

    it "rejects a non-loopback http hub (cleartext token)" $ do
        isSecureHub "http://sabela.datahaskell.com" `shouldBe` False
        isSecureHub "http://192.168.1.10:8080" `shouldBe` False
