{-# LANGUAGE OverloadedStrings #-}

module Test.AccessSpec (spec) where

import Network.Wai (Request, defaultRequest, queryString, requestHeaders)
import Test.Hspec

import Sabela.Server.Access (
    Access (..),
    isLoopbackHost,
    resolveAccess,
    tokenAllowed,
 )

reqWith :: Request
reqWith = defaultRequest

spec :: Spec
spec = do
    describe "isLoopbackHost" $ do
        it "treats localhost and 127.x as loopback" $ do
            isLoopbackHost "127.0.0.1" `shouldBe` True
            isLoopbackHost "127.0.1.1" `shouldBe` True
            isLoopbackHost "localhost" `shouldBe` True
            isLoopbackHost "::1" `shouldBe` True
        it "treats wildcard and LAN addresses as network binds" $ do
            isLoopbackHost "0.0.0.0" `shouldBe` False
            isLoopbackHost "*" `shouldBe` False
            isLoopbackHost "192.168.1.5" `shouldBe` False

    describe "resolveAccess" $
        it "leaves a loopback bind open" $
            resolveAccess "127.0.0.1" `shouldReturn` Right Open

    describe "tokenAllowed" $ do
        it "accepts a bearer header" $
            tokenAllowed
                "s3cret"
                reqWith{requestHeaders = [("Authorization", "Bearer s3cret")]}
                `shouldBe` True
        it "accepts the sabela_token cookie among others" $
            tokenAllowed
                "s3cret"
                reqWith{requestHeaders = [("Cookie", "a=1; sabela_token=s3cret; b=2")]}
                `shouldBe` True
        it "accepts a matching token query" $
            tokenAllowed "s3cret" reqWith{queryString = [("token", Just "s3cret")]}
                `shouldBe` True
        it "rejects a missing or wrong token" $ do
            tokenAllowed "s3cret" reqWith `shouldBe` False
            tokenAllowed
                "s3cret"
                reqWith{requestHeaders = [("Cookie", "sabela_token=nope")]}
                `shouldBe` False
            tokenAllowed "s3cret" reqWith{queryString = [("token", Just "s3cre")]}
                `shouldBe` False
