{-# LANGUAGE OverloadedStrings #-}

module Test.ReadSourceArgsSpec (spec) where

import Data.Either (isLeft)
import Data.Text (Text)
import Test.Hspec

import Data.Aeson (object, (.=))
import Sabela.AI.Capabilities (needsKernel)
import Sabela.AI.Capabilities.ToolName (
    ToolName (ReadSource),
    parseToolName,
    primaryArgKey,
    toolWireName,
 )
import Sabela.AI.ReadSourceArgs (
    ReadSourceReq (..),
    parseReadSourceArgs,
 )

m :: Text
m = "Data.HodaTime.Instant"

spec :: Spec
spec = describe "read_source" $ do
    describe "the wire name" $ do
        it "is read_source, both ways" $ do
            toolWireName ReadSource `shouldBe` "read_source"
            parseToolName "read_source" `shouldBe` Just ReadSource
        it "folds a bare-string call onto module" $
            primaryArgKey ReadSource `shouldBe` Just "module"
        it "answers without a kernel" $
            needsKernel ReadSource `shouldBe` False

    describe "the argument grammar" $ do
        it "accepts a module alone" $
            parseReadSourceArgs (object ["module" .= m])
                `shouldBe` Right (ReadSourceReq m Nothing Nothing Nothing)
        it "accepts module with a name" $
            parseReadSourceArgs
                (object ["module" .= m, "name" .= ("difference" :: Text)])
                `shouldBe` Right
                    (ReadSourceReq m (Just "difference") Nothing Nothing)
        it "accepts module, package and version together" $
            parseReadSourceArgs
                ( object
                    [ "module" .= m
                    , "package" .= ("hodatime" :: Text)
                    , "version" .= ("0.2.2.1" :: Text)
                    ]
                )
                `shouldBe` Right
                    (ReadSourceReq m Nothing (Just "hodatime") (Just "0.2.2.1"))
        it "a missing module says required, not a format complaint" $
            parseReadSourceArgs (object ["name" .= ("difference" :: Text)])
                `shouldBe` Left "`module` required"
        it "rejects an empty module" $
            parseReadSourceArgs (object ["module" .= ("" :: Text)])
                `shouldSatisfy` isLeft
        it "rejects a lowercase or malformed module" $ do
            parseReadSourceArgs (object ["module" .= ("data.hodatime" :: Text)])
                `shouldSatisfy` isLeft
            parseReadSourceArgs (object ["module" .= ("../etc" :: Text)])
                `shouldSatisfy` isLeft
            parseReadSourceArgs (object ["module" .= ("Data HodaTime" :: Text)])
                `shouldSatisfy` isLeft
        it "rejects a malformed package name" $ do
            parseReadSourceArgs
                (object ["module" .= m, "package" .= ("a/b" :: Text)])
                `shouldSatisfy` isLeft
            parseReadSourceArgs
                (object ["module" .= m, "package" .= (".." :: Text)])
                `shouldSatisfy` isLeft
        it "rejects a version that is not dotted digits" $
            parseReadSourceArgs
                (object ["module" .= m, "version" .= ("latest" :: Text)])
                `shouldSatisfy` isLeft
        it "treats an empty optional as unstated, not illegal" $
            parseReadSourceArgs
                (object ["module" .= m, "name" .= ("" :: Text)])
                `shouldBe` Right (ReadSourceReq m Nothing Nothing Nothing)
