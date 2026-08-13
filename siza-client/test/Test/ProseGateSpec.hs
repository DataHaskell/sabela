{-# LANGUAGE OverloadedStrings #-}

module Test.ProseGateSpec (proseGateSpec) where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import Siza.Cli (haskellCodePayload)
import Test.Hspec (Spec, describe, it, shouldBe)

insertPayload :: Text -> Text -> Value
insertPayload cellType lang =
    object
        [ "after_cell_id" .= (1 :: Int)
        , "cell_type" .= cellType
        , "language" .= lang
        , "source" .= ("Prose is not a parse error,," :: Text)
        ]

proseGateSpec :: Spec
proseGateSpec = describe "pre-flight gates only Haskell code payloads" $ do
    it "a ProseCell insert is never parsed as Haskell" $ do
        haskellCodePayload (insertPayload "ProseCell" "Haskell") `shouldBe` False
        haskellCodePayload (insertPayload "ProseCell" "Markdown") `shouldBe` False

    it "a Python insert is never parsed as Haskell" $
        haskellCodePayload (insertPayload "CodeCell" "Python") `shouldBe` False

    it "a Haskell code insert is gated" $
        haskellCodePayload (insertPayload "CodeCell" "Haskell") `shouldBe` True

    it "a payload that omits type and language is gated (legacy default)" $
        haskellCodePayload (object ["source" .= ("x = 1" :: Text)])
            `shouldBe` True
