{-# LANGUAGE OverloadedStrings #-}

module Test.OutputSpec (spec) where

import Sabela.Output (parseMimeOutputs)
import Test.Hspec

spec :: Spec
spec = describe "parseMimeOutputs" $ do
    it "plain text with no MIME markers returns text/plain" $ do
        parseMimeOutputs "hello\n" `shouldBe` [("text/plain", "hello\n")]

    it "empty/whitespace-only output returns nothing" $ do
        parseMimeOutputs "   \n  \n" `shouldBe` []

    it "single HTML block is parsed correctly" $ do
        let raw = "---MIME:text/html---\n<p>hi</p>\n"
        parseMimeOutputs raw `shouldBe` [("text/html", "<p>hi</p>\n")]

    it "two consecutive HTML blocks are returned as separate items" $ do
        let raw =
                "---MIME:text/html---\n<input type='range'>\n"
                    <> "---MIME:text/html---\n<p>result</p>\n"
        parseMimeOutputs raw
            `shouldBe` [ ("text/html", "<input type='range'>\n")
                       , ("text/html", "<p>result</p>\n")
                       ]

    it "mixed HTML then markdown are returned as separate items" $ do
        let raw =
                "---MIME:text/html---\n<p>hello</p>\n"
                    <> "---MIME:text/markdown---\n# Title\n"
        parseMimeOutputs raw
            `shouldBe` [ ("text/html", "<p>hello</p>\n")
                       , ("text/markdown", "# Title\n")
                       ]

    it "plain text before an HTML block splits into two items" $ do
        let raw = "plain output\n---MIME:text/html---\n<b>bold</b>\n"
        parseMimeOutputs raw
            `shouldBe` [ ("text/plain", "plain output\n")
                       , ("text/html", "<b>bold</b>\n")
                       ]

    it "slider + result: two html blocks stay separate at parse time" $ do
        -- mergeOutputs (JS) will later collapse them; parser should keep them split
        let raw =
                "---MIME:text/html---\n<input type='range' value='50'>\n"
                    <> "---MIME:text/html---\n<p>50 C = 122 F</p>\n"
        let result = parseMimeOutputs raw
        length result `shouldBe` 2
        fst (head result) `shouldBe` "text/html"
        fst (result !! 1) `shouldBe` "text/html"

    it "all supported MIME types are parsed" $ do
        let raw =
                "---MIME:text/html---\n<p>html</p>\n"
                    <> "---MIME:text/markdown---\n# md\n"
                    <> "---MIME:image/svg+xml---\n<svg/>\n"
                    <> "---MIME:text/latex---\nx^2\n"
                    <> "---MIME:application/json---\n{}\n"
        let result = parseMimeOutputs raw
        map fst result
            `shouldBe` ["text/html", "text/markdown", "image/svg+xml", "text/latex", "application/json"]
