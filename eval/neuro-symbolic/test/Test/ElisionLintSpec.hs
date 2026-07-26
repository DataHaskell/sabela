{-# LANGUAGE OverloadedStrings #-}

module Test.ElisionLintSpec (spec) where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Eval.ElisionLint (loadBearingLine)
import Eval.TranscriptLint (LintIssue (..), lintMessages)

toolMsg :: Text -> Value
toolMsg c =
    object
        [ "role" .= ("tool" :: Text)
        , "tool_name" .= ("discover" :: Text)
        , "content" .= c
        ]

sigBlock :: Text
sigBlock =
    "Facts already established for the chart hunt this session.\n"
        <> "bars :: [(Text, Double)] -> Plot -> Text\n"
        <> "The granite package (installed) renders terminal charts."

proseBlock :: Text
proseBlock =
    "Facts already established for the chart hunt this session.\n"
        <> "The granite package renders terminal charts from lists.\n"
        <> "Its documentation lives in the module header."

stubFor :: Int -> Text -> Text
stubFor turn block =
    "[as established turn "
        <> T.pack (show turn)
        <> " (unchanged): "
        <> T.take 40 (T.strip (T.takeWhile (/= '\n') block))
        <> "\x2026]"

rules :: [Value] -> [Text]
rules = map liRule . lintMessages

spec :: Spec
spec = describe "elided-load-bearing-field lint (R8-T1 x R8.4)" $ do
    it "passes a standalone whole-block back-reference of a signature block" $
        rules [toolMsg sigBlock, toolMsg (stubFor 1 sigBlock)]
            `shouldNotSatisfy` elem "elided-load-bearing-field"
    it "passes the standalone back-ref beside surviving fresh content" $
        rules
            [ toolMsg sigBlock
            , toolMsg ("Write this candidate cell.\n\n" <> stubFor 1 sigBlock)
            ]
            `shouldNotSatisfy` elem "elided-load-bearing-field"
    it "flags a stub EMBEDDED in a larger block over a signature block" $
        rules
            [ toolMsg sigBlock
            , toolMsg
                ( "The producer signature is "
                    <> stubFor 1 sigBlock
                    <> " so paste it into the cell."
                )
            ]
            `shouldSatisfy` elem "elided-load-bearing-field"
    it "flags an embedded stub over a cabal-line block" $ do
        let cabalBlock =
                "Package facts held from the earlier search this session.\n"
                    <> "granite (hidden): -- cabal: build-depends: granite\n"
                    <> "Re-declare the dependency to expose the library."
        rules
            [ toolMsg cabalBlock
            , toolMsg ("Declare it via " <> stubFor 1 cabalBlock <> " first.")
            ]
            `shouldSatisfy` elem "elided-load-bearing-field"
    it "passes a stub whose established block was plain prose" $
        rules [toolMsg proseBlock, toolMsg (stubFor 1 proseBlock)]
            `shouldNotSatisfy` elem "elided-load-bearing-field"
    it "passes a load-bearing block re-transmitted verbatim (no stub)" $
        rules [toolMsg sigBlock, toolMsg sigBlock]
            `shouldNotSatisfy` elem "elided-load-bearing-field"
    it "passes a stub with no established block behind it" $
        rules [toolMsg (stubFor 1 sigBlock)]
            `shouldNotSatisfy` elem "elided-load-bearing-field"
    it "keys on the LATEST block under the anchor (identity semantics)" $ do
        rules
            [ toolMsg sigBlock
            , toolMsg proseBlock
            , toolMsg (stubFor 2 proseBlock)
            ]
            `shouldNotSatisfy` elem "elided-load-bearing-field"
    it "flags a stub sitting INSIDE a structured type field (R8-T1)" $ do
        let envelope =
                "{\"query\":\"defaultPlot\",\"state\":\"found\",\"hits\":"
                    <> "[{\"name\":\"Plot\",\"type\":\""
                    <> stubFor 1 sigBlock
                    <> "\"}]}"
        rules [toolMsg envelope]
            `shouldSatisfy` elem "elided-load-bearing-field"
    it "flags the escaped-JSON variant of a keyed stub" $ do
        let nested =
                "tool returned: {\\\"type\\\": \\\""
                    <> stubFor 1 sigBlock
                    <> "\\\"}"
        rules [toolMsg nested]
            `shouldSatisfy` elem "elided-load-bearing-field"
    it "passes the same stub in prose position" $ do
        let proseRef =
                "Earlier facts apply here. "
                    <> stubFor 1 proseBlock
                    <> " Nothing else changed this turn."
        rules [toolMsg proseRef]
            `shouldNotSatisfy` elem "elided-load-bearing-field"
    it "loadBearingLine: signature and cabal lines only" $ do
        loadBearingLine "bars :: [(Text, Double)] -> Plot -> Text"
            `shouldBe` True
        loadBearingLine "granite (hidden): build-depends: granite"
            `shouldBe` True
        loadBearingLine "The granite package renders terminal charts."
            `shouldBe` False
