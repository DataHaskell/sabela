{-# LANGUAGE OverloadedStrings #-}

{- | The deterministic sanitizers that meet a weak model's natural (imperfect)
tool inputs where they are: liberal enum parsing, top-level @let@ rewriting, and
detecting a tool call the model wrote as text.
-}
module Test.ToolInputRewriteSpec (spec) where

import Test.Hspec

import Data.Aeson (Value (..), object, (.=))

import Sabela.AI.Capabilities.ToolName (ToolName (..), resolveToolCall)
import Sabela.AI.Capabilities.Util (parseCellLang, parseCellType)
import Sabela.AI.Orchestrator.Loop (looksLikeToolCallText)
import Sabela.Model (CellType (..))
import Sabela.Parse.Normalize (rewriteTopLevelLet)
import Sabela.SessionTypes (CellLang (..))

spec :: Spec
spec = describe "tool-input sanitizers" $ do
    describe "parseCellType (liberal)" $ do
        it "accepts the schema enum and lowercase aliases" $ do
            parseCellType "CodeCell" `shouldBe` Just CodeCell
            parseCellType "code" `shouldBe` Just CodeCell
            parseCellType "CODE" `shouldBe` Just CodeCell
            parseCellType "prose" `shouldBe` Just ProseCell
            parseCellType "markdown" `shouldBe` Just ProseCell
        it "is Nothing for a truly unknown value" $
            parseCellType "widget" `shouldBe` Nothing

    describe "parseCellLang (liberal)" $ do
        it "accepts the schema enum and lowercase aliases" $ do
            parseCellLang "Haskell" `shouldBe` Just Haskell
            parseCellLang "haskell" `shouldBe` Just Haskell
            parseCellLang "hs" `shouldBe` Just Haskell
            parseCellLang "py" `shouldBe` Just Python
        it "is Nothing for a truly unknown value" $
            parseCellLang "rust" `shouldBe` Nothing

    describe "rewriteTopLevelLet" $ do
        it "rewrites a statement-form top-level let to a plain binding" $
            rewriteTopLevelLet "let xs = [1,2,3]" `shouldBe` "xs = [1,2,3]"
        it "leaves a let ... in ... expression alone" $
            rewriteTopLevelLet "let x = 1 in x + 1" `shouldBe` "let x = 1 in x + 1"
        it "leaves an indented (nested) let alone" $
            rewriteTopLevelLet "  let y = 2" `shouldBe` "  let y = 2"
        it "rewrites each single-line top-level let across lines" $
            rewriteTopLevelLet "let a = 1\nlet b = 2" `shouldBe` "a = 1\nb = 2"
        it "de-indents a multi-binding let block into top-level bindings" $
            rewriteTopLevelLet "let x = 1\n    y = 2\nz = x + y"
                `shouldBe` "x = 1\ny = 2\nz = x + y"
        it "leaves a let ... in ... block alone" $
            rewriteTopLevelLet "let x = 1 in x + 1" `shouldBe` "let x = 1 in x + 1"

    describe "resolveToolCall (name-baked argument)" $ do
        it "passes a well-formed call through untouched" $
            resolveToolCall "find_function" (object ["query" .= String "map"])
                `shouldBe` Just (FindFunction, object ["query" .= String "map"])
        it "splits an argument baked into the name into the primary field" $
            resolveToolCall "find_function \"DataFrame\"" (object [])
                `shouldBe` Just (FindFunction, object ["query" .= String "DataFrame"])
        it "routes the inline arg to the tool's own primary key" $
            resolveToolCall "describe_function fold" (object [])
                `shouldBe` Just (DescribeFunction, object ["name" .= String "fold"])
        it "keeps an explicit arg over the name-baked one" $
            resolveToolCall "find_function \"x\"" (object ["query" .= String "y"])
                `shouldBe` Just (FindFunction, object ["query" .= String "y"])
        it "is Nothing when the first token is not a tool" $
            resolveToolCall "frobnicate foo" (object []) `shouldBe` Nothing

    describe "looksLikeToolCallText" $ do
        it "flags a bare JSON object (a tool call written as text)" $
            looksLikeToolCallText "{\"query\":\"map\"}" `shouldBe` True
        it "does not flag ordinary prose" $
            looksLikeToolCallText "The sine wave is plotted." `shouldBe` False
        it "does not flag an empty object" $
            looksLikeToolCallText "{}" `shouldBe` False
