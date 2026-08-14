{-# LANGUAGE OverloadedStrings #-}

{- | The top-level arrow split both gate renders share: what counts as a bind,
and where the pattern ends and the body begins.
-}
module Test.BindSpec (bindSpec) where

import Test.Hspec

import Sabela.AI.Capabilities.Edit.CompileGate.Bind (bindParts)

bindSpec :: Spec
bindSpec = describe "bindParts (the top-level arrow split both renders share)" $ do
    it "splits a plain bind into pattern and body" $
        bindParts "x <- readLn" `shouldBe` Just ("x", "readLn")

    it "keeps a tuple pattern whole" $
        bindParts "(a, b) <- pure (1, 2)"
            `shouldBe` Just ("(a, b)", "pure (1, 2)")

    it "ignores an arrow inside a comprehension" $
        bindParts "plot [(x, sin x) | x <- [0,0.01..(2*pi)]]"
            `shouldBe` Nothing

    it "ignores an arrow inside a string literal" $
        bindParts "putStrLn \"a <- b\"" `shouldBe` Nothing

    it "splits at the first top-level arrow only" $
        bindParts "ys <- pure [y | y <- [1,2]]"
            `shouldBe` Just ("ys", "pure [y | y <- [1,2]]")

    it "keeps a multi-line body together" $
        bindParts "df <- D.readCsv\n  \"./data.csv\""
            `shouldBe` Just ("df", "D.readCsv\n  \"./data.csv\"")

    it "reads a char literal without opening a quote span" $
        bindParts "y <- pure '<'" `shouldBe` Just ("y", "pure '<'")

    it "treats a prime in an identifier as part of the pattern" $
        bindParts "x' <- act" `shouldBe` Just ("x'", "act")
