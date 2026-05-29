{-# LANGUAGE OverloadedStrings #-}

-- | Tests for 'cellNames' — what defs/uses the parser extracts.
module Test.TopoSpec.Names (spec) where

import qualified Data.Set as S
import Sabela.Topo
import Test.Hspec

spec :: Spec
spec = do
    describe "cellNames — variable scoping" $ do
        it "does not treat indented lines as definitions" $ do
            let (defs, _) = cellNames "let x = 1\n  let y = 2"
            defs `shouldBe` S.fromList ["x"]

        it "extracts multiple definitions from a multi-line cell" $ do
            let (defs, _) = cellNames "let a = 1\nlet b = 2"
            defs `shouldBe` S.fromList ["a", "b"]

        it "tracks data type definitions" $ do
            let (defs, _) = cellNames "data Foo = Bar | Baz"
            S.member "Foo" defs `shouldBe` True

        it "tracks type alias definitions" $ do
            let (defs, _) = cellNames "type Name = String"
            S.member "Name" defs `shouldBe` True

        it "tracks newtype definitions" $ do
            let (defs, _) = cellNames "newtype Wrapper = Wrap Int"
            S.member "Wrapper" defs `shouldBe` True

        it "tracks class definitions" $ do
            let (defs, _) = cellNames "class MyShow a where"
            S.member "MyShow" defs `shouldBe` True

        it "produces no defs from a comment-only cell" $ do
            let (defs, _) = cellNames "-- just a comment"
            defs `shouldBe` S.empty

        it "does not treat import lines as definitions" $ do
            let (defs, _) = cellNames "import Data.Map"
            defs `shouldBe` S.empty

        it "does not treat pragmas as definitions" $ do
            let (defs, _) = cellNames "{-# LANGUAGE OverloadedStrings #-}"
            defs `shouldBe` S.empty

        it "extracts monadic bind as a definition" $ do
            let (defs, _) = cellNames "x <- readFile \"a\""
            S.member "x" defs `shouldBe` True

        it "treats primed identifiers as distinct names" $ do
            let (defs, uses) = cellNames "x' = x + 1"
            S.member "x'" defs `shouldBe` True
            S.member "x" uses `shouldBe` True
            -- x' and x are separate
            S.member "x" defs `shouldBe` False

    describe "cellNames — literals and comments are not scanned" $ do
        it "does NOT pick up identifiers inside string literals" $ do
            let (_, uses) = cellNames "putStrLn \"foo bar baz\""
            S.member "foo" uses `shouldBe` False
            S.member "bar" uses `shouldBe` False
            S.member "baz" uses `shouldBe` False
            -- real use of putStrLn IS picked up
            S.member "putStrLn" uses `shouldBe` True

        it "does NOT pick up identifiers inside line comments" $ do
            let (defs, uses) = cellNames "y = 1 -- secretName is mentioned here"
            S.member "secretName" uses `shouldBe` False
            S.member "y" defs `shouldBe` True
            S.member "secretName" defs `shouldBe` False

        it "does NOT pick up identifiers inside block comments" $ do
            let (_, uses) =
                    cellNames "x = 1 {- old note about hiddenName -} + 2"
            S.member "hiddenName" uses `shouldBe` False

        it "still picks up real identifiers adjacent to literals" $ do
            let (_, uses) =
                    cellNames "main = putStrLn message >> print result"
            S.member "putStrLn" uses `shouldBe` True
            S.member "message" uses `shouldBe` True
            S.member "print" uses `shouldBe` True
            S.member "result" uses `shouldBe` True

        it "handles multi-line string literals without corrupting later defs" $ do
            let src =
                    "template :: String\n"
                        <> "template = \"defA uses defB\"\n"
                        <> "realDef = 42"
                (defs, uses) = cellNames src
            S.member "template" defs `shouldBe` True
            S.member "realDef" defs `shouldBe` True
            -- The 'defA' and 'defB' tokens inside the string must not
            -- become uses of real identifiers.
            S.member "defA" uses `shouldBe` False
            S.member "defB" uses `shouldBe` False

    describe "cellNames — function parameters are scope-local" $ do
        it "does NOT treat the inline param of `f x = ...` as a free use" $ do
            let (_, uses) = cellNames "isPrime x = x * 2"
            S.member "x" uses `shouldBe` False

        it "params are stripped across indented continuation lines" $ do
            let src =
                    "isPrime n\n"
                        <> "  | n < 2 = False\n"
                        <> "  | n == 2 = True\n"
                        <> "  | otherwise = all (\\d -> n `mod` d /= 0) [2..n-1]"
                (defs, uses) = cellNames src
            S.member "isPrime" defs `shouldBe` True
            -- n is the function parameter; it should not be recorded as a
            -- top-level use of something defined elsewhere.
            S.member "n" uses `shouldBe` False
            -- Real uses (from the body) are preserved.
            S.member "all" uses `shouldBe` True

        it "a FREE mention of `x` outside any binding is still a use" $ do
            -- 'double x = ...' binds x locally. 'main = print (double x)'
            -- uses x at the top level — it must remain in uses.
            let src =
                    "double x = x * 2\n"
                        <> "main = print (double x)"
                (defs, uses) = cellNames src
            S.member "double" defs `shouldBe` True
            S.member "main" defs `shouldBe` True
            S.member "x" uses `shouldBe` True

        it "multiple params on one line are all treated as local" $ do
            let (_, uses) = cellNames "combine a b c = a + b * c"
            S.member "a" uses `shouldBe` False
            S.member "b" uses `shouldBe` False
            S.member "c" uses `shouldBe` False
