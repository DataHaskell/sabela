{-# LANGUAGE OverloadedStrings #-}

module Test.EditSignificanceSpec (spec) where

import Sabela.Parse.Change (significantCodeChange)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "significantCodeChange (does an edit change what the kernel sees?)" $ do
    describe "insignificant: comments and layout" $ do
        it "an appended comment line is not a change" $
            significantCodeChange "x = 1" "x = 1\n-- probe" `shouldBe` False

        it "an inline trailing comment is not a change" $
            significantCodeChange "x = 1" "x = 1 -- note" `shouldBe` False

        it "a block comment is not a change" $
            significantCodeChange "x = 1" "{- header -}\nx = 1" `shouldBe` False

        it "blank-line reflow is not a change" $
            significantCodeChange "x = 1\ny = 2" "x = 1\n\n\ny = 2" `shouldBe` False

        it "trailing whitespace is not a change" $
            significantCodeChange "x = 1" "x = 1   \n" `shouldBe` False

        it "identical sources are not a change" $
            significantCodeChange "x = 1" "x = 1" `shouldBe` False

        it "a comment after a trailing do block is not a change" $
            significantCodeChange
                "do\n  print 1\n  print 2"
                "do\n  print 1\n  print 2\n-- probe"
                `shouldBe` False

        it "a comment after nested trailing layout is not a change" $
            significantCodeChange
                "f = go\n  where\n    go = do\n      print 1"
                "f = go\n  where\n    go = do\n      print 1\n-- probe"
                `shouldBe` False

        it "a comment edit in a bare-expression cell is not a change" $
            significantCodeChange
                "df |> whatever\n-- old note"
                "df |> whatever\n-- new note"
                `shouldBe` False

    describe "significant: code" $ do
        it "a changed literal is a change" $
            significantCodeChange "x = 1" "x = 2" `shouldBe` True

        it "a new binding is a change" $
            significantCodeChange "x = 1" "x = 1\nprobeNew = 9" `shouldBe` True

        it "a removed binding is a change" $
            significantCodeChange "x = 1\ny = 2" "x = 1" `shouldBe` True

        it "an edit inside a string literal is a change, even one that looks\
           \ like a comment" $
            significantCodeChange "s = \"a\"" "s = \"a -- b\"" `shouldBe` True

        it "a bare-expression edit is a change" $
            significantCodeChange "df |> D.take 5" "df |> D.take 6" `shouldBe` True

    describe "significant: directives that live in comments" $ do
        it "an added -- cabal: dependency is a change" $
            significantCodeChange
                "x = 1"
                "-- cabal: build-depends: aeson\nx = 1"
                `shouldBe` True

        it "an added -- compile directive is a change" $
            significantCodeChange "x = 1" "-- compile\nx = 1" `shouldBe` True

        it "a removed -- compile directive is a change" $
            significantCodeChange "-- compile\nx = 1" "x = 1" `shouldBe` True

        it "renaming a -- compile: module is a change" $
            significantCodeChange
                "-- compile: A\nx = 1"
                "-- compile: B\nx = 1"
                `shouldBe` True

    describe "fallback: sources the lexer cannot read" $ do
        it "two different unlexable sources count as a change" $
            significantCodeChange "x = \"unterminated" "y = \"unterminated"
                `shouldBe` True

        it "an identical unlexable source is not a change" $
            significantCodeChange "x = \"unterminated" "x = \"unterminated"
                `shouldBe` False
