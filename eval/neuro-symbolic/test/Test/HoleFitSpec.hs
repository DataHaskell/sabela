{-# LANGUAGE OverloadedStrings #-}

module Test.HoleFitSpec (spec) where

import Data.Text (Text)
import Test.Hspec

import Eval.HoleFit (goalFromError, parseFitNames, substituteName)

blob :: Text
blob =
    "<interactive>:1:1: error: [GHC-88464]\n\
    \    Found hole: _ :: DataFrame -> [Double]\n\
    \    Valid hole fits include\n\
    \      columnAsList :: Columnable a => Expr a -> DataFrame -> [a]\n\
    \        with columnAsList @Double\n\
    \        (imported from DataFrame)\n\
    \      toColumn :: Columnable a => DataFrame -> [a]\n\
    \    Valid refinement hole fits include\n\
    \      apply (_ :: b -> [Double])"

spec :: Spec
spec = describe "Eval.HoleFit (substitute-and-verify core)" $ do
    describe "goalFromError" $ do
        it "reads the wrong name and goal type from a printed not-in-scope error" $
            goalFromError
                "<interactive>:5:9: Variable not in scope: getCol :: DataFrame -> [Double]"
                `shouldBe` Just ("getCol", "DataFrame -> [Double]")

        it "reads a qualified wrong name" $
            goalFromError "Variable not in scope: D.getCol :: [Double]"
                `shouldBe` Just ("D.getCol", "[Double]")

        it "is Nothing for a bare not-in-scope with no printed type" $
            goalFromError "Not in scope: type constructor or class \8216Frame\8217"
                `shouldBe` Nothing

        it "is Nothing for benign output" $
            goalFromError "all good" `shouldBe` Nothing

    describe "parseFitNames" $ do
        it "keeps the plain fit names in order, dropping provenance and refinement" $
            parseFitNames blob `shouldBe` ["columnAsList", "toColumn"]

        it "excludes the refinement fit 'apply'" $
            ("apply" `elem` parseFitNames blob) `shouldBe` False

    describe "substituteName" $ do
        it "replaces the wrong name with the fit" $
            substituteName "getCol" "columnAsList" "total = sum (getCol df)"
                `shouldBe` "total = sum (columnAsList df)"

        it "replaces a qualified name as one token" $
            substituteName "D.getCol" "D.columnAsList" "xs = D.getCol df"
                `shouldBe` "xs = D.columnAsList df"

        it "does not corrupt a longer identifier that contains the wrong name" $
            substituteName "col" "column" "let colour = col"
                `shouldBe` "let colour = column"

        it "is a no-op when the names match" $
            substituteName "col" "col" "x = col y" `shouldBe` "x = col y"
