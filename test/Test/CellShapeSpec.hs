{-# LANGUAGE OverloadedStrings #-}

{- | The pre-GHC structural validator (C2): at the cell-mutation boundary it
rejects obviously-wrong cell shapes — a top-level @let@ in a code cell and
Haskell code pasted into a 'ProseCell' — before they reach the compiler,
with a deterministic message (the @let@ one deduped against
'Sabela.Diagnose.letParse').
-}
module Test.CellShapeSpec (spec) where

import qualified Data.Text as T
import Sabela.Model (CellType (..))
import Sabela.Parse (validateCellShape)
import Test.Hspec

spec :: Spec
spec = describe "Sabela.Parse.validateCellShape" $ do
    describe "top-level let in a code cell is rejected" $ do
        it "rejects `let x = 1` with the deduped letParse message" $ do
            let r = validateCellShape CodeCell "let x = 1"
            r `shouldSatisfy` rejected
            fmap (T.isInfixOf "top-level `let`") r `shouldBe` Just True

        it "rejects an indented/multi-line cell whose first stmt is a let" $ do
            let r = validateCellShape CodeCell "let y = 2\nmain = print y"
            r `shouldSatisfy` rejected

    describe "code in a ProseCell is rejected" $ do
        it "rejects a value binding pasted into prose" $ do
            let r = validateCellShape ProseCell "x = 1"
            r `shouldSatisfy` rejected
            fmap (T.isInfixOf "ProseCell") r `shouldBe` Just True

        it "rejects a function definition pasted into prose" $
            validateCellShape ProseCell "square n = n * n"
                `shouldSatisfy` rejected

        it "rejects a data declaration pasted into prose" $
            validateCellShape ProseCell "data Foo = Bar | Baz"
                `shouldSatisfy` rejected

    describe "well-formed cells pass" $ do
        it "a plain value binding in a code cell passes" $
            validateCellShape CodeCell "x = 1" `shouldBe` Nothing

        it "a function definition in a code cell passes" $
            validateCellShape CodeCell "square n = n * n" `shouldBe` Nothing

        it "a `let ... in` expression in a code cell passes" $
            validateCellShape CodeCell "let x = 1 in x + 1" `shouldBe` Nothing

        it "a bare expression in a code cell passes" $
            validateCellShape CodeCell "print (1 + 2)" `shouldBe` Nothing

        it "markdown prose in a ProseCell passes" $
            validateCellShape ProseCell "# Heading\n\nSome explanatory prose."
                `shouldBe` Nothing

        it "an empty ProseCell passes" $
            validateCellShape ProseCell "   " `shouldBe` Nothing
  where
    rejected = maybe False (not . T.null)
