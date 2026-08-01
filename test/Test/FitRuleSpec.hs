{-# LANGUAGE OverloadedStrings #-}

{- | Verbatim fits GHC returned for `_ :: Picture -> Picture -> Picture`. Two of
the eight are the answer; the rest inhabit the goal without depending on
anything specific to it, and only GHC's `with f @T` line separates them.
-}
module Test.FitRuleSpec (spec) where

import Data.Text (Text)
import Test.Hspec

import Sabela.AI.FitRule (Freeness (..), classifyFit, informativeFits)
import Sabela.AI.HoleFits (HoleFit (..), parseHoleFits)

pictureGoal :: Text
pictureGoal =
    "Valid hole fits include\n\
    \  mappend :: forall a. Monoid a => a -> a -> a\n\
    \    with mappend @Picture\n\
    \  mempty :: forall a. Monoid a => a\n\
    \    with mempty @(Picture -> Picture -> Picture)\n\
    \  (<>) :: forall a. Semigroup a => a -> a -> a\n\
    \    with (<>) @Picture\n\
    \  asTypeOf :: forall a. a -> a -> a\n\
    \    with asTypeOf @Picture\n\
    \  const :: forall a b. a -> b -> a\n\
    \    with const @Picture @Picture\n\
    \  return :: forall (m :: Type -> Type) a. Monad m => a -> m a\n\
    \    with return @((->) Picture) @Picture\n\
    \  pure :: forall (f :: Type -> Type) a. Applicative f => a -> f a\n\
    \    with pure @((->) Picture) @Picture\n\
    \  seq :: forall a b. a -> b -> b\n\
    \    with seq @Picture @Picture"

-- | Verbatim from `_ :: [Int] -> Int`, where the rule must do nothing at all.
listGoal :: Text
listGoal =
    "Valid hole fits include\n\
    \  head :: forall a. HasCallStack => [a] -> a\n\
    \    with head @Int\n\
    \  last :: forall a. HasCallStack => [a] -> a\n\
    \    with last @Int\n\
    \  length :: forall (t :: Type -> Type) a. Foldable t => t a -> Int\n\
    \    with length @[] @Int"

-- | Verbatim from `_ :: Canvas -> Double`: monomorphic, so no `with` line.
recordGoal :: Text
recordGoal =
    "Valid hole fits include\n\
    \  canvasHeight :: Canvas -> Double\n\
    \  canvasWidth :: Canvas -> Double"

named :: Text -> [Text]
named = map hfWrite . informativeFits . parseHoleFits

spec :: Spec
spec = describe "a fit that inhabits any goal of its shape says nothing about this one" $ do
    describe "the with-line is captured, because nothing else separates the fits" $ do
        it "records each type application GHC reported" $
            map hfAppliedAt (take 2 (parseHoleFits pictureGoal))
                `shouldBe` [["Picture"], ["(Picture -> Picture -> Picture)"]]

        it "is empty for a monomorphic fit, which has no application" $
            map hfAppliedAt (parseHoleFits recordGoal) `shouldBe` [[], []]

    describe "the rule, on the goal that started this" $ do
        it "keeps exactly the two fits a person would write" $
            named pictureGoal `shouldBe` ["mappend", "(<>)"]

        it "drops the projections, which ignore an argument" $ do
            let byName n = head [f | f <- parseHoleFits pictureGoal, hfWrite f == n]
            classifyFit (byName "const") `shouldBe` FreeProjection
            classifyFit (byName "seq") `shouldBe` FreeProjection
            classifyFit (byName "asTypeOf") `shouldBe` FreeProjection

        it "drops the fits whose constraint is discharged at a function type" $ do
            let byName n = head [f | f <- parseHoleFits pictureGoal, hfWrite f == n]
            classifyFit (byName "mempty") `shouldBe` FreeArrow
            classifyFit (byName "return") `shouldBe` FreeArrow
            classifyFit (byName "pure") `shouldBe` FreeArrow

        it "keeps a constraint discharged at the goal's own type" $ do
            let byName n = head [f | f <- parseHoleFits pictureGoal, hfWrite f == n]
            classifyFit (byName "mappend") `shouldBe` Informative
            classifyFit (byName "(<>)") `shouldBe` Informative

    describe "the rule is a no-op where every fit is real" $ do
        it "keeps all three list fits, including head and last" $
            named listGoal `shouldBe` ["head", "last", "length"]

        it "keeps both record accessors" $
            named recordGoal `shouldBe` ["canvasHeight", "canvasWidth"]

    it "drops undefined, which inhabits every goal" $ do
        let blob =
                "Valid hole fits include\n\
                \  undefined :: forall a. HasCallStack => a\n\
                \    with undefined @Picture"
        named blob `shouldBe` []
