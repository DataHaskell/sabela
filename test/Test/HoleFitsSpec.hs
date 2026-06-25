{-# LANGUAGE OverloadedStrings #-}

{- | Valid-hole-fits (D2): parse GHC's "Valid hole fits include" blob into a
list of @(name, type)@ pairs, and turn a not-in-scope error that already
carries a printed type into the @_ :: <type>@ goal we feed to the query.

The parser is pinned against a captured-real GHC 9.12 blob; it is
GHC-version sensitive, so a large unexpected diff signals the upstream
format moved.
-}
module Test.HoleFitsSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.Capabilities.Query (parseHoleFits)
import Sabela.Diagnose (holeFitGoal)
import Test.Hspec

{- | A real blob captured from GHCi 9.12.2 with the three D2 flags set, for
the goal @_ :: [Int] -> Int@ after @import Data.List@.
-}
realBlob :: Text
realBlob =
    T.unlines
        [ "<interactive>:3:1: error: [GHC-88464]"
        , "    \8226 Found hole: _ :: [Int] -> Int"
        , "    \8226 In the expression: _ :: [Int] -> Int"
        , "      In an equation for \8216it\8217: it = _ :: [Int] -> Int"
        , "    \8226 Relevant bindings include"
        , "        it :: [Int] -> Int (bound at <interactive>:3:1)"
        , "      Valid hole fits include"
        , "        genericLength :: forall i a. Num i => [a] -> i"
        , "          with genericLength @Int @Int"
        , "          (imported from \8216Data.List\8217"
        , "           (and originally defined in \8216GHC.Internal.Data.OldList\8217))"
        , "        product :: forall (t :: * -> *) a. (Foldable t, Num a) => t a -> a"
        , "          with product @[] @Int"
        , "          (imported from \8216Prelude\8217"
        , "           (and originally defined in \8216GHC.Internal.Data.Foldable\8217))"
        , "        sum :: forall (t :: * -> *) a. (Foldable t, Num a) => t a -> a"
        , "          with sum @[] @Int"
        , "          (imported from \8216Prelude\8217"
        , "           (and originally defined in \8216GHC.Internal.Data.Foldable\8217))"
        , "        length :: forall (t :: * -> *) a. Foldable t => t a -> Int"
        , "          with length @[] @Int"
        , "          (imported from \8216Prelude\8217"
        , "           (and originally defined in \8216GHC.Internal.Data.Foldable\8217))"
        , "        head :: forall a. GHC.Internal.Stack.Types.HasCallStack => [a] -> a"
        , "          with head @Int"
        , "          (imported from \8216Prelude\8217"
        , "           (and originally defined in \8216GHC.Internal.List\8217))"
        , "      Valid refinement hole fits include"
        , "        foldl1' (_ :: Int -> Int -> Int)"
        , "          where foldl1' :: forall a."
        , "                           GHC.Internal.Stack.Types.HasCallStack =>"
        , "                           (a -> a -> a) -> [a] -> a"
        , "          with foldl1' @Int"
        , "          (imported from \8216Data.List\8217"
        , "           (and originally defined in \8216GHC.Internal.List\8217))"
        ]

spec :: Spec
spec = describe "Sabela hole-fits (D2)" $ do
    describe "parseHoleFits (pinned to a real GHC 9.12 blob)" $ do
        let fits = parseHoleFits realBlob

        it "extracts every plain valid hole fit, in order" $
            map fst fits
                `shouldBe` ["genericLength", "product", "sum", "length", "head"]

        it "keeps the full type signature for each fit" $ do
            lookup "genericLength" fits
                `shouldBe` Just "forall i a. Num i => [a] -> i"
            lookup "length" fits
                `shouldBe` Just "forall (t :: * -> *) a. Foldable t => t a -> Int"

        it "stops at the refinement section (no refinement fits leak in)" $
            ("foldl1'" `elem` map fst fits) `shouldBe` False

        it "drops the with/imported-from continuation lines" $ do
            any (T.isPrefixOf "with" . fst) fits `shouldBe` False
            any (T.isInfixOf "imported" . snd) fits `shouldBe` False

        it "returns nothing for a blob without the header" $
            parseHoleFits "just some text\nno fits here" `shouldBe` []

    describe "holeFitGoal (not-in-scope -> queryable goal)" $ do
        it "turns a printed not-in-scope type into a `_ :: <type>` goal" $
            holeFitGoal
                "<interactive>:1:1: error: [GHC-88464]\n    Variable not in scope: foo :: Int -> Int"
                `shouldBe` Just "_ :: Int -> Int"

        it "handles a multi-arg constrained signature" $
            holeFitGoal "Variable not in scope: g :: Foldable t => t a -> Int"
                `shouldBe` Just "_ :: Foldable t => t a -> Int"

        it "yields nothing when the not-in-scope name has no printed type" $
            holeFitGoal "Variable not in scope: bar" `shouldBe` Nothing

        it "yields nothing for an unrelated error" $
            holeFitGoal "Couldn't match expected type \8216Int\8217" `shouldBe` Nothing
