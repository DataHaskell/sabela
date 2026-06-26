{-# LANGUAGE OverloadedStrings #-}

{- | Valid-hole-fits (D2): parse GHC's "Valid hole fits include" blob into a
list of @(name, type)@ pairs, and turn a not-in-scope error that already
carries a printed type into the @_ :: <type>@ goal we feed to the query.

The parser is pinned against a captured-real GHC 9.12 blob; it is
GHC-version sensitive, so a large unexpected diff signals the upstream
format moved.
-}
module Test.HoleFitsSpec (spec) where

import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.HoleFits (HoleFit (..), parseHoleFits)
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

{- | A harder real blob (GHC 9.12.2, same flags) for @_ :: Maybe Int -> Int@,
trimmed to fits that exercise multi-hole refinement skeletons and a refinement
type that wraps across three lines.
-}
multiHoleBlob :: Text
multiHoleBlob =
    T.unlines
        [ "<interactive>:2:1: error: [GHC-88464]"
        , "    \8226 Found hole: _ :: Maybe Int -> Int"
        , "      Valid hole fits include"
        , "        product :: forall (t :: * -> *) a. (Foldable t, Num a) => t a -> a"
        , "          with product @Maybe @Int"
        , "          (imported from \8216Prelude\8217"
        , "           (and originally defined in \8216GHC.Internal.Data.Foldable\8217))"
        , "        sum :: forall (t :: * -> *) a. (Foldable t, Num a) => t a -> a"
        , "          with sum @Maybe @Int"
        , "          (imported from \8216Prelude\8217"
        , "           (and originally defined in \8216GHC.Internal.Data.Foldable\8217))"
        , "      Valid refinement hole fits include"
        , "        foldr1 (_ :: Int -> Int -> Int)"
        , "          where foldr1 :: forall (t :: * -> *) a."
        , "                          Foldable t =>"
        , "                          (a -> a -> a) -> t a -> a"
        , "          with foldr1 @Maybe @Int"
        , "          (imported from \8216Prelude\8217"
        , "           (and originally defined in \8216GHC.Internal.Data.Foldable\8217))"
        , "        foldr (_ :: Int -> Int -> Int) (_ :: Int)"
        , "          where foldr :: forall (t :: * -> *) a b."
        , "                         Foldable t =>"
        , "                         (a -> b -> b) -> b -> t a -> b"
        , "          with foldr @Maybe @Int @Int"
        , "          (imported from \8216Prelude\8217"
        , "           (and originally defined in \8216GHC.Internal.Data.Foldable\8217))"
        ]

{- | A real blob (GHC 9.12.2, same flags) for
@_ :: (Int -> Maybe Int) -> [Int] -> Maybe [Int]@, where each plain fit's type
wraps across three lines, exercising the type-continuation fold. It carries no
refinement section, so it also covers the plain-only path.
-}
wrappedTypeBlob :: Text
wrappedTypeBlob =
    T.unlines
        [ "<interactive>:2:1: error: [GHC-88464]"
        , "    \8226 Found hole: _ :: (Int -> Maybe Int) -> [Int] -> Maybe [Int]"
        , "      Valid hole fits include"
        , "        mapM :: forall (t :: * -> *) (m :: * -> *) a b."
        , "                (Traversable t, Monad m) =>"
        , "                (a -> m b) -> t a -> m (t b)"
        , "          with mapM @[] @Maybe @Int @Int"
        , "          (imported from \8216Prelude\8217"
        , "           (and originally defined in \8216GHC.Internal.Data.Traversable\8217))"
        , "        traverse :: forall (t :: * -> *) (f :: * -> *) a b."
        , "                    (Traversable t, Applicative f) =>"
        , "                    (a -> f b) -> t a -> f (t b)"
        , "          with traverse @[] @Maybe @Int @Int"
        , "          (imported from \8216Prelude\8217"
        , "           (and originally defined in \8216GHC.Internal.Data.Traversable\8217))"
        ]

spec :: Spec
spec = describe "Sabela hole-fits (D2)" $ do
    describe "parseHoleFits (pinned to a real GHC 9.12 blob)" $ do
        let fits = parseHoleFits realBlob
            plain = filter (not . hfRefined) fits
            refined = filter hfRefined fits

        it "extracts every plain valid hole fit, in order" $
            map hfWrite plain
                `shouldBe` ["genericLength", "product", "sum", "length", "head"]

        it "keeps the full type signature for each plain fit" $ do
            (hfType <$> find ((== "genericLength") . hfWrite) plain)
                `shouldBe` Just "forall i a. Num i => [a] -> i"
            (hfType <$> find ((== "length") . hfWrite) plain)
                `shouldBe` Just "forall (t :: * -> *) a. Foldable t => t a -> Int"

        it "captures the refinement skeleton, its hole, and its type" $ do
            map hfWrite refined `shouldBe` ["foldl1' (_ :: Int -> Int -> Int)"]
            map hfType refined
                `shouldBe` [ "forall a. GHC.Internal.Stack.Types.HasCallStack =>"
                                <> " (a -> a -> a) -> [a] -> a"
                           ]

        it "tags refinement fits, and only those" $ do
            map hfRefined plain `shouldBe` replicate 5 False
            map hfRefined refined `shouldBe` [True]

        it "drops the with/imported-from continuation lines" $ do
            any (T.isPrefixOf "with" . hfWrite) fits `shouldBe` False
            any (T.isInfixOf "imported" . hfType) fits `shouldBe` False

        it "returns nothing for a blob without the header" $
            parseHoleFits "just some text\nno fits here" `shouldBe` []

    describe "parseHoleFits on harder real blobs" $ do
        describe "multi-hole refinement fits (_ :: Maybe Int -> Int)" $ do
            let fits = parseHoleFits multiHoleBlob
                plain = filter (not . hfRefined) fits
                refined = filter hfRefined fits
            it "keeps a plain fit's constrained polymorphic type" $
                (hfType <$> find ((== "product") . hfWrite) plain)
                    `shouldBe` Just "forall (t :: * -> *) a. (Foldable t, Num a) => t a -> a"
            it "captures one-hole and two-hole skeletons verbatim, in order" $
                map hfWrite refined
                    `shouldBe` [ "foldr1 (_ :: Int -> Int -> Int)"
                               , "foldr (_ :: Int -> Int -> Int) (_ :: Int)"
                               ]
            it "folds a refinement fit's three-line where-type" $
                (hfType <$> find (T.isPrefixOf "foldr (" . hfWrite) refined)
                    `shouldBe` Just
                        "forall (t :: * -> *) a b. Foldable t => (a -> b -> b) -> b -> t a -> b"

        describe "wrapped plain types (_ :: (Int -> Maybe Int) -> [Int] -> Maybe [Int])" $ do
            let fits = parseHoleFits wrappedTypeBlob
            it "folds each plain fit's wrapped forall and constraints" $ do
                map hfWrite fits `shouldBe` ["mapM", "traverse"]
                (hfType <$> find ((== "mapM") . hfWrite) fits)
                    `shouldBe` Just
                        ( "forall (t :: * -> *) (m :: * -> *) a b. (Traversable t, Monad m) =>"
                            <> " (a -> m b) -> t a -> m (t b)"
                        )
            it "has no refinement fits when the blob lacks that section" $
                filter hfRefined fits `shouldBe` []

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
