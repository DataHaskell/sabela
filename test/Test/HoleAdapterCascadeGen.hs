{-# LANGUAGE OverloadedStrings #-}

{- | Rejections the compiler printed more than one block for: a real cascade,
where one blamed expression stands inside another, and two separate rejections
that merely share a name. Every identifier is drawn.
-}
module Test.HoleAdapterCascadeGen (
    Cascade (..),
    CrossLine (..),
    Separate (..),
    genCascade,
    genCrossLine,
    genSeparate,
    genSeparateLate,
) where

import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck

import Test.HoleAdapterDiag (mismatchDiagAt, mismatchDiagSpan)
import Test.HoleAdapterGen (genOperator)
import Test.HoleRewriteGen (genDistinct)

{- | A rejection GHC printed two blocks for, where the second blames an
expression standing inside the one the first blames. Both are placeable, so
only the containment tells the root apart from what follows from it.
-}
data Cascade = Cascade
    { caSrc :: Text
    , caRoot :: (Text, Int, Text)
    , caInner :: Text
    , caDiagnostic :: Text
    , caRootUnplaceable :: Text
    }
    deriving (Eq, Show)

{- | Two rejections the compiler reported at different places, where the
expression one blames happens to stand inside the expression the other blames.
Neither follows from the other: they are separate errors that share a name.
-}
data Separate = Separate
    { spSrc :: Text
    , spFirst :: (Text, Int, Text)
    , spSecond :: (Text, Int, Text)
    , spDiagnostic :: Text
    }
    deriving (Eq, Show)

{- | A rejection the compiler reported over an expression running across two
source lines, with a second block blaming an expression inside it. Nothing can
be placed at the outer span, so the inner one must not be answered in its place.
-}
data CrossLine = CrossLine
    { clSrc :: Text
    , clDiagnostic :: Text
    }
    deriving (Eq, Show)

genCrossLine :: Gen CrossLine
genCrossLine = do
    names <- genDistinct 4
    let binder = at names 0
        outer = at names 1
        inner = at names 2
        sub = at names 3
        indent = "    "
        nested = "(" <> inner <> " " <> sub <> ")"
        src = binder <> " = " <> outer <> "\n" <> indent <> nested <> "\n"
        headCol = 1 + T.length (binder <> " = ")
        subCol = 1 + T.length (indent <> "(" <> inner <> " ")
        rootBlock =
            mismatchDiagSpan
                (1, headCol)
                (2, T.length indent + T.length nested)
                "Ee"
                "Aa"
                outer
                1
                nested
        innerBlock = mismatchDiagAt (2, subCol) "Ff" "Bb" inner 1 sub
    pure (CrossLine src (rootBlock <> "\n" <> innerBlock))

{- | Two rejections at different places where the contained expression is the
one reported later. Reading order alone would call it a knock-on; the spans say
the two do not overlap, so nothing may be published as following from the other.
-}
genSeparateLate :: Gen Separate
genSeparateLate = do
    names <- genDistinct 6
    let binderA = at names 0
        fnA = at names 1
        shared = at names 2
        binderB = at names 3
        fnB = at names 4
        nested = at names 5 <> " " <> shared
        src =
            binderA
                <> " = "
                <> fnA
                <> " ("
                <> nested
                <> ")\n"
                <> binderB
                <> " = "
                <> fnB
                <> " "
                <> shared
                <> "\n"
        colA = 1 + T.length (binderA <> " = " <> fnA <> " (")
        colB = 1 + T.length (binderB <> " = " <> fnB <> " ")
        blockA = mismatchDiagAt (1, colA) "Ee" "Aa" fnA 1 nested
        blockB = mismatchDiagAt (2, colB) "Ff" "Bb" fnB 1 shared
    pure
        ( Separate
            src
            (fnA, 1, nested)
            (fnB, 1, shared)
            (blockA <> "\n" <> blockB)
        )

genSeparate :: Gen Separate
genSeparate = do
    names <- genDistinct 6
    let binderA = at names 0
        fnA = at names 1
        shared = at names 2
        binderB = at names 3
        fnB = at names 4
        nested = at names 5 <> " " <> shared
        src =
            binderA
                <> " = "
                <> fnA
                <> " "
                <> shared
                <> "\n"
                <> binderB
                <> " = "
                <> fnB
                <> " ("
                <> nested
                <> ")\n"
        blockA = mismatchDiagAt (1, 6) "Ee" "Aa" fnA 1 shared
        blockB = mismatchDiagAt (2, 6) "Ff" "Bb" fnB 1 nested
    pure
        ( Separate
            src
            (fnA, 1, shared)
            (fnB, 1, nested)
            (blockA <> "\n" <> blockB)
        )

genCascade :: Gen Cascade
genCascade = do
    names <- genDistinct 6
    let binder = at names 0
        lhs = at names 1
        outer = at names 2
        inner = at names 3
        sub = at names 4
        elsewhere = at names 5
    op <- genOperator
    operatorHead <- elements [True, False]
    rootFirst <- elements [True, False]
    let nested = inner <> " " <> sub
        (called, idx, body) =
            if operatorHead
                then ("(" <> op <> ")", 2 :: Int, lhs <> " " <> op <> " " <> nested)
                else (outer, 1, outer <> " (" <> nested <> ")")
        src = binder <> " = " <> body <> "\n"
        columnOf t = (1, 1 + T.length (fst (T.breakOn t src)))
        rootBlock = mismatchDiagAt (columnOf nested) "Ee" "Aa" called idx nested
        innerBlock = mismatchDiagAt (columnOf sub) "Ff" "Bb" inner 1 sub
        blocks = if rootFirst then [rootBlock, innerBlock] else [innerBlock, rootBlock]
        reprinted =
            mismatchDiagAt
                (columnOf nested)
                "Ee"
                "Aa"
                called
                idx
                (nested <> " " <> elsewhere)
        unplaceable =
            T.intercalate
                "\n"
                (if rootFirst then [reprinted, innerBlock] else [innerBlock, reprinted])
    pure
        ( Cascade
            src
            (called, idx, nested)
            sub
            (T.intercalate "\n" blocks)
            unplaceable
        )

at :: [Text] -> Int -> Text
at xs i = fromMaybe "zz" (listToMaybe (drop i xs))
