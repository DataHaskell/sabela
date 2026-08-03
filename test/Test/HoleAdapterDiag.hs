{-# LANGUAGE OverloadedStrings #-}

{- | The diagnostics GHC 9.12 really prints for a rejected argument, in each
of the renderings it uses. Nothing here draws anything: the shapes are the
compiler's, and the names in them are the caller's.
-}
module Test.HoleAdapterDiag (
    argMismatchCode,
    mismatchDiag,
    mismatchDiagAt,
    mismatchDiagSpan,
    ordinalWord,
    untaggedMismatchDiag,
    wrappedMismatchDiag,
) where

import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T

-- | GHC's structural code for a type mismatch.
argMismatchCode :: Int
argMismatchCode = 83865

{- | GHC's rejection of an argument's type, carrying its structural code. The
expected and actual types are the two ends the adapter hole must join.
-}
mismatchDiag :: Text -> Text -> Text -> Int -> Text -> Text
mismatchDiag = mismatchDiagAt (7, 15)

{- | The same rejection, over the span the compiler reported it at: the blamed
expression's own extent, which is what GHC prints for anything wider than one
column.
-}
mismatchDiagAt :: (Int, Int) -> Text -> Text -> Text -> Int -> Text -> Text
mismatchDiagAt (line, col) expected actual fn idx arg =
    mismatchDiagSpan
        (line, col)
        (line, col + T.length arg - 1)
        expected
        actual
        fn
        idx
        arg

{- | The rejection GHC reports across a span, in each spelling it uses: a range
on one line, and the bracketed pair it writes for a range across lines.
-}
mismatchDiagSpan ::
    (Int, Int) -> (Int, Int) -> Text -> Text -> Text -> Int -> Text -> Text
mismatchDiagSpan (line, col) end expected actual fn idx arg =
    T.unlines
        [ "<interactive>:"
            <> spanText (line, col) end
            <> ": error: [GHC-"
            <> T.pack (show argMismatchCode)
            <> "]"
        , "    \8226 Couldn't match expected type \8216"
            <> expected
            <> "\8217 with actual type \8216"
            <> actual
            <> "\8217"
        , "    \8226 In the "
            <> ordinalWord idx
            <> " argument of \8216"
            <> fn
            <> "\8217, namely \8216"
            <> arg
            <> "\8217"
        , "      In the expression: " <> fn <> " " <> arg
        , "  |"
        , T.pack (show line) <> " | " <> fn <> " " <> arg
        , "  |     ^^^^^^"
        ]

-- | A span as GHC writes one: a point, a range on a line, or one across lines.
spanText :: (Int, Int) -> (Int, Int) -> Text
spanText (l1, c1) (l2, c2)
    | l1 /= l2 =
        "(" <> num l1 <> "," <> num c1 <> ")-(" <> num l2 <> "," <> num c2 <> ")"
    | c2 > c1 = num l1 <> ":" <> num c1 <> "-" <> num c2
    | otherwise = num l1 <> ":" <> num c1
  where
    num = T.pack . show

-- | The same rejection from a compiler that printed no structural code.
untaggedMismatchDiag :: Text -> Text -> Text -> Int -> Text -> Text
untaggedMismatchDiag expected actual fn idx arg =
    T.replace
        ("error: [GHC-" <> T.pack (show argMismatchCode) <> "]")
        "error:"
        (mismatchDiag expected actual fn idx arg)

{- | GHC's rendering when the blame bullet runs long: the named expression
moves onto its own continuation line.
-}
wrappedMismatchDiag :: Text -> Text -> Text -> Int -> Text -> Text
wrappedMismatchDiag expected actual fn idx arg =
    T.unlines
        [ "<interactive>:7:15: error: [GHC-" <> T.pack (show argMismatchCode) <> "]"
        , "    \8226 Couldn't match expected type \8216"
            <> expected
            <> "\8217"
        , "                  with actual type \8216" <> actual <> "\8217"
        , "    \8226 In the "
            <> ordinalWord idx
            <> " argument of \8216"
            <> fn
            <> "\8217, namely"
        , "        \8216" <> arg <> "\8217"
        , "      In the expression: " <> fn <> " " <> arg
        ]

{- | GHC's word for an argument position, and its numeric form past ten, as
@speakNth@ prints them.
-}
ordinalWord :: Int -> Text
ordinalWord n
    | n >= 1 && n <= length ws = at ws (n - 1)
    | otherwise = T.pack (show n) <> "th"
  where
    ws =
        [ "first"
        , "second"
        , "third"
        , "fourth"
        , "fifth"
        , "sixth"
        , "seventh"
        , "eighth"
        , "ninth"
        , "tenth"
        ]

at :: [Text] -> Int -> Text
at xs i = fromMaybe "zz" (listToMaybe (drop i xs))
