{-# LANGUAGE OverloadedStrings #-}

{- | Generated calls whose argument GHC rejected for its type, and the
diagnostics GHC 9.12 really prints for them. Every identifier, type and
position is drawn; nothing names a library or a format from any episode.
-}
module Test.HoleAdapterGen (
    ArgSource (..),
    genArgSource,
    genInfixCall,
    genOperator,
    genPrefixCall,
    genPrintedType,
    genQualified,
    genFive,
    genThree,
    genTwo,
    genTypeName,
    mismatchDiag,
    untaggedMismatchDiag,
    wrappedMismatchDiag,
    ordinalWord,
    argMismatchCode,
) where

import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck

import Test.HoleAdapterDiag (
    argMismatchCode,
    mismatchDiag,
    ordinalWord,
    untaggedMismatchDiag,
    wrappedMismatchDiag,
 )
import Test.HoleRewriteGen (genDistinct, genIdent)

{- | A generated call, which of its arguments the compiler rejected, and where
that argument really stands in the source.
-}
data ArgSource = ArgSource
    { asSrc :: Text
    , asFn :: Text
    , asCalled :: Text
    , asIndex :: Int
    , asArg :: Text
    , asSpan :: (Int, Int)
    }
    deriving (Eq, Show)

-- | A capitalised name, for a type or a module alias.
genTypeName :: Gen Text
genTypeName = do
    n <- choose (3, 7) :: Gen Int
    c <- elements ['A' .. 'Z']
    cs <- vectorOf n (elements ['a' .. 'z'])
    pure (T.pack (c : cs))

{- | A rejected call in any of the shapes a candidate writes one: applied as a
prefix head, or written infix. Every live rejection the campaign has measured
blamed an argument of an operator, so both arms carry equal weight.
-}
genArgSource :: Gen ArgSource
genArgSource = oneof [genPrefixCall, genInfixCall]

{- | A symbolic operator, drawn rather than named, so no property depends on
which operator any library happens to export.
-}
genOperator :: Gen Text
genOperator = do
    n <- choose (1, 3) :: Gen Int
    T.pack <$> vectorOf n (elements "<>+*$&|!^%?~/.")

{- | The infix shapes: a symbolic operator, one carrying a module qualifier, a
backticked function, and the two sections. GHC parenthesises a symbolic name in
its blame and leaves a backticked one bare, so both spellings are drawn.
-}
genInfixCall :: Gen ArgSource
genInfixCall = do
    names <- genDistinct 6
    let binder = at names 0
        fnName = at names 1
        plainLhs = at names 2
        plainRhs = at names 3
    op <- genOperator
    alias <- genTypeName
    form <- elements [0 .. 4 :: Int]
    idx <- choose (1, 2)
    trailing <- elements [True, False]
    applied <- elements [True, False]
    let written = case form of
            1 -> alias <> "." <> op
            2 -> "`" <> fnName <> "`"
            _ -> op
        called = if form == 2 then fnName else "(" <> op <> ")"
        position = case form of
            3 -> 1
            4 -> 2
            _ -> idx
        compound = at names 4 <> " " <> at names 5
        operand n plain = if applied && position == n then compound else plain
        lhs = operand 1 plainLhs
        rhs = operand 2 plainRhs
        body = case form of
            3 -> "(" <> lhs <> " " <> op <> ")"
            4 -> "(" <> op <> " " <> rhs <> ")"
            _ -> lhs <> " " <> written <> " " <> rhs
        arg = if position == 1 then lhs else rhs
        before = case (form, position) of
            (3, _) -> "("
            (4, _) -> "(" <> op <> " "
            (_, 1) -> ""
            _ -> lhs <> " " <> written <> " "
        assign = binder <> " = "
        col = T.length assign + T.length before + 1
        src = assign <> body <> (if trailing then "\n" else "")
    pure (ArgSource src called called position arg (1, col))

{- | The prefix shapes: one line, a qualified head, the argument also bound
above the call, the argument named in a trailing comment, and the argument's
text also standing inside a string literal.
-}
genPrefixCall :: Gen ArgSource
genPrefixCall = do
    names <- genDistinct 10
    let binder = at names 0
        other = at names 1
        fn = at names 2
        args = take 5 (drop 3 names)
    arity <- choose (1, 4) :: Gen Int
    idx <- choose (1, arity)
    alias <- genTypeName
    shape <- elements [0 .. 4 :: Int]
    trailing <- elements [True, False]
    applied <- elements [True, False]
    let compound = "(" <> at names 8 <> " " <> at names 9 <> ")"
        at' i a = if applied && i == idx then compound else a
        used = zipWith at' [1 ..] (take arity args)
        arg = at used (idx - 1)
        called = if shape == 1 then alias <> "." <> fn else fn
        callText = called <> " " <> T.unwords used
        callLine lead = lead <> callText
        assign = binder <> " = "
        argCol lead =
            T.length lead
                + T.length called
                + 1
                + sum [T.length a + 1 | a <- take (idx - 1) used]
                + 1
        (ls, row, pre) = case shape of
            2 ->
                ( ["  let " <> arg <> " = 0", "  " <> callLine assign]
                , 2
                , "  " <> assign
                )
            3 -> ([callLine assign <> "  -- " <> arg], 1, assign)
            4 ->
                ( [other <> " = \"" <> arg <> "\"", callLine assign]
                , 2
                , assign
                )
            _ -> ([callLine assign], 1, assign)
        src = T.intercalate "\n" ls <> (if trailing then "\n" else "")
    pure (ArgSource src fn called idx arg (row, argCol pre))

{- | Two printed types, in the shapes GHC prints one: an arrow, an
application, and a list of an application.
-}
genPrintedType :: Gen (Text, Text)
genPrintedType = do
    parts <- vectorOf 4 genTypeName
    shape <- elements [0 .. 2 :: Int]
    let mk a b = case shape of
            1 -> a <> " " <> b
            2 -> "[" <> a <> " " <> b <> "]"
            _ -> a <> " -> " <> b
    pure (mk (at parts 0) (at parts 1), mk (at parts 2) (at parts 3))

{- | A name GHC printed through a package it built from, and the module inside
that package the caller would have to import to write it.
-}
genQualified :: Gen (Text, Text)
genQualified = do
    words' <- vectorOf 2 genIdent
    digits <- vectorOf 3 (choose (0, 9 :: Int))
    segments <- vectorOf 2 genTypeName
    entity <- genTypeName
    let package = T.intercalate "-" words'
        version = T.intercalate "." (map (T.pack . show) digits)
        modulePath = T.intercalate "." segments
    pure
        (package <> "-" <> version <> ":" <> modulePath <> "." <> entity, modulePath)

at :: [Text] -> Int -> Text
at xs i = fromMaybe "zz" (listToMaybe (drop i xs))

-- | Distinct drawn names as a tuple, so no property matches on a list shape.
genTwo :: Gen (Text, Text)
genTwo = do
    xs <- genDistinct 2
    pure (at xs 0, at xs 1)

genThree :: Gen (Text, Text, Text)
genThree = do
    xs <- genDistinct 3
    pure (at xs 0, at xs 1, at xs 2)

genFive :: Gen (Text, Text, Text, Text, Text)
genFive = do
    xs <- genDistinct 5
    pure (at xs 0, at xs 1, at xs 2, at xs 3, at xs 4)
