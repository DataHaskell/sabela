{-# LANGUAGE OverloadedStrings #-}

{- | Generators for the harness-truth properties: arbitrary Haskell
identifiers, modules and packages, and GHC diagnostics built from them. No
generator names a library the audited episode used, so a property that passes
here cannot have been fitted to one task.
-}
module Test.HarnessGen (
    Ident (..),
    ModuleName (..),
    PackageName (..),
    CellSource (..),
    genIdent,
    genConIdent,
    genModuleName,
    genPackageName,
    packageNames,
    genQualifiedName,
    genCellSource,
    genScopeDiagnostic,
    genAmbiguousDiagnostic,
    genMissingModuleDiagnostic,
    genParseErrorDiagnostic,
    genTypeErrorDiagnostic,
    genDiagnostic,
    genDefaultingWarning,
) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck

newtype Ident = Ident {unIdent :: Text} deriving (Eq, Show)
newtype ModuleName = ModuleName {unModuleName :: Text} deriving (Eq, Show)
newtype PackageName = PackageName {unPackageName :: Text} deriving (Eq, Show)
newtype CellSource = CellSource {unCellSource :: Text} deriving (Eq, Show)

instance Arbitrary Ident where
    arbitrary = Ident <$> genIdent

instance Arbitrary ModuleName where
    arbitrary = ModuleName <$> genModuleName

instance Arbitrary PackageName where
    arbitrary = PackageName <$> genPackageName

instance Arbitrary CellSource where
    arbitrary = CellSource <$> genCellSource

genIdent :: Gen Text
genIdent = do
    stem <- elements stems
    suffix <- elements ["", "'", "1", "s", "Of", "By", "_x"]
    pure (stem <> suffix)
  where
    stems =
        [ "acc"
        , "collect"
        , "counts"
        , "entries"
        , "fold"
        , "gather"
        , "grouped"
        , "keys"
        , "lookupAll"
        , "merge"
        , "nullish"
        , "pairs"
        , "reduce"
        , "rows"
        , "summarise"
        , "tally"
        , "toTable"
        , "widen"
        ]

genConIdent :: Gen Text
genConIdent =
    elements
        [ "Entry"
        , "Bucket"
        , "Tally"
        , "Summary"
        , "Row"
        , "Widen"
        , "Merge"
        , "Collect"
        ]

genModuleName :: Gen Text
genModuleName =
    elements
        [ "Data.Map.Strict"
        , "Data.Map"
        , "Data.Set"
        , "Data.IntMap"
        , "Data.Sequence"
        , "Data.Text"
        , "Data.Text.Lazy"
        , "Data.Aeson"
        , "Data.ByteString.Lazy"
        , "Control.Monad.State"
        , "Data.List.NonEmpty"
        , "Zither.Frobnicate"
        , "Quux.Internal.Bar"
        ]

genPackageName :: Gen Text
genPackageName = elements packageNames

{- | The package vocabulary these generators draw on. A property that asks
which packages a message names can only look for ones that could be there.
-}
packageNames :: [Text]
packageNames =
    [ "containers"
    , "text"
    , "aeson"
    , "mtl"
    , "bytestring"
    , "unordered-containers"
    , "zither"
    , "quux-internal"
    ]

genQualifiedName :: Gen Text
genQualifiedName = do
    m <- genModuleName
    n <- genIdent
    pure (m <> "." <> n)

{- | A cell of top-level bindings over generated identifiers. Half the time it
is deliberately unparseable, because that is exactly when repair runs.
-}
genCellSource :: Gen Text
genCellSource = do
    binders <- listOf1 genIdent
    bodies <- vectorOf (length binders) genBody
    broken <- arbitrary
    let decls = [b <> " = " <> body | (b, body) <- zip binders bodies]
        trailer = ["  where wrecked = ("]
    pure (T.unlines (decls <> if broken then trailer else []))
  where
    genBody = do
        f <- genQualifiedName
        a <- genIdent
        pure (f <> " " <> a)

genScopeDiagnostic :: Gen Text
genScopeDiagnostic = do
    n <- genIdent
    line <- choose (1 :: Int, 900)
    col <- choose (1 :: Int, 80)
    replacement <- oneof [genIdent, genConIdent, genQualifiedName]
    pure
        ( "<interactive>:"
            <> tshow line
            <> ":"
            <> tshow col
            <> ": error: [GHC-88464]\n    Variable not in scope: "
            <> n
            <> "\n    Suggested fix: Perhaps use \8216"
            <> replacement
            <> "\8217 (imported from Some.Module)"
        )

genAmbiguousDiagnostic :: Gen Text
genAmbiguousDiagnostic = do
    n <- genIdent
    m <- genModuleName
    line <- choose (1 :: Int, 900)
    pure
        ( "<interactive>:"
            <> tshow line
            <> ":1: error: [GHC-87543]\n    Ambiguous occurrence \8216"
            <> n
            <> "\8217\n    It could refer to\n       either \8216Prelude."
            <> n
            <> "\8217,\n           or \8216"
            <> m
            <> "."
            <> n
            <> "\8217"
        )

genMissingModuleDiagnostic :: Gen Text
genMissingModuleDiagnostic = do
    m <- genModuleName
    pure
        ( "<no location info>: error: [GHC-35235]\n    Could not find module \8216"
            <> m
            <> "\8217.\n    It is not a module in the current program."
        )

genParseErrorDiagnostic :: Gen Text
genParseErrorDiagnostic = do
    line <- choose (1 :: Int, 900)
    tok <- elements ["=", "in", "where", ")", "let"]
    pure
        ( "<interactive>:"
            <> tshow line
            <> ":5: error: [GHC-58481]\n    parse error on input \8216"
            <> tok
            <> "\8217"
        )

genTypeErrorDiagnostic :: Gen Text
genTypeErrorDiagnostic = do
    line <- choose (1 :: Int, 900)
    a <- genConIdent
    b <- genConIdent
    pure
        ( "<interactive>:"
            <> tshow line
            <> ":3: error: [GHC-83865]\n    Couldn't match expected type \8216"
            <> a
            <> "\8217 with actual type \8216"
            <> b
            <> "\8217"
        )

genDiagnostic :: Gen Text
genDiagnostic =
    oneof
        [ genScopeDiagnostic
        , genAmbiguousDiagnostic
        , genMissingModuleDiagnostic
        , genParseErrorDiagnostic
        , genTypeErrorDiagnostic
        ]

{- | A GHC @-Wtype-defaults@ warning over an arbitrary type variable and an
arbitrary defaulted type.
-}
genDefaultingWarning :: Gen Text -> Gen Text
genDefaultingWarning genTy = do
    var <- elements ["a0", "b1", "m0", "t2", "w0"]
    ty <- genTy
    line <- choose (1 :: Int, 900)
    pure
        ( "<interactive>:"
            <> tshow line
            <> ":1: warning: [GHC-18042] [-Wtype-defaults]\n\
               \    \8226 Defaulting the type variable \8216"
            <> var
            <> "\8217 to type \8216"
            <> ty
            <> "\8217 in the following constraints"
        )

tshow :: Int -> Text
tshow = T.pack . show
