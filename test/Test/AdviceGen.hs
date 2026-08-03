{-# LANGUAGE OverloadedStrings #-}

{- | Generators for advice-truth properties: a cell paired with what is true of
it by construction, and the real GHC diagnostic shapes, over arbitrary
identifiers, modules and packages.
-}
module Test.AdviceGen (
    GenCell (..),
    cabalLine,
    genCell,
    genDiagnostic,
    genLetParseError,
    genHiddenPackageError,
    genFramedDiagnostic,
    harnessFrameBinder,
    located,
) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck

import Test.HarnessGen (genConIdent, genIdent, genModuleName, genPackageName)

-- | A generated cell, alongside what is true of it by construction.
data GenCell = GenCell
    { cellText :: Text
    , cellDeclares :: [Text]
    , cellWritesLet :: Bool
    }
    deriving (Show)

{- | Binders that open with the letters of @let@ without being one. They are
the case a prefix test gets wrong.
-}
genLetLookalike :: Gen Text
genLetLookalike =
    ("let" <>) <$> elements ["ters", "Rec", "hal", "terFreq'", "Down", "s"]

genRhs :: Gen Text
genRhs =
    elements
        [ "Map.insert k v m"
        , "T.splitOn \",\" s"
        , "encode payload"
        , "pure ()"
        , "42"
        , "foldr (+) 0 xs"
        , "BS.pack []"
        ]

-- | A source line, paired with whether it really is a top-level @let@.
genLine :: Gen (Text, Bool)
genLine =
    oneof
        [ (\i r -> (i <> " = " <> r, False)) <$> genIdent <*> genRhs
        , (\i r -> (i <> " = " <> r, False)) <$> genLetLookalike <*> genRhs
        , (\i r -> ("let " <> i <> " = " <> r, True)) <$> genIdent <*> genRhs
        , (\i r -> (i <> " = let tmp = " <> r <> " in tmp", False))
            <$> genIdent
            <*> genRhs
        , (\m -> ("import qualified " <> m <> " as M", False)) <$> genModuleName
        , pure ("", False)
        ]

genCell :: Gen GenCell
genCell = do
    declared <-
        oneof
            [ pure []
            , (: []) <$> genPackageName
            , (\a b -> [a, b]) <$> genPackageName <*> genPackageName
            ]
    ls <- resize 6 (listOf1 genLine)
    pure
        GenCell
            { cellText = T.unlines (cabalLine declared <> map fst ls)
            , cellDeclares = declared
            , cellWritesLet = any snd ls
            }

cabalLine :: [Text] -> [Text]
cabalLine [] = []
cabalLine ps = ["-- cabal: build-depends: " <> T.intercalate ", " ps]

-- | Real GHC diagnostic shapes, over generated modules and packages.
genDiagnostic :: Gen Text
genDiagnostic =
    oneof
        [ genLetParseError
        , genEqualsParseError
        , (\m -> located ("Could not find module \8216" <> m <> "\8217"))
            <$> genModuleName
        , genHiddenPackageError =<< genPackageName
        , pure
            (located "Ambiguous type variable \8216a0\8217 arising from \8216show\8217")
        , pure
            (located "Couldn't match expected type \8216Int\8217 with \8216[Char]\8217")
        , (\i -> located ("Variable not in scope: " <> i)) <$> genIdent
        , genUnshowableError
        , pure "ld: warning: -keep_dwarf_unwind is obsolete"
        ]

-- | GHC refusing to print a value whose type has no Show instance.
genUnshowableError :: Gen Text
genUnshowableError = do
    ty <- genConIdent
    pure
        ( located
            ( "No instance for \8216Show "
                <> ty
                <> "\8217 arising from a use of \8216print\8217"
            )
        )

located :: Text -> Text
located body = "<interactive>:41:7: error: [GHC-88464]\n    " <> body

-- | The wrapper binder the compile gate submits the model's code inside.
harnessFrameBinder :: Text
harnessFrameBinder = "_sabelaGateProbe0"

{- | A diagnostic carrying a context frame for the harness's own wrapper, the
way GHC reports one when the gate submits the model's code inside it.
-}
genFramedDiagnostic :: Gen Text
genFramedDiagnostic = do
    err <- genDiagnostic
    pure
        ( err
            <> "\nIn an equation for \8216"
            <> harnessFrameBinder
            <> "\8217: "
            <> harnessFrameBinder
            <> " = x"
        )

genLetParseError :: Gen Text
genLetParseError =
    elements
        [ located "parse error on input \8216let\8217"
        , "<interactive>:3:1: error: [GHC-58481]\n\
          \    parse error on input `let'"
        ]

{- | GHC's own suggested fix for a parse error on @=@ names @let@ whatever the
cell wrote; it is the message the episode was misclassified from.
-}
genEqualsParseError :: Gen Text
genEqualsParseError =
    pure
        ( located
            "parse error on input \8216=\8217\n\
            \    Perhaps you need a 'let' in a 'do' block?"
        )

genHiddenPackageError :: Text -> Gen Text
genHiddenPackageError pkg = do
    m <- genModuleName
    pure
        ( "<no location info>: error: [GHC-87110]\n\
          \    Could not load module \8216"
            <> m
            <> "\8217.\n    It is a member of the hidden package \8216"
            <> pkg
            <> "-1.2.3\8217."
        )
