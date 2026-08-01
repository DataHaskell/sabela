{-# LANGUAGE OverloadedStrings #-}

{- | Generators shared by the harness-truth properties. Every input is drawn
from arbitrary Haskell identifiers, modules and packages, so no property
depends on the library, function or file format any one episode used.
-}
module Test.TruthGen (
    genIdent,
    genPrefixed,
    genDisjointNames,
    genPackage,
    genModuleName,
    genCheckOver,
    genGhcDiagnostic,
    genPreambleSource,
    genSubstantiveSource,
    genSource,
    genUnperturbableType,
    genPerturbableType,
) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck

-- | Reserved words a generated binder must never be, so a generated scope
-- name is always a name a check could legitimately mention.
keywords :: [Text]
keywords =
    [ "case"
    , "class"
    , "data"
    , "deriving"
    , "do"
    , "else"
    , "if"
    , "import"
    , "in"
    , "infix"
    , "infixl"
    , "infixr"
    , "instance"
    , "let"
    , "module"
    , "newtype"
    , "of"
    , "then"
    , "type"
    , "where"
    ]

genIdent :: Gen Text
genIdent = genPrefixed ""

-- | A lowercase Haskell identifier under a caller-chosen prefix. Prefixes are
-- how a property gets two name sets that are disjoint by construction.
genPrefixed :: Text -> Gen Text
genPrefixed prefix =
    (`suchThat` (`notElem` keywords)) $ do
        c <- elements ['a' .. 'z']
        n <- choose (0, 6)
        rest <- vectorOf n (elements identChars)
        pure (prefix <> T.pack (c : rest))
  where
    identChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "_'"

{- | Two name sets that cannot overlap: one standing for bindings that were
already in the notebook, one for bindings a turn caused to exist.
-}
genDisjointNames :: Int -> Int -> Gen ([Text], [Text])
genDisjointNames nUser nTask = do
    users <- distinct nUser (genPrefixed "u")
    tasks <- distinct nTask (genPrefixed "t")
    pure (users, tasks)
  where
    distinct n g = dedup <$> vectorOf (n * 2) g
      where
        dedup = take n . foldr keep []
        keep x acc = if x `elem` acc then acc else x : acc

genPackage :: Gen Text
genPackage =
    elements
        [ "containers"
        , "text"
        , "aeson"
        , "mtl"
        , "bytestring"
        , "zigzagulator"
        , "wibblesort"
        ]

genModuleName :: Gen Text
genModuleName = do
    parts <- choose (1, 3) >>= \n -> vectorOf n component
    pure (T.intercalate "." parts)
  where
    component = do
        c <- elements ['A' .. 'Z']
        n <- choose (1, 5)
        rest <- vectorOf n (elements ['a' .. 'z'])
        pure (T.pack (c : rest))

-- | A boolean check expression over one of the given names.
genCheckOver :: [Text] -> Gen Text
genCheckOver names = do
    n <- elements names
    op <- elements ["==", "/=", "<", ">"]
    lit <- T.pack . show <$> (arbitrary :: Gen Int)
    pure (n <> " " <> op <> " " <> lit)

-- | Text shaped like a GHC diagnostic, for a rejection payload.
genGhcDiagnostic :: Gen Text
genGhcDiagnostic = do
    n <- genIdent
    m <- genModuleName
    elements
        [ "Variable not in scope: " <> n
        , "Could not deduce (Show " <> n <> ")"
        , "Could not find module ‘" <> m <> "’"
        , "parse error on input ‘" <> n <> "’"
        ]

{- | A cell source that commits nothing executable: comments, pragmas, blank
lines and dependency declarations.
-}
genPreambleSource :: Gen Text
genPreambleSource = do
    p <- genPackage
    n <- genIdent
    elements
        [ ""
        , "   "
        , "-- " <> n
        , "-- cabal: build-depends: " <> p
        , "{-# LANGUAGE OverloadedStrings #-}"
        , "-- cabal: build-depends: " <> p <> "\n-- " <> n
        ]

-- | A cell source that binds something.
genSubstantiveSource :: Gen Text
genSubstantiveSource = do
    n <- genIdent
    m <- genIdent
    v <- T.pack . show <$> (arbitrary :: Gen Int)
    p <- genPackage
    mod' <- genModuleName
    elements
        [ n <> " = " <> v
        , n <> " = " <> m <> " " <> v
        , n <> " :: Int\n" <> n <> " = " <> v
        , "-- cabal: build-depends: " <> p <> "\nimport " <> mod' <> "\n" <> n <> " = " <> v
        ]

genSource :: Gen Text
genSource = oneof [genPreambleSource, genSubstantiveSource]

-- | Types the mutation gate can offer no perturbation for.
genUnperturbableType :: Gen Text
genUnperturbableType = do
    t <- genModuleName
    n <- genIdent
    elements
        [ t
        , "Map " <> n <> " " <> n
        , "IORef Int"
        , n <> " -> " <> n
        , "Maybe " <> t
        ]

genPerturbableType :: Gen Text
genPerturbableType = elements ["Int", "Integer", "Double", "String", "Text", "[Int]"]
