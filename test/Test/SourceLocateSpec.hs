{-# LANGUAGE OverloadedStrings #-}

module Test.SourceLocateSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Sabela.AI.SourceLocate (
    DeclSlice (..),
    Located (..),
    Outline (..),
    declSlice,
    moduleOutline,
 )
import Sabela.AI.SourceLocate.Imports (moduleAliases)

plain :: Text
plain =
    T.unlines
        [ "module Probe (difference, add) where" -- 1
        , "" -- 2
        , "import Data.List (sort)" -- 3
        , "" -- 4
        , "-- | The gap between two values." -- 5
        , "difference :: Int -> Int -> Int" -- 6
        , "difference a b = a - b" -- 7
        , "" -- 8
        , "add :: Int -> Int -> Int" -- 9
        , "add a 0 = a" -- 10
        , "add a b = a + b" -- 11
        , "" -- 12
        , "data Shape = Circle Int | Square Int" -- 13
        , "" -- 14
        , "class Sized a where" -- 15
        , "    size :: a -> Int" -- 16
        ]

withPragma :: Text
withPragma =
    T.unlines
        [ "{-# LANGUAGE TypeFamilies #-}"
        , "module Probe where"
        , "type family Elem c"
        ]

headerless :: Text
headerless =
    T.unlines
        [ "one :: Int"
        , "one = 1"
        ]

unknownPragma :: Text
unknownPragma =
    T.unlines
        [ "{-# LANGUAGE NotARealExtension #-}"
        , "module Probe where"
        , "two :: Int"
        , "two = 2"
        ]

withCpp :: Text
withCpp =
    T.unlines
        [ "{-# LANGUAGE CPP #-}" -- 1
        , "module Probe where" -- 2
        , "#if MIN_VERSION_base(4,18,0)" -- 3
        , "gap :: Int -> Int" -- 4
        , "gap x = x - 1" -- 5
        , "#else" -- 6
        , "gap x = x" -- 7
        , "#endif" -- 8
        ]

spec :: Spec
spec = describe "locating a definition in module source" $ do
    describe "a module that parses" $ do
        it "slices the signature through the last equation" $
            case declSlice plain "add" of
                Right s -> do
                    dsHow s `shouldBe` Parsed
                    dsFrom s `shouldBe` 9
                    dsTo s `shouldBe` 11
                    dsText s `shouldSatisfy` T.isInfixOf "add a 0 = a"
                Left cs -> expectationFailure (show cs)
        it "slices one-equation defs at their own two lines" $
            case declSlice plain "difference" of
                Right s -> (dsFrom s, dsTo s) `shouldBe` (6, 7)
                Left cs -> expectationFailure (show cs)
        it "a constructor answers with its data declaration" $
            case declSlice plain "Circle" of
                Right s -> dsText s `shouldSatisfy` T.isInfixOf "data Shape"
                Left cs -> expectationFailure (show cs)
        it "a class method answers with its class declaration" $
            case declSlice plain "size" of
                Right s -> dsText s `shouldSatisfy` T.isInfixOf "class Sized"
                Left cs -> expectationFailure (show cs)
        it "a miss names the nearest declared names" $
            case declSlice plain "diference" of
                Left cs -> take 1 cs `shouldBe` ["difference"]
                Right _ -> expectationFailure "expected a miss"

    describe "LANGUAGE pragmas in the file" $ do
        it "honours an extension the baseline lacks" $
            case declSlice withPragma "Elem" of
                Right s -> dsHow s `shouldBe` Parsed
                Left cs -> expectationFailure (show cs)
        it "skips a pragma name it does not know and still parses" $
            case declSlice unknownPragma "two" of
                Right s -> dsHow s `shouldBe` Parsed
                Left cs -> expectationFailure (show cs)

    describe "a file that cannot parse (CPP)" $ do
        it "degrades to a lexical slice rather than an error" $
            case declSlice withCpp "gap" of
                Right s -> do
                    dsHow s `shouldBe` Scanned
                    dsFrom s `shouldBe` 4
                    dsText s `shouldSatisfy` T.isInfixOf "gap x = x - 1"
                Left cs -> expectationFailure (show cs)
        it "still misses honestly with candidates" $
            case declSlice withCpp "gapp" of
                Left cs -> cs `shouldSatisfy` elem "gap"
                Right _ -> expectationFailure "expected a miss"

    describe "the module outline" $ do
        it "carries the header and each decl's name and line" $ do
            let o = moduleOutline plain
            oHow o `shouldBe` Parsed
            oHeader o `shouldSatisfy` T.isInfixOf "module Probe"
            lookup "difference" (outlinePairs o) `shouldBe` Just 6
            lookup "Shape" (outlinePairs o) `shouldBe` Just 13
        it "carries each decl's signature when the source states one" $ do
            let o = moduleOutline plain
            lookup "add" [(n, s) | (n, _, s) <- oDecls o]
                `shouldBe` Just (Just "add :: Int -> Int -> Int")
        it "a module-less file outlines with an empty header" $ do
            let o = moduleOutline headerless
            oHeader o `shouldBe` ""
            map fst (outlinePairs o) `shouldSatisfy` elem "one"
        it "answers for an unparseable file too" $ do
            let o = moduleOutline withCpp
            oHow o `shouldBe` Scanned
            map fst (outlinePairs o) `shouldSatisfy` elem "gap"

    describe "the import alias map" $ do
        it "pairs a qualified alias with its module" $
            moduleAliases (imports ["import qualified Data.List as L"])
                `shouldBe` [("L", "Data.List")]
        it "pairs an alias declared without qualified" $
            moduleAliases (imports ["import DataFrame as DXD"])
                `shouldBe` [("DXD", "DataFrame")]
        it "honours postpositive qualified" $
            moduleAliases
                ( "{-# LANGUAGE ImportQualifiedPost #-}\n"
                    <> imports ["import Data.Text qualified as T"]
                )
                `shouldBe` [("T", "Data.Text")]
        it "skips an import that declares no alias" $
            moduleAliases (imports ["import qualified Data.Map", "import Data.Char"])
                `shouldBe` []
        it "collects every aliased import in order" $
            moduleAliases
                ( imports
                    [ "import qualified DataFrame as DXD"
                    , "import Data.HodaTime.Instant"
                    , "import qualified Numeric.LinearAlgebra as LA"
                    ]
                )
                `shouldBe` [("DXD", "DataFrame"), ("LA", "Numeric.LinearAlgebra")]
        it "still answers for a CPP file via the lexical rung" $
            moduleAliases
                ( T.unlines
                    [ "{-# LANGUAGE CPP #-}"
                    , "module Probe where"
                    , "#if MIN_VERSION_base(4,18,0)"
                    , "import qualified Data.Vector.Storable as V"
                    , "#endif"
                    , "probe = V.length"
                    ]
                )
                `shouldBe` [("V", "Data.Vector.Storable")]
  where
    outlinePairs o = [(n, l) | (n, l, _) <- oDecls o]
    imports ls = T.unlines ("module Probe where" : ls)
