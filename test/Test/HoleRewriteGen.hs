{-# LANGUAGE OverloadedStrings #-}

{- | Generated identifiers, generated sources and GHC diagnostics in the shapes
GHC 9.12 really prints them. Nothing here names a library, a function or a type
from any episode; the fixtures are formats, and every name in them is drawn.
-}
module Test.HoleRewriteGen (
    HeadSource (..),
    genIdent,
    genDistinct,
    genArg,
    genHeadSource,
    scopeDiag,
    untaggedScopeDiag,
    holeBlob,
    namedHoleBlob,
    twoHoleDiag,
    weakHoleDiag,
    wrappedHoleDiag,
    wrappedType,
    applySrc,
    field,
    countingProbe,
) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck

import Sabela.AI.Capabilities.Edit.HoleRewrite (holeName)

reserved :: [Text]
reserved =
    ["let", "in", "do", "case", "of", "if", "then", "else", "where", "data"]

genIdent :: Gen Text
genIdent = do
    n <- choose (3, 8) :: Gen Int
    cs <- vectorOf n (elements ['a' .. 'z'])
    let t = T.pack cs
    if t `elem` reserved then genIdent else pure t

genDistinct :: Int -> Gen [Text]
genDistinct n = do
    xs <- vectorOf (n * 3) genIdent
    let ys = dedup xs
    if length ys < n then genDistinct n else pure (take n ys)
  where
    dedup = foldr (\x acc -> x : filter (/= x) acc) []

genArg :: Gen Text
genArg =
    oneof
        [ genIdent
        , pure "42"
        , pure "\"literal\""
        , pure "[1, 2, 3]"
        , pure "(1, 2)"
        ]

{- | A generated cell whose head is not in scope, with the span the head really
stands at and how many times it heads an application.
-}
data HeadSource = HeadSource
    { hsSrc :: Text
    , hsHead :: Text
    , hsSpan :: (Int, Int)
    , hsCalls :: Int
    }
    deriving (Eq, Show)

{- | The shapes a real cell takes: one line or several, the head repeated, the
head's text also standing somewhere it is not a head, an indented block, and
with or without a closing newline.
-}
genHeadSource :: Gen HeadSource
genHeadSource = do
    ns <- genDistinct 3
    args <- resize 3 (listOf1 genArg)
    let [binder, other, headName] = take 3 (ns <> repeat "zz")
    trailing <- elements [True, False]
    shape <- elements [0 .. 5 :: Int]
    let call pre = pre <> headName <> " " <> T.unwords args
        assign nm = nm <> " = "
        (ls, row, prefix, calls) = case shape of
            0 -> ([call (assign binder)], 1, assign binder, 1)
            1 ->
                ( [other <> " = 1", call (assign binder)]
                , 2
                , assign binder
                , 1
                )
            2 ->
                ( [other <> " = \"" <> headName <> " here\"", call (assign binder)]
                , 2
                , assign binder
                , 1
                )
            3 ->
                ( [call (assign binder), call (assign other)]
                , 1
                , assign binder
                , 2
                )
            4 ->
                ( [other <> " = (" <> headName <> ")", call (assign binder)]
                , 2
                , assign binder
                , 1
                )
            _ ->
                ( ["do", "  let " <> other <> " = [1, 2]", call "  "]
                , 3
                , "  "
                , 1
                )
        src = T.intercalate "\n" ls <> (if trailing then "\n" else "")
    pure (HeadSource src headName (row, T.length prefix + 1) calls)

-- | A not-in-scope diagnostic carrying one of GHC's structural codes.
scopeDiag :: Int -> Text -> Text
scopeDiag code name =
    "<interactive>:12:5: error: [GHC-"
        <> T.pack (show code)
        <> "]\n    Variable not in scope: "
        <> name
        <> " :: t0 -> t1"

-- | The same diagnostic from a compiler that printed no code.
untaggedScopeDiag :: Text -> Text
untaggedScopeDiag name =
    "<interactive>:12:5: error:\n    Variable not in scope: " <> name

-- | One block about the harness's hole, in GHC's layout.
holeBlob :: Text -> [(Text, Text)] -> Text
holeBlob = namedHoleBlob holeName

{- | The same block about a hole of any name, so the standing nudge's bare
@_@ can be answered too.
-}
namedHoleBlob :: Text -> Text -> [(Text, Text)] -> Text
namedHoleBlob nm ty fits =
    "A.hs:5:5: error: [GHC-88464]\n    \8226 Found hole: "
        <> nm
        <> " :: "
        <> ty
        <> "\n      Valid hole fits include"
        <> T.concat ["\n        " <> n <> " :: " <> t | (n, t) <- fits]

{- | GHC 9.12's output for a module carrying a bare hole of its own before the
harness's named one: two blocks, prose, and the echoed source lines.
-}
twoHoleDiag :: Text -> Text -> Text -> Text
twoHoleDiag ownBinder theirs ours =
    T.unlines
        [ "B.hs:3:5: error: [GHC-88464]"
        , "    \8226 Found hole: _ :: Int"
        , "    \8226 In an equation for \8216"
            <> ownBinder
            <> "\8217: "
            <> ownBinder
            <> " = _"
        , "    \8226 Relevant bindings include " <> ownBinder <> " :: Int"
        , "      Valid hole fits include"
        , "        " <> theirs <> " :: Int (bound at B.hs:3:1)"
        , "  |"
        , "3 | " <> ownBinder <> " = _"
        , "  |     ^"
        , ""
        , "B.hs:5:5: error: [GHC-88464]"
        , "    \8226 Found hole: " <> holeName <> " :: [Int] -> Int"
        , "      Or perhaps \8216" <> holeName <> "\8217 is mis-spelled, or not in scope"
        , "    \8226 In the expression: " <> holeName <> " [1 :: Int, 2]"
        , "    \8226 Relevant bindings include " <> ownBinder <> "z :: Int"
        , "      Valid hole fits include"
        , "        " <> ours <> " :: [Int] -> Int"
        , "          (imported from \8216Prelude\8217 at B.hs:1:8)"
        , "  |"
        , "5 | b = " <> holeName <> " [1 :: Int, 2]"
        , "  |     ^^^^^^^^^^^"
        ]

{- | GHC's rendering when nothing pins the hole's type: the printed type is
followed, inside the same bullet, by prose naming the variables.
-}
weakHoleDiag :: Text -> Text
weakHoleDiag binder =
    T.unlines
        [ "C.hs:2:9: error: [GHC-88464]"
        , "    \8226 Found hole: " <> holeName <> " :: t1 -> t2 -> t3"
        , "      Where: \8216t1\8217, \8216t2\8217, \8216t3\8217 are rigid type variables bound by"
        , "               the inferred type of " <> binder <> " :: t1 -> t2 -> t3"
        , "               at C.hs:2:1-23"
        , "      Or perhaps \8216" <> holeName <> "\8217 is mis-spelled, or not in scope"
        , "    \8226 In the expression: " <> holeName <> " x y"
        , "  |"
        , "2 | " <> binder <> " x y = " <> holeName <> " x y"
        , "  |         ^^^^^^^^^^^"
        ]

-- | The type GHC wraps in `wrappedHoleDiag`, as it reads once rejoined.
wrappedType :: Text
wrappedType =
    "[(String, Int)] -> (Int -> Int) -> [(String, Int)] -> String \
    \-> [(String, Int)]"

{- | GHC's rendering when the hole's type is too long for one line: the name
and the type each move onto their own continuation line.
-}
wrappedHoleDiag :: Text -> Text
wrappedHoleDiag fitName =
    T.unlines
        [ "C.hs:5:13: error: [GHC-88464]"
        , "    \8226 Found hole:"
        , "        " <> holeName
        , "          :: [(String, Int)]"
        , "             -> (Int -> Int) -> [(String, Int)] -> String -> [(String, Int)]"
        , "      Or perhaps \8216" <> holeName <> "\8217 is mis-spelled, or not in scope"
        , "    \8226 In the expression: " <> holeName <> " a b c d"
        , "      Valid hole fits include"
        , "        " <> fitName <> " :: [(String, Int)]"
        , "             -> (Int -> Int) -> [(String, Int)] -> String -> [(String, Int)]"
        , "          (bound at C.hs:5:1)"
        , "  |"
        , "5 | g a b c d = " <> holeName <> " a b c d"
        , "  |             ^^^^^^^^^^^"
        ]

applySrc :: Text -> Text -> [Text] -> Text
applySrc binder headName args =
    binder <> " = " <> headName <> " " <> T.unwords args <> "\n"

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (Key.fromText k) o
field _ _ = Nothing

-- | A probe that answers with a fixed blob and counts how often it was asked.
countingProbe :: Text -> IO (Text -> IO Text, IO Int)
countingProbe answer = do
    ref <- newIORef (0 :: Int)
    pure (\_ -> modifyIORef' ref (+ 1) >> pure answer, readIORef ref)
