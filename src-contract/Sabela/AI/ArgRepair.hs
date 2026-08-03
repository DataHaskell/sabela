{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.ArgRepair (
    ArgBlame (..),
    argFillCandidates,
    argumentBlame,
    argumentOf,
    insertArgAt,
    missingArgType,
    ordinalAt,
    quotedAt,
    tooFewArgsTarget,
    wrapExprAt,
) where

import Data.Char (isAlphaNum, isDigit)
import Data.List (findIndex)
import Data.Maybe (isJust, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)

import Sabela.AI.HoleRepair (holeFitNames, substituteNameAt)

{- | Which argument of which call GHC blamed, and the expression it named
there. Read from the diagnostic's own prose, which is the only place the
disposable compile reports it: JSON diagnostics are off in that session.
-}
data ArgBlame = ArgBlame
    { abFunction :: Text
    , abIndex :: Int
    , abExpr :: Text
    }
    deriving (Eq, Show)

tooFewArgsTarget :: Text -> Maybe Text
tooFewArgsTarget err =
    listToMaybe
        [ name
        | l <- T.lines err
        , Just rest <- [T.stripPrefix "Probable cause: `" (T.strip l)]
        , let (name, rest') = T.breakOn "'" rest
        , "' is applied to too few arguments" `T.isPrefixOf` rest'
        , not (T.null name)
        ]

{- | The expected type GHC printed above its blame for one of @fn@'s
arguments. Any argument position, not only the first: GHC names the position
with an ordinal and the type sits above it either way.
-}
missingArgType :: Text -> Text -> Maybe Text
missingArgType err fn = do
    i <- findIndex (isJust . argumentOf fn) ls
    listToMaybe
        [ ty
        | l <- reverse (take i ls)
        , (_, rest) <- [T.breakOn expectedMarker l]
        , not (T.null rest)
        , let ty = T.strip (T.drop (T.length expectedMarker) rest)
        , not (T.null ty)
        ]
  where
    ls = T.lines err
    expectedMarker = "Couldn't match expected type:"

{- | The position at which a fragment blames one of @fn@'s arguments, counting
from one. Qualified and unqualified spellings of the same name agree.
-}
argumentOf :: Text -> Text -> Maybe Int
argumentOf fn fragment =
    listToMaybe
        [ n
        | rest <- afterEach "In the " fragment
        , Just (n, r1) <- [ordinalAt rest]
        , Just r2 <- [T.stripPrefix " argument of " r1]
        , Just (nm, _) <- [quotedAt r2]
        , unqualify nm == unqualify fn
        ]

{- | The whole of GHC's argument blame: @In the second argument of `f',
namely `x'@. Continuation lines are rejoined first, because GHC wraps the
named expression onto the following line whenever the bullet runs long.
-}
argumentBlame :: Text -> Maybe ArgBlame
argumentBlame err =
    listToMaybe
        [ ArgBlame fn n expr
        | rest <- afterEach "In the " joined
        , Just (n, r1) <- [ordinalAt rest]
        , Just r2 <- [T.stripPrefix " argument of " r1]
        , Just (fn, r3) <- [quotedAt r2]
        , not (T.null fn)
        , Just r4 <- [T.stripPrefix ", namely " (T.stripStart r3)]
        , Just (expr, _) <- [quotedAt r4]
        , not (T.null expr)
        ]
  where
    joined = T.unwords (T.words err)

{- | GHC's ordinal for an argument position, in the word form it uses up to
ten and the numeric form it uses past that, with what follows it.
-}
ordinalAt :: Text -> Maybe (Int, Text)
ordinalAt t = listToMaybe (words' <> numeric)
  where
    words' =
        [ (n, rest)
        | (w, n) <- zip ordinalWords [1 :: Int ..]
        , Just rest <- [T.stripPrefix w t]
        ]
    numeric =
        [ (n, T.drop 2 rest)
        | let (ds, rest) = T.span isDigit t
        , not (T.null ds)
        , T.take 2 rest `elem` ["st", "nd", "rd", "th"]
        , Just n <- [readMaybe (T.unpack ds)]
        ]

ordinalWords :: [Text]
ordinalWords =
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

{- | The fragment GHC quoted at the head of some text, in either quoting, and
what follows. The closing quote is the first at which the fragment's brackets
balance, so an apostrophe in a character literal does not end it early.
-}
quotedAt :: Text -> Maybe (Text, Text)
quotedAt t =
    listToMaybe
        [ (body, T.drop (T.length close) after)
        | (open, close) <- [("\8216", "\8217"), ("`", "'")]
        , Just rest <- [T.stripPrefix open t]
        , (body, after) <- balancedSplits close rest
        ]

-- | Every split of @t@ at a separator whose preceding text has balanced brackets.
balancedSplits :: Text -> Text -> [(Text, Text)]
balancedSplits sep t =
    [ (body, T.drop i t)
    | i <- [0 .. T.length t - 1]
    , sep `T.isPrefixOf` T.drop i t
    , let body = T.take i t
    , balanced body
    ]

balanced :: Text -> Bool
balanced = (== 0) . T.foldl' step (0 :: Int)
  where
    step d c
        | c `elem` ("([" :: String) = d + 1
        | c `elem` (")]" :: String) = d - 1
        | otherwise = d

argFillCandidates :: Text -> [Text]
argFillCandidates = holeFitNames

insertArgAt :: (Int, Int) -> Text -> Text -> Text -> Maybe Text
insertArgAt sp fn fill = substituteNameAt sp fn (fn <> " " <> fill)

{- | Wraps the expression standing at a span, leaving every other byte of the
source alone. Nothing when the span does not carry that exact expression, so a
mislocated blame can never rewrite something else.
-}
wrapExprAt :: (Int, Int) -> Text -> (Text -> Text) -> Text -> Maybe Text
wrapExprAt (line, col) expr wrap src
    | T.null expr || line < 1 || col < 1 = Nothing
    | otherwise = case splitAt (line - 1) (T.splitOn "\n" src) of
        (before, target : after) -> do
            newLine <- replaceAt target
            Just (T.intercalate "\n" (before ++ newLine : after))
        _ -> Nothing
  where
    replaceAt l =
        let (pre, rest) = T.splitAt (col - 1) l
         in case T.stripPrefix expr rest of
                Just post -> Just (pre <> wrap expr <> post)
                Nothing -> Nothing

afterEach :: Text -> Text -> [Text]
afterEach needle t =
    [ T.drop (i + T.length needle) t
    | i <- [0 .. T.length t - 1]
    , needle `T.isPrefixOf` T.drop i t
    ]

unqualify :: Text -> Text
unqualify n = if T.null seg then n else seg
  where
    seg = T.takeWhileEnd isIdent n
    isIdent c = isAlphaNum c || c == '_' || c == '\''
