{-# LANGUAGE OverloadedStrings #-}

{- | Reads the candidate's own text: which stretches of a line are code, which
identifiers stand there, and where a name heads an application. The gate's
session reports @\<interactive\>@ positions, so spans come from the source.
-}
module Sabela.AI.Capabilities.Edit.HoleRewrite.Head (
    codeSegments,
    enclosingBinder,
    exprSpans,
    headSpans,
    headSpansBy,
    lineTokens,
    numberedLines,
) where

import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T

{- | Every (line, column) at which a name stands as the head of an application
on its own line: applied to something, and not itself an argument.
-}
headSpans :: Text -> Text -> [(Int, Int)]
headSpans name = headSpansBy (== name)

-- | The same, over any test on the token, so a qualified spelling can match.
headSpansBy :: (Text -> Bool) -> Text -> [(Int, Int)]
headSpansBy p src =
    [ (lineNo, col)
    | (lineNo, l) <- numberedLines src
    , (col, tok) <- lineTokens l
    , p tok
    , headPosition (T.take (col - 1) l)
    , applied (T.drop (col - 1 + T.length tok) l)
    ]

{- | Every (line, column) at which an expression's exact text stands in code,
bounded on both sides so a name is never found inside a longer one. The match
must begin in code, and may then run through a string literal it contains.
-}
exprSpans :: Text -> Text -> [(Int, Int)]
exprSpans expr src
    | T.null expr = []
    | otherwise =
        [ (lineNo, col)
        | (lineNo, l) <- numberedLines src
        , off <- occurrences expr l
        , let col = off + 1
        , startsInCode l col
        , bounded l col
        ]
  where
    startsInCode l col =
        or
            [segCol <= col && col < segCol + T.length seg | (segCol, seg) <- codeSegments l]
    bounded l col =
        edgeFree (T.take (col - 1) l) (T.drop (col - 1 + T.length expr) l)
    edgeFree before after =
        maybe True (not . isIdent . snd) (T.unsnoc before)
            && maybe True (not . isIdent . fst) (T.uncons after)

numberedLines :: Text -> [(Int, Text)]
numberedLines src = zip [1 ..] (T.splitOn "\n" src)

occurrences :: Text -> Text -> [Int]
occurrences needle hay =
    [ i
    | i <- [0 .. T.length hay - T.length needle]
    , needle `T.isPrefixOf` T.drop i hay
    ]

{- | The binder whose equation a span sits in: the nearest name at column one
at or above it. A fit that names it is the caller's own recursion.
-}
enclosingBinder :: (Int, Int) -> Text -> Maybe Text
enclosingBinder (line, _) src =
    listToMaybe
        [ tok
        | l <- reverse (take line (T.splitOn "\n" src))
        , (1, tok) <- take 1 (lineTokens l)
        ]

{- | Identifier tokens with their 1-based columns. Built on the code stretches
so a name quoted in text or written in a comment is never rewritten.
-}
lineTokens :: Text -> [(Int, Text)]
lineTokens l =
    [ (segCol + off, tok)
    | (segCol, seg) <- codeSegments l
    , (off, tok) <- identsIn seg
    ]

identsIn :: Text -> [(Int, Text)]
identsIn = go 0
  where
    go i t = case T.uncons t of
        Nothing -> []
        Just (c, _)
            | isIdentStart c ->
                let (tok, rest) = T.span isIdent t
                 in (i, tok) : go (i + T.length tok) rest
            | otherwise -> go (i + 1) (T.drop 1 t)

{- | The stretches of a line that are code, with their 1-based start columns:
string literals and everything from a line comment onwards are dropped.
-}
codeSegments :: Text -> [(Int, Text)]
codeSegments = go 1
  where
    go i t
        | T.null t = []
        | otherwise =
            let n = stopAt t
                (code, rest) = T.splitAt n t
                here = [(i, code) | not (T.null code)]
             in case T.uncons rest of
                    Just ('"', more) ->
                        let (width, after) = afterString more
                         in here <> go (i + n + width) after
                    _ -> here

-- | How far a stretch of code runs: to a string literal, a comment, or the end.
stopAt :: Text -> Int
stopAt t = go 0
  where
    go n
        | n >= T.length t = n
        | T.index t n == '"' = n
        | "--" `T.isPrefixOf` T.drop n t = n
        | otherwise = go (n + 1)

{- | How wide a string literal is, counting both quotes, and what follows it. A
backslash escapes the next character, so an escaped quote does not end it.
-}
afterString :: Text -> (Int, Text)
afterString = go 1
  where
    go n t = case T.uncons t of
        Nothing -> (n, t)
        Just ('"', rest) -> (n + 1, rest)
        Just ('\\', rest) -> case T.uncons rest of
            Nothing -> (n + 1, rest)
            Just (_, more) -> go (n + 2) more
        Just (_, rest) -> go (n + 1) rest

{- | Whether what precedes a token leaves it heading an application. An
identifier, closing bracket, or dot or backtick bound straight onto it makes it
part of something else; a backtick with a space after it closed an operator.
-}
headPosition :: Text -> Bool
headPosition before
    | Just (_, c) <- T.unsnoc before, c `elem` (".`" :: String) = False
    | otherwise = case T.unsnoc (T.stripEnd before) of
        Nothing -> True
        Just (_, c) -> not (nameChar c || c `elem` (")]}" :: String))
  where
    nameChar c = isAlphaNum c || c `elem` ("_'" :: String)

-- | Whether an argument follows on the same line.
applied :: Text -> Bool
applied after = case T.uncons (T.stripStart after) of
    Nothing -> False
    Just (c, _) -> isIdentStart c || isDigit c || c `elem` ("([\"" :: String)

isIdentStart :: Char -> Bool
isIdentStart c = isAlpha c || c == '_'

isIdent :: Char -> Bool
isIdent c = isAlphaNum c || c `elem` ("_'." :: String)
