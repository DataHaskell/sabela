{-# LANGUAGE OverloadedStrings #-}

{- | Where the call GHC blamed stands in the candidate, and which side of it a
given argument lies on. GHC names a prefix head, a backticked function and a
symbolic operator the same way in its blame, so all three must be locatable.
-}
module Sabela.AI.Capabilities.Edit.HoleRewrite.Call (
    CallHead (..),
    argumentSpans,
    callHeads,
    infixSpans,
    operatorOf,
    unqualifyName,
) where

import Data.Char (isAlphaNum, isUpper)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.Edit.HoleRewrite.Head (
    codeSegments,
    exprSpans,
    headSpansBy,
    numberedLines,
 )

{- | Where a call the compiler named stands, and which side of it its arguments
lie on. An operator's first argument is written to its left; every argument of
a prefix head, and every later argument of an operator, stands to its right.
-}
data CallHead = CallHead
    { chLine :: Int
    , chCol :: Int
    , chInfix :: Bool
    }
    deriving (Eq, Show)

-- | Every (line, column) at which the blamed argument of a call could stand.
argumentSpans :: Text -> Int -> Text -> Text -> [(Int, Int)]
argumentSpans fn idx expr src =
    [place | place <- exprSpans expr src, any (argumentSide idx place) heads]
  where
    heads = callHeads fn src

-- | Every place the named function heads a call, prefix or infix.
callHeads :: Text -> Text -> [CallHead]
callHeads fn src = case operatorOf fn of
    Just _ -> infixHeads
    Nothing ->
        [CallHead l c False | (l, c) <- headSpansBy sameName src] <> infixHeads
  where
    infixHeads = [CallHead l c True | (l, c) <- infixSpans fn src]
    sameName tok = unqualifyName tok == unqualifyName fn

{- | Whether a (line, column) stands where the given argument of a call would.
Argument one of an operator is its left operand; everything else is a right one.
-}
argumentSide :: Int -> (Int, Int) -> CallHead -> Bool
argumentSide idx (line, col) h
    | chLine h /= line = False
    | chInfix h && idx == 1 = col < chCol h
    | otherwise = chCol h < col

{- | Every (line, column) at which the named function stands infix: a symbolic
operator written bare, or an identifier written between backticks. GHC prints a
symbolic name parenthesised, and either spelling may carry a module qualifier.
-}
infixSpans :: Text -> Text -> [(Int, Int)]
infixSpans fn src = case operatorOf fn of
    Just op -> spansOf operatorTokens op
    Nothing -> spansOf backtickedTokens (unqualifyName fn)
  where
    spansOf tokensIn wanted =
        [ (lineNo, col)
        | (lineNo, l) <- numberedLines src
        , (col, tok) <- tokensIn l
        , tok == wanted
        ]

{- | The operator a printed name stands for, once GHC's parentheses and any
module qualifier come off. Nothing when the name is alphanumeric, which GHC
writes infix in backticks instead.
-}
operatorOf :: Text -> Maybe Text
operatorOf fn
    | T.null op = Nothing
    | T.all isSymbolChar op = Just op
    | otherwise = Nothing
  where
    op = unqualifyName (T.dropAround (`elem` ("()" :: String)) (T.strip fn))

{- | A name with its module qualifier removed. The qualifier is peeled a
segment at a time rather than read off the last dot, because the operator that
follows it may itself be spelled with dots.
-}
unqualifyName :: Text -> Text
unqualifyName n = case T.uncons n of
    Just (c, _)
        | isUpper c
        , (seg, rest) <- T.span identChar n
        , not (T.null seg)
        , Just after <- T.stripPrefix "." rest ->
            unqualifyName after
    _ -> n
  where
    identChar c = isAlphaNum c || c == '_' || c == '\''

{- | Symbolic operator tokens with their 1-based columns, read off the code
stretches. A leading dot that separates a module qualifier is not part of one.
-}
operatorTokens :: Text -> [(Int, Text)]
operatorTokens l =
    [ (segCol + off + T.length lead, T.drop (T.length lead) tok)
    | (segCol, seg) <- codeSegments l
    , (off, tok) <- symbolRuns seg
    , let lead = qualifierDot seg off
    , T.length lead < T.length tok
    ]
  where
    qualifierDot seg off
        | off > 0
        , "." `T.isPrefixOf` T.drop off seg
        , maybe False (isAlphaNum . snd) (T.unsnoc (T.take off seg)) =
            "."
        | otherwise = ""

symbolRuns :: Text -> [(Int, Text)]
symbolRuns = go 0
  where
    go i t = case T.uncons t of
        Nothing -> []
        Just (c, _)
            | isSymbolChar c ->
                let (tok, rest) = T.span isSymbolChar t
                 in (i, tok) : go (i + T.length tok) rest
            | otherwise -> go (i + 1) (T.drop 1 t)

{- | Identifiers written between backticks, at the column of the opening
backtick, which is where the operator itself stands.
-}
backtickedTokens :: Text -> [(Int, Text)]
backtickedTokens l =
    [ (segCol + off, tok)
    | (segCol, seg) <- codeSegments l
    , (off, tok) <- go 0 seg
    ]
  where
    go i seg = case T.uncons (T.drop i seg) of
        Nothing -> []
        Just ('`', rest) ->
            let (tok, after) = T.span identChar rest
             in if T.take 1 after == "`" && not (T.null tok)
                    then (i, tok) : go (i + T.length tok + 2) seg
                    else go (i + 1) seg
        _ -> go (i + 1) seg
    identChar c = isAlphaNum c || c `elem` ("_'." :: String)

isSymbolChar :: Char -> Bool
isSymbolChar c = c `elem` ("!#$%&*+./<=>?@\\^|-~:" :: String)
