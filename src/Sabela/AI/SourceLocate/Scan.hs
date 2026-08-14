{-# LANGUAGE OverloadedStrings #-}

{- | Rung 2 of the source-location ladder: a lexical column-0 scan for the
CPP-ridden files ghc-lib-parser refuses, sharing the parser rung's 'Row'.
-}
module Sabela.AI.SourceLocate.Scan (
    Row (..),
    scannedRows,
) where

import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

{- | One row per source declaration: the names it defines or signs, whether
it is a signature, and its line span.
-}
data Row = Row
    { rowNames :: S.Set Text
    , rowPrimary :: Text
    , rowIsSig :: Bool
    , rowFrom :: Int
    , rowTo :: Int
    }

{- | Column-0 chunks of a file the parser refused. A CPP directive is a
boundary that belongs to no declaration; an indented line continues the
chunk above it.
-}
scannedRows :: Text -> [Row]
scannedRows src = mapMaybe chunkRow (chunks (zip [1 ..] (T.lines src)))
  where
    chunks [] = []
    chunks ((n, l) : rest)
        | opensChunk l =
            let (body, others) = span (continues . snd) rest
             in ((n, l), map fst body) : chunks others
        | otherwise = chunks rest
    continues l = T.null (T.strip l) || startsIndented l
    startsIndented l = case T.uncons l of
        Just (c, _) -> c == ' ' || c == '\t'
        Nothing -> True
    chunkRow ((n, l), body) = do
        name <- chunkName l
        let lastLine = if null body then n else last (n : body)
        pure
            Row
                { rowNames = S.singleton name
                , rowPrimary = name
                , rowIsSig = " :: " `T.isInfixOf` l
                , rowFrom = n
                , rowTo = lastLine
                }

opensChunk :: Text -> Bool
opensChunk l = case T.uncons l of
    Just (c, _) ->
        c /= ' '
            && c /= '\t'
            && c /= '#'
            && not (T.isPrefixOf "--" l)
            && not (T.isPrefixOf "{-" l)
            && not (T.isPrefixOf "module " l)
            && not (T.isPrefixOf "import " l)
    Nothing -> False

-- | What a column-0 line declares, read from its leading tokens.
chunkName :: Text -> Maybe Text
chunkName l = case T.words l of
    ("data" : rest) -> nameAfter rest
    ("newtype" : rest) -> nameAfter rest
    ("type" : rest) -> nameAfter rest
    ("class" : rest) -> nameAfter rest
    ("instance" : _) -> Nothing
    ("foreign" : _) -> Nothing
    ("infix" : _) -> Nothing
    ("infixl" : _) -> Nothing
    ("infixr" : _) -> Nothing
    (w : _) | plainName w -> Just w
    _ -> Nothing
  where
    nameAfter ws = listToMaybe [w | w <- ws, w /= "family", plainName w]
    plainName = T.all (\c -> c `notElem` ("(){}[]=," :: String))
