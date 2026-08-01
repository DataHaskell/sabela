{-# LANGUAGE OverloadedStrings #-}

{- | The names a cell binds, answered so that the answer cannot go empty. The
AST parser returns nothing for a chunk it cannot parse, and an unparseable
cell is exactly the cell repair runs on, so the guard that stops a rewrite
landing on a binding site would vacate at the one moment it matters.
-}
module Sabela.AI.Capabilities.Edit.GateRepair.Binders (
    boundNames,
    lexicalBinders,
) where

import Data.Char (isAlphaNum, isLower, isUpper)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.Parse (cellNames)

{- | Every name the cell binds according to the parser, widened by a purely
lexical reading. Over-approximating is safe here: it can only refuse a
rewrite, never permit one.
-}
boundNames :: Text -> Set Text
boundNames src = fst (cellNames src) `Set.union` lexicalBinders src

-- | Top-level binders read off the layout: unindented @name … =@ or @name ::@.
lexicalBinders :: Text -> Set Text
lexicalBinders = Set.fromList . concatMap binderOf . T.lines

binderOf :: Text -> [Text]
binderOf line
    | isIndented line = []
    | T.null token = []
    | token `elem` typeKeywords = take 1 (drop 1 (identTokens rest))
    | token `elem` otherKeywords = []
    | not (startsIdent token) = []
    | declares rest = [token]
    | otherwise = []
  where
    token = identPrefix line
    rest = T.drop (T.length token) line
    isIndented t = maybe True (\(c, _) -> c == ' ' || c == '\t') (T.uncons t)
    startsIdent t = maybe False (\(c, _) -> isLower c || isUpper c || c == '_') (T.uncons t)

identPrefix :: Text -> Text
identPrefix = T.takeWhile (\c -> isAlphaNum c || c == '_' || c == '\'')

identTokens :: Text -> [Text]
identTokens = filter (not . T.null) . map identPrefix . T.words

{- | Whether what follows the leading token makes the line a binding: an
equals sign or a type signature outside any string or comment.
-}
declares :: Text -> Bool
declares = go ' ' False
  where
    go prev inString t = case T.uncons t of
        Nothing -> False
        Just ('"', r) -> go '"' (not inString) r
        Just ('\\', r) | inString -> go '\\' inString (T.drop 1 r)
        Just (c, r)
            | inString -> go c inString r
            | "--" `T.isPrefixOf` t -> False
            | "::" `T.isPrefixOf` t -> True
            | c == '=', not (isOperatorChar prev), not (startsOperator r) -> True
            | otherwise -> go c inString r
    startsOperator r = maybe False (isOperatorChar . fst) (T.uncons r)
    isOperatorChar c = c `elem` ("=<>/!$*+.^|~-&%#?@:" :: String)

typeKeywords :: [Text]
typeKeywords = ["data", "newtype", "type", "class"]

otherKeywords :: [Text]
otherKeywords =
    [ "import"
    , "module"
    , "instance"
    , "foreign"
    , "infixl"
    , "infixr"
    , "infix"
    , "deriving"
    , "let"
    , "where"
    , "do"
    , "case"
    , "if"
    ]
