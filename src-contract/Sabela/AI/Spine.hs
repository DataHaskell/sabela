{-# LANGUAGE OverloadedStrings #-}

{- | The application spine of an expression: a head applied to arguments, with
visible type applications kept aside. Backs arity repair, where a weak model
over-applies a function (@D.col \@Double "revenue" df@ against @col :: Text ->
Expr a@) and the fix is to drop the excess argument.

Lexical, not an AST: nothing in the tree parses an expression to a term or prints
one back. So this reads only a plain application and declines everything else —
a decline costs a missed repair, a misparse would corrupt the cell.
-}
module Sabela.AI.Spine (
    Spine (..),
    splitSpine,
    renderSpine,
    trimTo,
    spineArity,
) where

import Data.Char (isAlpha, isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T

{- | A head applied to arguments. A parenthesised argument is kept whole (parens
included) so it can be re-read on its own; type applications ride separately so a
trim cannot drop them.
-}
data Spine = Spine
    { spHead :: Text
    , spTypeApps :: [Text]
    , spArgs :: [Text]
    }
    deriving (Eq, Show)

-- | The number of value arguments the spine applies.
spineArity :: Spine -> Int
spineArity = length . spArgs

{- | Keep at most @n@ value arguments. The fix for an over-applied head; never
grows a spine, so a wrong arity can only ever shorten it.
-}
trimTo :: Int -> Spine -> Spine
trimTo n sp = sp{spArgs = take (max 0 n) (spArgs sp)}

-- | Re-render a spine: @head \@T arg1 arg2@.
renderSpine :: Spine -> Text
renderSpine sp = T.unwords (spHead sp : spTypeApps sp ++ spArgs sp)

{- | Read a plain application. 'Nothing' unless the whole expression is an
identifier head applied to atoms, literals, or balanced parenthesised groups —
an operator, lambda, literal head, or unbalanced text declines.
-}
splitSpine :: Text -> Maybe Spine
splitSpine src = do
    toks <- spineTokens (T.strip src)
    case toks of
        [] -> Nothing
        (h : rest)
            | isIdentHead h ->
                Just
                    Spine
                        { spHead = h
                        , spTypeApps = filter isTypeApp rest
                        , spArgs = filter (not . isTypeApp) rest
                        }
        _ -> Nothing

-- | An application head: a (possibly qualified) identifier, never a literal.
isIdentHead :: Text -> Bool
isIdentHead t = case T.uncons t of
    Just (c, _) -> (isAlpha c || c == '_') && T.all isIdentChar t
    Nothing -> False
  where
    isIdentChar c = isAlphaNum c || c `elem` ("_.'" :: String)

isTypeApp :: Text -> Bool
isTypeApp = T.isPrefixOf "@"

{- | Split into top-level tokens, keeping string literals and parenthesised
groups whole. 'Nothing' on unbalanced parens or a token that is not an
identifier, literal, type application, or group.
-}
spineTokens :: Text -> Maybe [Text]
spineTokens = go [] . T.strip
  where
    go acc t
        | T.null t = Just (reverse acc)
        | otherwise = do
            (tok, rest) <- lexOne t
            go (tok : acc) (T.stripStart rest)

{- | One top-level token. Fails on an unbalanced group or a bare operator, which
is how a non-application expression declines.
-}
lexOne :: Text -> Maybe (Text, Text)
lexOne t = case T.uncons t of
    Nothing -> Nothing
    Just ('(', _) -> lexGroup t
    Just ('"', _) -> lexString t
    Just (c, _)
        | isAlpha c || c == '_' || c == '@' || isAlphaNum c ->
            let (tok, rest) = T.span plain t
             in if T.null tok then Nothing else Just (tok, rest)
    _ -> Nothing
  where
    plain c = isAlphaNum c || c `elem` ("_.'@[]" :: String)

-- | A balanced parenthesised group, parens kept, nested groups and strings honoured.
lexGroup :: Text -> Maybe (Text, Text)
lexGroup t = walk 0 0 False t
  where
    walk :: Int -> Int -> Bool -> Text -> Maybe (Text, Text)
    walk depth i inStr s = case T.uncons s of
        Nothing -> Nothing
        Just (c, cs)
            | inStr, c == '\\' -> skipEscape depth i cs
            | inStr, c == '"' -> walk depth (i + 1) False cs
            | inStr -> walk depth (i + 1) True cs
            | c == '"' -> walk depth (i + 1) True cs
            | c == '(' -> walk (depth + 1) (i + 1) False cs
            | c == ')' ->
                if depth == 1
                    then Just (T.take (i + 1) t, T.drop (i + 1) t)
                    else walk (depth - 1) (i + 1) False cs
            | otherwise -> walk depth (i + 1) inStr cs
    skipEscape depth i cs = case T.uncons cs of
        Nothing -> Nothing
        Just (_, cs') -> walk depth (i + 2) True cs'

-- | A string literal, quotes kept, escapes honoured.
lexString :: Text -> Maybe (Text, Text)
lexString t = walk 1 (T.drop 1 t)
  where
    walk i s = case T.uncons s of
        Nothing -> Nothing
        Just ('\\', cs) -> case T.uncons cs of
            Nothing -> Nothing
            Just (_, cs') -> walk (i + 2) cs'
        Just ('"', _) -> Just (T.take (i + 1) t, T.drop (i + 1) t)
        Just (_, cs) -> walk (i + 1) cs
