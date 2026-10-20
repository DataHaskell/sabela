{-# LANGUAGE OverloadedStrings #-}

{- | A table's filter box: the text is a row predicate in the dataframe
expression language, written over the frame's columns by name. Every column
is bound locally as a typed column expression, the way @declareColumns@
would bind it, and the predicate is applied with @filterWhere@.
-}
module Sabela.Server.FrameFilter (
    filterBlock,
    columnBindings,
    columnIdent,
    renderType,
) where

import Data.Char (
    isAlphaNum,
    isLower,
    isPunctuation,
    isSpace,
    isSymbol,
    isUpper,
    toLower,
 )
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

{- | The GHCi input: the columns bound in a @let@, the predicate as the
argument of @filterWhere@, all inside the typed query over the frame.
-}
filterBlock :: Text -> Text -> [(Text, Text)] -> Text -> Text
filterBlock fn slot cols expr =
    T.unlines $
        [":{", fn <> " " <> T.pack (show (T.unpack slot)) <> " (\\df ->"]
            ++ letLines
            ++ ["     in SabelaDF.filterWhere", "            ("]
            ++ ["            " <> l | l <- T.lines expr, not (T.null (T.strip l))]
            ++ ["            )", "            df", "  )", ":}"]
  where
    binds = columnBindings cols
    letLines = case binds of
        [] -> ["    let _sabelaNoColumns = ()"]
        (b : bs) -> ("    let " <> b) : ["        " <> b' | b' <- bs]

{- | One @name = col "name" :: Expr Type@ per column whose name and type can
be bound; a later column whose identifier repeats an earlier one is skipped.
-}
columnBindings :: [(Text, Text)] -> [Text]
columnBindings = go []
  where
    go _ [] = []
    go seen ((name, ty) : rest) =
        case (columnIdent name, renderType ty) of
            (Just ident, Just t)
                | ident `notElem` seen ->
                    ( ident
                        <> " = SabelaDF.col (SabelaDFText.pack "
                        <> T.pack (show (T.unpack name))
                        <> ") :: SabelaDF.Expr "
                        <> t
                    )
                        : go (ident : seen) rest
            _ -> go seen rest

{- | The identifier a column name binds as, following the dataframe library's
own @sanitize@: a plain lower-case name is kept, anything else is lower-cased
with its punctuation and spaces made underscores, and a result that is still
not a variable name (or is a keyword) is wrapped in underscores.
-}
columnIdent :: Text -> Maybe Text
columnIdent raw
    | T.null cleaned = Nothing
    | plain = Just raw
    | varId cleaned && not (reserved cleaned) = Just cleaned
    | otherwise = Just ("_" <> cleaned <> "_")
  where
    plain = varId raw && not (reserved raw) && T.all isAlphaNum raw
    cleaned = T.map replace (T.filter (`notElem` ("()" :: String)) raw)
    replace c
        | isUpper c = toLower c
        | isSpace c || isPunctuation c || isSymbol c = '_'
        | isAlphaNum c = c
        | otherwise = '_'
    varId s = case T.uncons s of
        Just (c, _) -> isLower c || c == '_'
        Nothing -> False
    reserved s = s `elem` keywords

keywords :: [Text]
keywords =
    [ "case", "class", "data", "default", "deriving", "do", "else", "foreign"
    , "if", "import", "in", "infix", "infixl", "infixr", "instance", "let"
    , "module", "newtype", "of", "then", "type", "where", "_"
    ]

{- | A column type as the library prints it (@Maybe Text@, @[Char]@, @Double@)
as an annotation that resolves in the session: base types through @Prelude@,
@Text@ through the harness's own alias, anything unknown left as written,
and an applied type in parentheses. A type with characters outside a type
expression is refused.
-}
renderType :: Text -> Maybe Text
renderType ty
    | T.null (T.strip ty) = Nothing
    | not (T.all allowed ty) = Nothing
    | otherwise = Just (bracket (T.strip (T.concat (map qualify (tokens ty)))))
  where
    bracket t
        | T.any (== ' ') t = "(" <> t <> ")"
        | otherwise = t
    allowed c = isAlphaNum c || c `elem` ("_.'[]() " :: String)
    qualify tok
        | tok == "Text" = "SabelaDFText.Text"
        | tok == "Maybe" = "SabelaBase.Maybe"
        | tok `elem` preludeTypes = "Prelude." <> tok
        | otherwise = tok
    preludeTypes =
        ["Int", "Integer", "Double", "Float", "Bool", "Char", "Word", "String"]

tokens :: Text -> [Text]
tokens t = case T.uncons t of
    Nothing -> []
    Just (c, rest)
        | isAlphaNum c || c == '_' ->
            let (w, more) = T.span (\x -> isAlphaNum x || x `elem` ("_.'" :: String)) t
             in w : tokens more
        | otherwise -> T.singleton c : tokens rest
