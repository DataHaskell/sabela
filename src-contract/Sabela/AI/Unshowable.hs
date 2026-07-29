{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Unshowable (
    baseTypeName,
    renderActionFor,
    renderVocabulary,
    unshowableContrast,
    unshowableGuidanceMessage,
    unshowableShowType,
    wrapTrailingExpression,
) where

import Data.Char (isAlphaNum, isSpace)
import Data.Text (Text)
import qualified Data.Text as T

unshowableShowType :: Text -> Maybe Text
unshowableShowType msg
    | not (printClause msg) = Nothing
    | otherwise = case T.breakOn "No instance for" msg of
        (_, rest)
            | T.null rest -> Nothing
            | otherwise ->
                showConstraintToken (T.drop (T.length "No instance for") rest)

printClause :: Text -> Bool
printClause msg =
    "arising from a use of" `T.isInfixOf` msg
        && any (`T.isInfixOf` msg) ["`print'", "\8216print\8217"]

showConstraintToken :: Text -> Maybe Text
showConstraintToken t = do
    body <- T.stripPrefix "Show" (T.dropWhile isOpenNoise t)
    (c, _) <- T.uncons body
    if isOpenNoise c then tokenIn body else Nothing
  where
    isOpenNoise c = isSpace c || c `elem` ("(`\8216" :: String)
    tokenIn body =
        let tok = T.takeWhile isTypeChar (T.dropWhile isOpenNoise body)
         in if T.null tok then Nothing else Just (trimPrime tok)
    isTypeChar c = isAlphaNum c || c `elem` (".:-'" :: String)
    trimPrime tok = case T.unsnoc tok of
        Just (initTok, '\'') -> initTok
        _ -> tok

baseTypeName :: Text -> Text
baseTypeName tok = lastPart "." (lastPart ":" tok)
  where
    lastPart sep t = case reverse (T.splitOn sep t) of
        (x : _) -> x
        [] -> t

renderVocabulary :: [(Text, Maybe Text)]
renderVocabulary =
    [ ("displayPicture", Just "Sabela.Notebook")
    , ("animate 3", Just "Sabela.Notebook")
    , ("display", Nothing)
    ]

-- | The canonical render action for a base type name, for messaging only.
renderActionFor :: Text -> Maybe (Text, Maybe Text)
renderActionFor ty =
    lookup
        ty
        [ ("Picture", ("displayPicture", Just "Sabela.Notebook"))
        , ("Input", ("display", Nothing))
        ]

wrapTrailingExpression :: Text -> Text -> Maybe Text
wrapTrailingExpression fn src = case reverse (T.lines src) of
    [] -> Nothing
    (lastLine : rest)
        | T.null (T.strip lastLine) -> Nothing
        | declarationLike (T.stripStart lastLine) -> Nothing
        | otherwise ->
            Just
                ( T.unlines
                    (reverse rest ++ [fn <> " (" <> T.strip lastLine <> ")"])
                )
  where
    declarationLike t =
        "import " `T.isPrefixOf` t
            || "{-#" `T.isPrefixOf` t
            || "--" `T.isPrefixOf` t
            || " = " `T.isInfixOf` t
            || isBindStatement t

-- | A top-level @pat <- expr@ bind; an arrow inside a comprehension or any
-- bracketed context is not one.
isBindStatement :: Text -> Bool
isBindStatement t = case T.breakOn " <- " t of
    (before, rest) ->
        not (T.null rest)
            && T.all (`notElem` ("[(|" :: String)) before

unshowableGuidanceMessage :: Text -> Maybe Text
unshowableGuidanceMessage msg = do
    ty <- baseTypeName <$> unshowableShowType msg
    pure $ case renderActionFor ty of
        Just (fn, mImp) ->
            "The final expression has type "
                <> ty
                <> ", which renders but has no Show instance, so GHCi's \
                   \print cannot display it. Wrap the final expression: "
                <> fn
                <> " (<expr>)"
                <> maybe
                    ""
                    (\m -> " — " <> fn <> " is in " <> m <> "; import it")
                    mImp
                <> "."
        Nothing ->
            "The final expression has type "
                <> ty
                <> ", which has no Show instance, so GHCi's print cannot \
                   \display it. Find a render action with discover {query: \""
                <> ty
                <> " -> IO ()\"} and wrap the final expression with it, or \
                   \bind the value instead of returning it."

unshowableContrast :: Text -> Maybe Text
unshowableContrast msg = do
    ty <- baseTypeName <$> unshowableShowType msg
    pure $ case renderActionFor ty of
        Just (fn, mImp) ->
            "the final expression's type ("
                <> ty
                <> ") has no Show instance — wrap it: "
                <> fn
                <> " (<expr>)"
                <> maybe "" (\m -> ", importing " <> m <> " if needed") mImp
                <> "."
        Nothing ->
            "the final expression's type ("
                <> ty
                <> ") has no Show instance — find its render action with \
                   \discover {query: \""
                <> ty
                <> " -> IO ()\"} and wrap it."
