{-# LANGUAGE OverloadedStrings #-}

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

data Spine = Spine
    { spHead :: Text
    , spTypeApps :: [Text]
    , spArgs :: [Text]
    }
    deriving (Eq, Show)

spineArity :: Spine -> Int
spineArity = length . spArgs

trimTo :: Int -> Spine -> Spine
trimTo n sp = sp{spArgs = take (max 0 n) (spArgs sp)}

renderSpine :: Spine -> Text
renderSpine sp = T.unwords (spHead sp : spTypeApps sp ++ spArgs sp)

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

isIdentHead :: Text -> Bool
isIdentHead t = case T.uncons t of
    Just (c, _) -> (isAlpha c || c == '_') && T.all isIdentChar t
    Nothing -> False
  where
    isIdentChar c = isAlphaNum c || c `elem` ("_.'" :: String)

isTypeApp :: Text -> Bool
isTypeApp = T.isPrefixOf "@"

spineTokens :: Text -> Maybe [Text]
spineTokens = go [] . T.strip
  where
    go acc t
        | T.null t = Just (reverse acc)
        | otherwise = do
            (tok, rest) <- lexOne t
            go (tok : acc) (T.stripStart rest)

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
