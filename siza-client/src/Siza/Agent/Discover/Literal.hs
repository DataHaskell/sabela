{-# LANGUAGE OverloadedStrings #-}

module Siza.Agent.Discover.Literal (
    literalConstructible,
    literalFill,
) where

import Data.Text (Text)
import qualified Data.Text as T

literalConstructible :: [Text]
literalConstructible = map fst scalarLiterals

scalarLiterals :: [(Text, Text)]
scalarLiterals =
    [ ("Int", "0")
    , ("Integer", "0")
    , ("Word", "0")
    , ("Double", "0.0")
    , ("Float", "0.0")
    , ("Char", "' '")
    , ("Bool", "True")
    , ("String", "\"\"")
    , ("FilePath", "\"\"")
    , ("Text", "\"\"")
    , ("Ordering", "EQ")
    ]

literalFill :: Text -> Maybe Text
literalFill ty0 = case T.strip ty0 of
    t
        | Just inner <- wrappedIn '[' ']' t ->
            (\e -> "[" <> e <> "]") <$> literalFill inner
        | Just inner <- wrappedIn '(' ')' t ->
            case splitTopCommas inner of
                [one] -> literalFill one
                parts -> do
                    lits <- traverse literalFill parts
                    pure ("(" <> T.intercalate ", " lits <> ")")
        | otherwise -> lookup (normType t) scalarLiterals
  where
    normType = T.unwords . T.words

wrappedIn :: Char -> Char -> Text -> Maybe Text
wrappedIn open close t
    | matches = Just (T.dropEnd 1 (T.drop 1 t))
    | otherwise = Nothing
  where
    n = T.length t
    matches = case T.uncons t of
        Just (h, _) | h == open -> go (0 :: Int) (zip [0 :: Int ..] (T.unpack t))
        _ -> False
    go _ [] = False
    go d ((i, c) : rest)
        | c == open = go (d + 1) rest
        | c == close = if d - 1 == 0 then i == n - 1 else go (d - 1) rest
        | otherwise = go d rest

splitTopCommas :: Text -> [Text]
splitTopCommas = map (T.strip . T.pack) . go (0 :: Int) "" . T.unpack
  where
    go _ acc [] = [reverse acc]
    go 0 acc (',' : rest) = reverse acc : go 0 "" rest
    go d acc (c : rest) = go (d + delta c) (c : acc) rest
    delta c
        | c `elem` ("([" :: String) = 1
        | c `elem` (")]" :: String) = -1
        | otherwise = 0
