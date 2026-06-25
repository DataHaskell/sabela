{-# LANGUAGE OverloadedStrings #-}

module Eval.HoleFit (
    goalFromError,
    parseFitNames,
    substituteName,
) where

import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T

goalFromError :: Text -> Maybe (Text, Text)
goalFromError err = firstJust (map fromLine (T.lines err))
  where
    fromLine l = do
        rest <- afterInfix "not in scope:" l
        let (lhs, rhs) = T.breakOn "::" rest
        if T.null rhs
            then Nothing
            else
                let name = T.strip lhs
                    ty = T.strip (T.drop 2 rhs)
                 in if T.null name || T.null ty then Nothing else Just (name, ty)

parseFitNames :: Text -> [Text]
parseFitNames blob =
    [ nm
    | l <- fitLines
    , not (isProvenance l)
    , let (lhs, rhs) = T.breakOn "::" l
    , not (T.null rhs)
    , let nm = T.strip lhs
    , not (T.null nm)
    ]
  where
    fitLines =
        takeWhile (not . isRefinement) $
            drop 1 (dropWhile (not . isValid) (T.lines blob))
    isValid = T.isInfixOf "Valid hole fits include"
    isRefinement = T.isInfixOf "Valid refinement hole fits"
    isProvenance x = any (`T.isInfixOf` x) ["with ", "(imported", "(bound"]

substituteName :: Text -> Text -> Text -> Text
substituteName wrong fit src
    | wrong == fit || T.null wrong = src
    | otherwise = T.concat (map sub (T.groupBy sameClass src))
  where
    sub tok = if tok == wrong then fit else tok
    sameClass a b = isIdent a == isIdent b
    isIdent c = isAlphaNum c || c == '_' || c == '.' || c == '\''

afterInfix :: Text -> Text -> Maybe Text
afterInfix needle t = case T.breakOn needle t of
    (_, rest) | not (T.null rest) -> Just (T.drop (T.length needle) rest)
    _ -> Nothing

firstJust :: [Maybe a] -> Maybe a
firstJust = foldr orElse Nothing
  where
    orElse (Just x) _ = Just x
    orElse Nothing y = y
