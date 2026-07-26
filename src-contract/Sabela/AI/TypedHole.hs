{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.TypedHole (containsTypedHole) where

import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T

containsTypedHole :: Text -> Bool
containsTypedHole = go . T.groupBy sameClass
  where
    sameClass a b = isHoleIdent a == isHoleIdent b
    isHoleIdent c = isAlphaNum c || c == '_' || c == '\''
    go (tok : rest)
        | tok == "_" = "::" `T.isPrefixOf` T.stripStart (T.concat rest) || go rest
        | otherwise = go rest
    go [] = False
