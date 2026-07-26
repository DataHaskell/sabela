{-# LANGUAGE OverloadedStrings #-}

module Siza.Agent.Discover.Resolved (
    provenNames,
    resolvedWhy,
) where

import Data.Char (isAlpha, isAlphaNum)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T

provenNames :: Text -> [Text]
provenNames src =
    nub (concatMap withComponents (filter ident (T.split (not . identChar) src)))
  where
    identChar c = isAlphaNum c || c `elem` ("_'." :: String)
    ident t = maybe False (\(c, _) -> isAlpha c || c == '_') (T.uncons t)
    withComponents t = t : filter (not . T.null) (T.splitOn "." t)

resolvedWhy :: Text
resolvedWhy =
    "compiler-proven in this session (a clean check_type or a landed compile \
    \resolved it) — the type checker outranks the lexical index"
