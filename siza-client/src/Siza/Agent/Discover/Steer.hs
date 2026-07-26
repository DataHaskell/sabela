{-# LANGUAGE OverloadedStrings #-}

module Siza.Agent.Discover.Steer (
    goalTypeOf,
    producerPrefixes,
) where

import Data.Char (isAlphaNum, isLower, isUpper)
import Data.Text (Text)
import qualified Data.Text as T

goalTypeOf :: Text -> Maybe Text
goalTypeOf name
    | not plainIdent = Nothing
    | upperHead name = Just name
    | prefix `elem` producerPrefixes && upperHead rest = Just rest
    | otherwise = Nothing
  where
    plainIdent =
        not (T.null name) && T.all (\c -> isAlphaNum c || c == '\'') name
    (prefix, rest) = T.span isLower name
    upperHead t = maybe False (isUpper . fst) (T.uncons t)

producerPrefixes :: [Text]
producerPrefixes =
    ["default", "mk", "make", "new", "empty", "init", "initial", "create"]
