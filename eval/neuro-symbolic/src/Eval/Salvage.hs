{-# LANGUAGE OverloadedStrings #-}

module Eval.Salvage (salvageCell) where

import Data.Text (Text)
import qualified Data.Text as T

salvageCell :: Text -> Maybe Text
salvageCell content
    | T.null openRest = Nothing
    | T.strip tag `notElem` ["", "haskell"] = Nothing
    | T.null close = Nothing
    | T.null (T.strip code) = Nothing
    | otherwise = Just (T.stripEnd code)
  where
    (_, openRest) = T.breakOn "```" content
    (tag, body0) = T.break (== '\n') (T.drop 3 openRest)
    (code, close) = T.breakOn "```" (T.drop 1 body0)
