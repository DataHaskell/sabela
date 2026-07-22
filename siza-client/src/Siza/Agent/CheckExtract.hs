{-# LANGUAGE OverloadedStrings #-}

{- | Extracting a proposed covering-check expression from a model reply and
interpreting the user's confirm\/edit\/skip input — split from
"Siza.Agent.Check" (module-size cap); that module re-exports these names.
-}
module Siza.Agent.CheckExtract (
    extractTestExpr,
    interpretConfirm,
) where

import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.Salvage (salvageCell)

extractTestExpr :: Text -> Text
extractTestExpr reply = case salvageCell reply of
    Just code -> deQuote (firstNonEmptyLine code)
    Nothing -> case inlineCode reply of
        Just span_ -> T.strip span_
        Nothing -> deQuote (firstNonEmptyLine reply)
  where
    deQuote = T.strip . T.dropAround (== '`') . T.strip

firstNonEmptyLine :: Text -> Text
firstNonEmptyLine = headOr "" . filter (not . T.null . T.strip) . T.lines
  where
    headOr d [] = d
    headOr _ (x : _) = x

inlineCode :: Text -> Maybe Text
inlineCode t = case T.breakOn "`" t of
    (_, rest)
        | not (T.null rest)
        , (code, close) <- T.breakOn "`" (T.drop 1 rest)
        , not (T.null close)
        , not (T.null (T.strip code)) ->
            Just code
    _ -> Nothing

interpretConfirm :: Text -> Text -> Text
interpretConfirm proposed input
    | low `elem` ["skip", "no", "n"] = ""
    | T.null low || low `elem` ["y", "yes"] = proposed
    | looksLikeTest stripped = stripped
    | otherwise = ""
  where
    stripped = T.strip input
    low = T.toLower stripped

looksLikeTest :: Text -> Bool
looksLikeTest t =
    any
        (`T.isInfixOf` t)
        ["==", "/=", "<=", ">=", "<", ">", "&&", "||", " elem ", "isInfixOf"]
