{-# LANGUAGE OverloadedStrings #-}

module Siza.Agent.Check.Gate (
    CheckRefusal (..),
    refusalNote,
    referenceGate,
    identifiersOf,
    mentionsAny,
    Perturbation (..),
    perturbationsFor,
    perturbCheck,
) where

import Data.Char (isAlpha, isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T

data CheckRefusal
    = NoReference
    | Indiscriminate
    deriving (Eq, Show)

refusalNote :: CheckRefusal -> Text
refusalNote NoReference =
    "it references nothing this task defined, so it cannot be about the \
    \deliverable"
refusalNote Indiscriminate =
    "it still passes when the value is perturbed, so it cannot fail"

referenceGate :: [Text] -> Text -> Maybe CheckRefusal
referenceGate owned check
    | mentionsAny owned check = Nothing
    | otherwise = Just NoReference

mentionsAny :: [Text] -> Text -> Bool
mentionsAny names t = any (`elem` identifiersOf t) names

identifiersOf :: Text -> [Text]
identifiersOf =
    filter (\w -> not (T.null w) && isAlpha (T.head w))
        . T.split (\c -> not (isAlphaNum c || c == '_' || c == '\''))

data Perturbation = Perturbation
    { pName :: !Text
    , pExpr :: !Text
    }
    deriving (Eq, Show)

perturbationsFor :: Text -> Text -> [Perturbation]
perturbationsFor ty name
    | isNumeric = [off "+ 1" "1", off "- 1" "-1"]
    | isStringy = [Perturbation "truncated" ("init " <> paren name)]
    | isList =
        [ Perturbation "dropped last" ("init " <> paren name)
        , Perturbation "reversed" ("reverse " <> paren name)
        ]
    | otherwise = []
  where
    t = T.strip ty
    isNumeric = t `elem` ["Int", "Integer", "Double", "Float", "Rational"]
    isStringy = t `elem` ["String", "Text", "[Char]"]
    isList = "[" `T.isPrefixOf` t && not isStringy
    off lbl op =
        Perturbation ("offset by " <> op) (paren (name <> " " <> lbl))

paren :: Text -> Text
paren e = "(" <> e <> ")"

perturbCheck :: Text -> Perturbation -> Text -> Text
perturbCheck name p = rebuild . tokenise
  where
    rebuild = T.concat . map swap
    swap tok = if tok == name then pExpr p else tok
    tokenise t
        | T.null t = []
        | isIdent (T.head t) =
            let (tok, rest) = T.span isIdent t in tok : tokenise rest
        | otherwise =
            let (gap, rest) = T.break isIdent t in gap : tokenise rest
    isIdent c = isAlphaNum c || c == '_' || c == '\''
