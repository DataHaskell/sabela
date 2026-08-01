{-# LANGUAGE OverloadedStrings #-}

{- | Which source answers a check_type question. A name is a question about the
world and is answered from the index without a session; an expression is a
question for a compiler and is answered from a session.
-}
module Sabela.AI.Capabilities.Query.Route (
    QueryShape (..),
    LookupName,
    lookupText,
    classifyQuery,
    Plan (..),
    planFor,
    refinePlan,
    typeShaped,
) where

import Data.Char (isAlpha, isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.QualifiedName (isPlainName)

{- | A name the index may be asked for. Only 'classifyQuery' builds one, so no
type-shaped text can reach a name lookup.
-}
newtype LookupName = LookupName Text
    deriving (Eq, Show)

lookupText :: LookupName -> Text
lookupText (LookupName t) = t

data QueryShape
    = ExpressionQuery
    | NameQuery !LookupName
    deriving (Eq, Show)

{- | A single identifier, qualified name, or parenthesised operator is a name.
Everything else — applications, binders, lambdas, signatures — is an
expression.
-}
classifyQuery :: Text -> QueryShape
classifyQuery raw
    | isPlainName t
    , plainIdent t || operatorSection t =
        NameQuery (LookupName t)
    | otherwise = ExpressionQuery
  where
    t = T.strip raw

data Plan
    = SessionPlan
    | IndexPlan !LookupName
    deriving (Eq, Show)

{- | Asked-for imports are a request for a scope, and a scope is a compiler's
answer; otherwise the shape decides.
-}
planFor :: [Text] -> QueryShape -> Plan
planFor imports shape
    | not (null imports) = SessionPlan
    | NameQuery n <- shape = IndexPlan n
    | otherwise = SessionPlan

{- | A name the notebook itself defines is a question about this session,
whatever its shape; nothing else moves a plan.
-}
refinePlan :: Bool -> Plan -> Plan
refinePlan True (IndexPlan _) = SessionPlan
refinePlan _ p = p

-- | Text a type search would read as a signature rather than as a name.
typeShaped :: Text -> Bool
typeShaped t =
    "::" `T.isInfixOf` t || "->" `T.isInfixOf` t || T.any (== ' ') t

plainIdent :: Text -> Bool
plainIdent t = case T.uncons t of
    Just (c, _) -> (isAlpha c || c == '_') && T.all identChar t
    Nothing -> False

identChar :: Char -> Bool
identChar c = isAlphaNum c || c `elem` ("_'." :: String)

operatorSection :: Text -> Bool
operatorSection t =
    "(" `T.isPrefixOf` t
        && ")" `T.isSuffixOf` t
        && not (T.null inner)
        && T.any opChar inner
        && T.all (\c -> opChar c || identChar c) inner
  where
    inner = T.init (T.drop 1 t)
    opChar c = c `elem` ("!#$%&*+./<=>?@\\^|-~:" :: String)
