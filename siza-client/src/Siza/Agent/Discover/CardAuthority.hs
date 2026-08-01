{-# LANGUAGE OverloadedStrings #-}

{- | What authority a card carries. A card is evidence about the axis it states
and about the query it was asked; a card that reports the query matched nothing
is evidence of exactly that, and cannot stand in for an answer.
-}
module Siza.Agent.Discover.CardAuthority (
    cardAnswers,
    cardField,
    cardInScope,
    stampCardAnswers,
) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T

import Siza.Agent.Discover.Types (Interpreted (..), Scope (..))

{- | A card answers a scoped request unless it states an axis the caller named
and states something else: scoping on module alone let a foreign card through.
-}
cardInScope :: Scope -> Value -> Bool
cardInScope (Scope m p) c =
    inside "module" m && inside "package" p
  where
    inside k = maybe True (\want -> maybe True (== want) (cardField k c))

{- | The query is answered unless the card denies it. A @matched@ stamp is only
ever written to report that the query matched no entry, so its presence is that
denial — unless the card's own rows name the query after all.
-}
cardAnswers :: Interpreted -> Value -> Bool
cardAnswers interp c = case cardField "matched" c of
    Nothing -> True
    Just _ -> rowsName (iName interp) c

rowsName :: Text -> Value -> Bool
rowsName q (Object o)
    | T.null q = False
    | otherwise = any (any (q `T.isInfixOf`) . rowsAt) rowKeys
  where
    rowsAt k = case KM.lookup (K.fromText k) o of
        Just (Array es) -> [s | String s <- toList es]
        _ -> []
rowsName _ _ = False

rowKeys :: [Text]
rowKeys = ["exports", "modules"]

{- | Record on the card that it does not answer the query, so a reader weighs
it as the harness weighed it. Nothing is stamped on a card that does answer:
the flag is a computed denial, not a decoration.
-}
stampCardAnswers :: Bool -> Value -> Value
stampCardAnswers False (Object o) =
    Object (KM.insert "cardAnswers" (Bool False) o)
stampCardAnswers _ v = v

cardField :: Text -> Value -> Maybe Text
cardField k (Object o) = case KM.lookup (K.fromText k) o of
    Just (String m) | not (T.null m) -> Just m
    _ -> Nothing
cardField _ _ = Nothing
