{-# LANGUAGE OverloadedStrings #-}

{- | What one discover answer must survive. Each check enters an outcome for
every claim it looked at, the unreachable ones included: a check that skips
what it cannot see reports "no check applies" as a confirmation.
-}
module Probe.Check (
    cardClaims,
    checkEnvelope,
    envelopeExports,
) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T

import Probe.Claim (Claim (..), Outcome (..))
import Probe.Oracle (Oracle, ownsModule)
import Siza.Agent.Discover.Envelope (
    boundEnvelope,
    envelopeCharBudget,
    envelopeChars,
    envelopeViolations,
 )
import Siza.Agent.Discover.Evict (evictionViolations)
import Siza.Agent.Discover.Types (exportRowName)

type Browsed = M.Map Text (Either Text [Text])

checkEnvelope :: Oracle -> Browsed -> Text -> Value -> [Claim]
checkEnvelope oracle browsed q v =
    [ verdictOf "schema" (envelopeViolations v)
    , verdictOf "eviction" (evictionViolations v (boundEnvelope v))
    , verdictOf "budget" budget
    , verdictOf "card-authority" authority
    ]
        ++ installBacked oracle q v
        ++ exportsReal browsed q v
  where
    verdictOf c [] = Claim c q Backed
    verdictOf c (x : _) = Claim c q (Broken x)
    budget
        | envelopeChars v <= envelopeCharBudget = []
        | length (hits v) <= 1 = []
        | otherwise =
            [tShow (envelopeChars v) <> " > " <> tShow envelopeCharBudget]
    authority =
        [ "a card denying the query set state=found"
        | (KM.lookup "cardAnswers" =<< card v) == Just (Bool False)
        , str "state" v == "found"
        , null (hits v)
        ]

{- | Every hit that says a package is installed is a claim on the package
database, answered by the database rather than by the harness's own index.
-}
installBacked :: Oracle -> Text -> Value -> [Claim]
installBacked oracle q v =
    [ Claim "install" q (owned p m)
    | h <- hits v
    , str "install" h `elem` ["installed", "installed-not-loaded"]
    , let p = str "package" h
    , let m = str "module" h
    , not (T.null p)
    , not (T.null m)
    ]
  where
    owned p m = case ownsModule oracle p m of
        Just True -> Backed
        Just False -> Broken (p <> " does not expose " <> m)
        Nothing -> Blind ("no package database knows " <> p)

{- | Every row a card lists is a claim about what a module exports, answered
by the compiler's own listing of that module.
-}
exportsReal :: Browsed -> Text -> Value -> [Claim]
exportsReal browsed q v =
    [ Claim "card-export" q (against m e)
    | Just (m, _) <- [cardClaims v]
    , e <- envelopeExports v
    ]
  where
    against m e = case M.lookup m browsed of
        Just (Right real)
            | e `elem` real -> Backed
            | otherwise -> Broken (m <> " does not export " <> e)
        Just (Left why) -> Blind why
        Nothing -> Blind ("no listing was taken for " <> m)

{- | The module a card enumerates and the package it says owns it — the pair
an independent listing needs. A card that lists nothing claims nothing.
-}
cardClaims :: Value -> Maybe (Text, Text)
cardClaims v = case card v of
    Just c
        | Just (String m) <- KM.lookup "module" c
        , not (T.null m)
        , not (null (envelopeExports v)) ->
            Just (m, cardPackage c)
    _ -> Nothing
  where
    cardPackage c = case KM.lookup "package" c of
        Just (String p) -> p
        _ -> ""

{- | The entities a card's export rows name, the unit an oracle listing can be
compared against. A row states a signature or a whole declaration, so the
subject is read through the published row-shape law, not off the row's head.
-}
envelopeExports :: Value -> [Text]
envelopeExports v = case card v >>= KM.lookup "exports" of
    Just (Array es) ->
        [n | String s <- toList es, let n = exportRowName s, not (T.null n)]
    _ -> []

card :: Value -> Maybe (KM.KeyMap Value)
card (Object o) = case KM.lookup "card" o of
    Just (Object c) -> Just c
    _ -> Nothing
card _ = Nothing

hits :: Value -> [Value]
hits (Object o) = case KM.lookup "hits" o of
    Just (Array a) -> toList a
    _ -> []
hits _ = []

str :: Text -> Value -> Text
str k (Object o) = case KM.lookup (K.fromText k) o of
    Just (String s) -> s
    _ -> ""
str _ _ = ""

tShow :: Int -> Text
tShow = T.pack . show
