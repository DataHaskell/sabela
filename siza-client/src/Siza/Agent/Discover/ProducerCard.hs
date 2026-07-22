{-# LANGUAGE OverloadedStrings #-}

{- | The section 7.1 established-target fallback: a zero-hit answer inside a
cluster whose target package has ledger provenance evidence answers with the
package's bounded export card, ranked by 'producesGoal' against the standing
goal — never an empty envelope, never triggered by a package name.
-}
module Siza.Agent.Discover.ProducerCard (
    establishedFallback,
    fetchPackageExports,
    producerCard,
) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.ToolName (ToolName (..))
import Sabela.AI.Types (ToolOutcome)
import Siza.Agent.Discover.Advice (setField)
import Siza.Agent.Discover.Envelope (boundEnvelope)
import Siza.Agent.Discover.Fetch (fetchOk)
import Siza.Agent.Discover.Goal (producesGoal)
import Siza.Agent.Discover.Request (DiscoverRequest (..))
import Siza.Agent.Discover.Types (StandingGoal (..))

{- | Section 7.1: a zero-hit answer while a derived goal stands with package
provenance answers the target's goal-ranked export card instead of an empty
envelope. Trigger: empty AND established-target AND goal-standing — a state
predicate, never a package name.
-}
establishedFallback ::
    Maybe StandingGoal ->
    (ToolName -> Value -> IO (Either Text ToolOutcome)) ->
    DiscoverRequest ->
    Value ->
    IO Value
establishedFallback mSG call req vOut
    | textAt "state" vOut == "not_found"
    , Just sg <- mSG
    , not (T.null (sgPackage sg)) = do
        exports <- fetchPackageExports call (sgPackage sg)
        pure $ case producerCard sg exports (drLimit req) of
            Just cardV -> setField "query" (drQuery req) cardV
            Nothing -> vOut
    | otherwise = pure vOut

{- | The goal-ranked export card for the established target package, or
'Nothing' when the catalogue held no exports (the honest miss stands —
a fabricated card would be a false found).
-}
producerCard :: StandingGoal -> [(Text, Text)] -> Int -> Maybe Value
producerCard sg exports limit
    | null exports = Nothing
    | otherwise = Just (boundEnvelope envelope)
  where
    g = sgType sg
    produces (_, ty) = producesGoal g ty
    ranked = filter produces exports ++ filter (not . produces) exports
    shown = take (max 1 limit) ranked
    total = length exports
    exportLines = [n <> " :: " <> ty | (n, ty) <- shown]
    producerCount = length (filter produces exports)
    envelope =
        object
            [ "state" .= ("found" :: Text)
            , "card"
                .= object
                    [ "package" .= sgPackage sg
                    , "exports" .= exportLines
                    , "note"
                        .= ( tShow total
                                <> " exports; producers of "
                                <> g
                                <> ": "
                                <> tShow producerCount
                           )
                    ]
            , "shown" .= length shown
            , "omitted" .= (total - length shown)
            , "total" .= total
            , "next"
                .= ( "no direct match; the established target "
                        <> sgPackage sg
                        <> " answered from its export card (goal "
                        <> g
                        <> ")"
                   )
            ]
    tShow = T.pack . show

{- | The (name, type) exports of a package via the live catalogue backends:
the capability channel's per-package API slice plus a bounded browse of its
modules — the same sources every discover answer draws from (R7.6).
-}
fetchPackageExports ::
    (ToolName -> Value -> IO (Either Text ToolOutcome)) ->
    Text ->
    IO [(Text, Text)]
fetchPackageExports call pkg = do
    cap <- fetchOk call SearchCapability (object ["query" .= pkg])
    let buckets =
            [ b
            | Just v <- [cap]
            , b <- arrayAt "hits" v
            , textAt "package" b == pkg
            ]
        api =
            [ (textAt "name" e, textAt "type" e)
            | b <- buckets
            , e <- arrayAt "api" b
            , not (T.null (textAt "name" e))
            ]
        mods = take 3 [m | b <- buckets, String m <- arrayAt "modules" b]
    browsed <- concat <$> mapM browseExports mods
    pure (take 60 (dedupOn fst (api ++ browsed)))
  where
    browseExports m = do
        r <- fetchOk call FindFunction (object ["query" .= m])
        pure $ case r of
            Just v ->
                [ (T.strip n, T.strip (T.drop 4 rest))
                | String s <- arrayAt "exports" v
                , let (n, rest) = T.breakOn " :: " s
                , not (T.null rest)
                ]
            Nothing -> []

arrayAt :: Text -> Value -> [Value]
arrayAt k (Object o) = case KM.lookup (K.fromText k) o of
    Just (Array a) -> toList a
    _ -> []
arrayAt _ _ = []

textAt :: Text -> Value -> Text
textAt k (Object o) = case KM.lookup (K.fromText k) o of
    Just (String s) -> s
    _ -> ""
textAt _ _ = ""

dedupOn :: (Eq b) => (a -> b) -> [a] -> [a]
dedupOn f = go []
  where
    go _ [] = []
    go seen (x : xs)
        | f x `elem` seen = go seen xs
        | otherwise = x : go (f x : seen) xs
