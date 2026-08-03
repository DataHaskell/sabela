{-# LANGUAGE OverloadedStrings #-}

module Siza.Agent.Discover.ProducerCard (
    establishedFallback,
    fetchPackageExports,
    producerCard,
    producerHintMark,
    withProducerHint,
) where

import Data.Aeson (Value (..), object, toJSON, (.=))
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
import Siza.Agent.Discover.Goal (argTypesOf, nominalArgType, producesGoal)
import Siza.Agent.Discover.Request (DiscoverRequest (..))
import Siza.Agent.Discover.Types (StandingGoal (..), exportRow)

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
    exportLines = [exportRow n ty | (n, ty) <- shown]
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

{- | The marker a producer hint begins with. It is the seam the envelope's
eviction ladder cuts on, so the hint can be shed without touching the `use`
line it was appended to.
-}
producerHintMark :: Text
producerHintMark = "; producer: "

maxHintChars :: Int
maxHintChars = 80

{- | Name, on the top exact hit's `use`, a value of an argument type that hit
needs and cannot make: a nullary producer the same envelope already holds,
from the same package. Anything weaker is a lookup, so it is not stated.
-}
withProducerHint :: Value -> Value
withProducerHint v@(Object o) = case (hits, hint) of
    (_ : rest, Just h) -> Object (KM.insert "hits" (toJSON (hinted h : rest)) o)
    _ -> v
  where
    hits = arrayAt "hits" v
    top = case hits of
        (h : _) -> h
        [] -> Null
    hint =
        firstOf
            [ producerHintMark <> n <> " :: " <> ty
            | textAt "matchKind" top == "exact"
            , not (T.null (textAt "use" top))
            , not (T.null pkg)
            , ty <- argTypesOf (textAt "type" top)
            , nominalArgType ty
            , p <- drop 1 hits
            , textAt "package" p == pkg
            , T.strip (textAt "type" p) == ty
            , n <- [textAt "name" p]
            , not (T.null n)
            ]
    pkg = textAt "package" top
    hinted t = case top of
        Object h -> Object (KM.insert "use" (String (textAt "use" top <> t)) h)
        _ -> top
withProducerHint v = v

firstOf :: [Text] -> Maybe Text
firstOf ts = case [t | t <- ts, T.length t <= maxHintChars] of
    (t : _) -> Just t
    [] -> Nothing

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
