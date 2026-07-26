{-# LANGUAGE OverloadedStrings #-}

module Siza.Agent.Discover.Advice (
    answerDup,
    answerKey,
    clusterOf,
    clusterName,
    clusterScope,
    duplicateEnvelope,
    factsClause,
    foundSummary,
    harvestFacts,
    harvestInto,
    hitsOf,
    maxHeldFacts,
    missSummary,
    resolvedTarget,
    setField,
    setNext,
    stripTried,
    strongEvidence,
    tShow,
    topText,
    totalOf,
) where

import Data.Aeson (Value (..), encode, object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (toList)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Siza.Agent.Discover.Closure (stripTried)
import Siza.Agent.Discover.Facts (foldFacts, maxHeldFacts)

duplicateEnvelope :: Text -> Text -> Text -> Value
duplicateEnvelope q ref summary =
    object
        [ "query" .= q
        , "state" .= ("duplicate" :: Text)
        , "ref" .= ref
        , "summary" .= summary
        ]

answerDup :: Bool -> Text -> Int -> Text -> Value
answerDup strong qn n q0 =
    duplicateEnvelope qn ("call " <> tShow n <> " ('" <> q0 <> "')") $
        label
            <> "; your query change did not change the answer"
            <> " — narrow with a module or package scope, or act."
  where
    label
        | strong = "same ranked answer"
        | otherwise = "same weak answer (no exact hit or card)"

answerKey :: Value -> Maybe Text
answerKey v
    | topText "state" v == "found"
    , not (null (hitsOf v)) =
        Just key
    | otherwise = Nothing
  where
    key =
        TE.decodeUtf8 . LBS.toStrict . encode $
            object
                [ "state" .= topText "state" v
                , "hits" .= hitsOf v
                , "card" .= cardOf v
                , "total" .= totalOf v
                ]

cardOf :: Value -> Maybe Value
cardOf (Object o) = KM.lookup "card" o
cardOf _ = Nothing

strongEvidence :: Value -> Bool
strongEvidence v =
    any ((== "exact") . topText "matchKind") (hitsOf v)
        || isJust (cardOf v)

harvestInto :: Value -> [Text] -> [Text]
harvestInto v = foldFacts (harvestFacts v)

harvestFacts :: Value -> [Text]
harvestFacts v = hitFacts ++ sigFacts ++ aliasFacts
  where
    hitFacts =
        [ topText "package" h
            <> " ("
            <> topText "install" h
            <> "): "
            <> topText "cabal" h
            <> providesTag h
        | h <- hitsOf v
        , topText "matchKind" h == "exact"
        , not (T.null (topText "cabal" h))
        ]
    providesTag h =
        let n = topText "name" h
         in if T.null n then "" else " — provides `" <> n <> "`"
    sigFacts =
        [ "`"
            <> topText "name" h
            <> "` :: "
            <> topText "type" h
            <> " — found in "
            <> topText "module" h
            <> " ("
            <> topText "package" h
            <> ")"
        | h <- take 1 (hitsOf v)
        , topText "matchKind" h == "exact"
        , not (T.null (topText "type" h))
        , not (T.null (topText "name" h))
        ]
    aliasFacts =
        [ note
        | Object o <- [v]
        , Just interp <- [KM.lookup "interpreted" o]
        , let note = topText "note" interp
        , "alias" `T.isInfixOf` note
        ]

factsClause :: [Text] -> Text
factsClause facts = case facts of
    [] -> ": nothing held bears on this yet"
    fs -> ": " <> T.intercalate "; " fs

clusterOf :: Value -> Text -> Text
clusterOf v qn = clusterName v qn <> clusterScope qn

clusterName :: Value -> Text -> Text
clusterName v qn = T.toLower (resolvedTarget v qn)

resolvedTarget :: Value -> Text -> Text
resolvedTarget v qn = T.takeWhile (/= ' ') resolved
  where
    resolved = case v of
        Object o
            | Just interp <- KM.lookup "interpreted" o
            , r <- topText "resolved" interp
            , not (T.null r) ->
                r
        _ -> qn

clusterScope :: Text -> Text
clusterScope qn =
    T.concat
        [ "@" <> T.toLower s
        | chunk <- drop 1 (T.splitOn "[" qn)
        , let s = T.takeWhile (/= ']') chunk
        , any (`T.isPrefixOf` s) ["module=", "package=", "mode="]
        ]

foundSummary :: Value -> Text
foundSummary v = tShow (totalOf v) <> " hits" <> topLine
  where
    topLine = case hitsOf v of
        (h : _) ->
            "; top: "
                <> topText "name" h
                <> ( let t = topText "type" h
                      in if T.null t then "" else " :: " <> t
                   )
        [] -> ""

missSummary :: Value -> Text
missSummary v = "no match; " <> T.take 120 (topText "next" v)

topText :: Text -> Value -> Text
topText k (Object o) = case KM.lookup (K.fromText k) o of
    Just (String s) -> s
    _ -> ""
topText _ _ = ""

totalOf :: Value -> Int
totalOf (Object o) = case KM.lookup "total" o of
    Just (Number n) -> round n
    _ -> 0
totalOf _ = 0

hitsOf :: Value -> [Value]
hitsOf (Object o) = case KM.lookup "hits" o of
    Just (Array a) -> toList a
    _ -> []
hitsOf _ = []

setNext :: Text -> Value -> Value
setNext = setField "next"

setField :: Text -> Text -> Value -> Value
setField k t (Object o) = Object (KM.insert (K.fromText k) (String t) o)
setField _ _ v = v

tShow :: Int -> Text
tShow = T.pack . show
