{-# LANGUAGE OverloadedStrings #-}

{- | Pure envelope readers and advice rewriting for the search-history ledger
(search-api.md section 8): answer-hash dedup, cluster keys, held-fact
harvesting and clauses, and the one-line duplicate reference. The escalation
rungs live in 'Siza.Agent.Discover.MissLadder'; state lives in
'Siza.Agent.Discover.History'; this module is stateless.
-}
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
import Siza.Agent.Discover.Ledger (installFactKey)

-- | The one-line back-reference of R3.8 and the terse escalation cap.
duplicateEnvelope :: Text -> Text -> Text -> Value
duplicateEnvelope q ref summary =
    object
        [ "query" .= q
        , "state" .= ("duplicate" :: Text)
        , "ref" .= ref
        , "summary" .= summary
        ]

{- | The answer-hash dedup reference (section 10): same answer, new spelling.
Every stratum dedups (round 8); a weak wall's reference labels itself weak
so the repetition is visible without ever being promoted to a fact.
-}
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

{- | The dedup key of an envelope's ANSWER part (section 10): the ranked hits,
card, state and total — never the query echo — so a different spelling whose
answer is byte-identical is recognisable. 'Nothing' when there is no
substantive answer to dedup (misses keep the escalation ladder). Dedup
participation is stratum-blind (round 8): 'strongEvidence' gates only the
assertion ledger (11.1), never this key.
-}
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

{- | Only strata 1-5 evidence — an exact-kind hit or a package/module card —
creates protected assertions or backs a dedup reference (section 11.1);
substring/synonym/semantic hits assert nothing protected.
-}
strongEvidence :: Value -> Bool
strongEvidence v =
    any ((== "exact") . topText "matchKind") (hitsOf v)
        || isJust (cardOf v)

{- | Fold newly harvested facts into a bounded held-facts list (R5.6). A
fresh install-state fact REPLACES the package's earlier one — one package
never holds two install states at once.
-}
harvestInto :: Value -> [Text] -> [Text]
harvestInto v facts = take maxHeldFacts (foldl addFact facts (harvestFacts v))
  where
    addFact acc f
        | f `elem` acc || T.null f = acc
        | Just p <- installFactKey f =
            [g | g <- acc, installFactKey g /= Just p] ++ [f]
        | otherwise = acc ++ [f]

maxHeldFacts :: Int
maxHeldFacts = 8

{- | Held facts worth keeping: install/cabal facts from EXACT hits only
(11.1 strength), tagged with the name they provide so relevance stays
computable from the fact ('Siza.Agent.Discover.FactSelect'); alias notes;
and the call-ready name + signature of an exact typed hit (R5.6).
-}
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

{- | Render held facts, or point at the SHIPPED narrowing knobs when none
are held — the advice must only ever name arguments discover offers.
-}
factsClause :: [Text] -> Text
factsClause facts = case facts of
    [] ->
        ": nothing held bears on this yet — discover a package or module \
        \name (narrow with module= or package=, or list a topic's packages \
        \with mode=\"inventory\"), or act"
    fs -> ": " <> T.intercalate "; " fs

{- | The miss cluster (R5.6, section 11.1): the lowercased resolved target
PLUS the request scope — "no bar in Granite" and "no bar anywhere" are
different propositions and must never share a ledger key.
-}
clusterOf :: Value -> Text -> Text
clusterOf v qn = clusterName v qn <> clusterScope qn

-- | The name part of a cluster: the lowercased resolved target's head word.
clusterName :: Value -> Text -> Text
clusterName v qn = T.toLower (resolvedTarget v qn)

-- | The case-preserving resolved target's head word (query fallback).
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

{- | The scope part of a normalised request key: its bracketed module\/
package\/mode segments. @limit@ never scopes a proposition, so it is skipped.
-}
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
