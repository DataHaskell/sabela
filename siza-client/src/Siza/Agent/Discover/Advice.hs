{-# LANGUAGE OverloadedStrings #-}

module Siza.Agent.Discover.Advice (
    answerDup,
    answerKey,
    answerKeyFields,
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
    scopedFacts,
    setField,
    setNext,
    stripTried,
    strongEvidence,
    tShow,
    topText,
    totalOf,
) where

import Data.Aeson (Value (..), encode, object, toJSON, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Siza.Agent.Discover.Beside (besideHits)
import Siza.Agent.Discover.Closure (heldPayload, stripTried)
import Siza.Agent.Discover.Facts (foldFacts, maxHeldFacts)
import Siza.Agent.Discover.Types (typeClause)

{- | A duplicate hands back the payload it deduped. Returning strictly less
than the call it stands for is answered by mutating the query, which costs
more than the dedup saved.
-}
duplicateEnvelope :: Text -> Text -> Text -> [Value] -> Value
duplicateEnvelope q ref summary held =
    object $
        [ "query" .= q
        , "state" .= ("duplicate" :: Text)
        , "ref" .= ref
        , "summary" .= besideHits held summary
        ]
            <> heldPayload held

-- | The dedup states what it compared, and returns what it compared against.
answerDup :: Value -> Text -> Int -> Text -> Value
answerDup v qn n q0 =
    duplicateEnvelope
        qn
        ("call " <> tShow n <> " ('" <> q0 <> "')")
        (label <> " as call " <> tShow n <> comparedClause)
        (hitsOf v)
  where
    label
        | strongEvidence v = "same ranked answer"
        | otherwise = "same weak answer (no exact hit or card)"

answerKey :: Value -> Maybe Text
answerKey v
    | topText "state" v == "found"
    , not (null (hitsOf v)) =
        Just key
    | otherwise = Nothing
  where
    key =
        TE.decodeUtf8 . LBS.toStrict . encode . object $
            [K.fromText k .= x | (k, x) <- answerFacets v]

{- | Everything the dedup compares, named. 'comparedClause' reports these and
only these, so the summary cannot claim more than the equality established.
-}
answerFacets :: Value -> [(Text, Value)]
answerFacets v =
    [ ("state", toJSON (topText "state" v))
    , ("hits", toJSON (hitsOf v))
    , ("card", toJSON (cardOf v))
    , ("total", toJSON (totalOf v))
    ]

answerKeyFields :: [Text]
answerKeyFields = map fst (answerFacets Null)

comparedClause :: Text
comparedClause =
    " (same " <> T.intercalate ", " answerKeyFields <> "; nothing else compared)"

cardOf :: Value -> Maybe Value
cardOf (Object o) = KM.lookup "card" o
cardOf _ = Nothing

{- | Evidence strong enough to assert a fact about the world. A card that
stamped itself as not answering the query is evidence of exactly that, so it
cannot stand in for an answer here either.
-}
strongEvidence :: Value -> Bool
strongEvidence v =
    any ((== "exact") . topText "matchKind") (hitsOf v)
        || any answeringCard (cardOf v)

answeringCard :: Value -> Bool
answeringCard (Object c) = KM.lookup "cardAnswers" c /= Just (Bool False)
answeringCard _ = False

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
            <> "` "
            <> typeClause (topText "type" h)
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

{- | Under a scoped question only a fact about that module or package answers
it. Quoting a fact from a scope the caller excluded reports something the
harness did not compute for the question asked.
-}
scopedFacts :: Text -> [Text] -> [Text]
scopedFacts qn facts = case scopeTerms qn of
    [] -> facts
    ts -> [f | f <- facts, any (`elem` factTokens f) ts]

{- | The dotted names a fact states, lowered. A scope term admits a fact only
by naming one outright, so @[module=Data.Map]@ does not admit a fact about
@Data.Map.Strict@, which a substring test would.
-}
factTokens :: Text -> [Text]
factTokens = T.split (`notElem` tokenChars) . T.toLower
  where
    tokenChars = ['a' .. 'z'] ++ ['0' .. '9'] ++ "._-'"

{- | One bracket can carry several axes — @[module=M package=P]@ — so the
segments are split on whitespace as well, or the whole tail is read as one
module name and no held fact ever matches.
-}
scopeTerms :: Text -> [Text]
scopeTerms qn =
    [ T.toLower (T.drop 1 (T.dropWhile (/= '=') seg))
    | seg <- concatMap T.words (T.splitOn "@" (clusterScope qn))
    , any (`T.isPrefixOf` seg) ["module=", "package="]
    ]

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
        (h : _) -> "; top: " <> topText "name" h <> stated h
        [] -> ""
    stated h = case typeClause (topText "type" h) of
        "" -> ""
        c -> " " <> c

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
