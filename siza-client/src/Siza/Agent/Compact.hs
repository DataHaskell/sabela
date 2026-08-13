{-# LANGUAGE OverloadedStrings #-}

{- |
Technique: transcript elision [Context Economy].
Guarantee: 'mustKeep' (shared with Siza.Agent.EmitLedger): no elision drops a diagnostic, verdict, or failure.
Entry: 'compactWith'. Next: Siza.Agent.Recall.
-}
module Siza.Agent.Compact (
    compactSeed,
    compactWith,
    resultStubFor,
    isResultStub,
    actionableKeys,
    carriesActionable,
    mustKeep,
) where

import Data.Aeson (Value (..), decodeStrict, encode, toJSON)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isDigit)
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Sabela.AI.Verdict (verdictClasses, verdictMarker)
import Siza.Agent.Recall (freshId, recallHint)

{- | Compact a transcript against the results already recalled from it. The
store carries forward, so an index minted for one prompt still resolves at the
next.
-}
compactWith :: Map Int Text -> [Value] -> ([Value], Map Int Text)
compactWith = go []
  where
    go acc store [] = (reverse acc, store)
    go acc store (m : ms) = case toolContent m of
        Just full
            | elidable full
            , n <- freshId store full
            , stub <- resultStubFor n (toolNameOf m) full
            , T.length stub < T.length full ->
                go (withContent stub m : acc) (Map.insert n full store) ms
        _ -> go (dropThinking m : acc) store ms

compactSeed :: [Value] -> ([Value], Map Int Text)
compactSeed = compactWith Map.empty

{- | A result worth replacing with a reference. A live diagnostic never is:
the model acts on it next turn, and a stub of it costs a round trip.
-}
elidable :: Text -> Bool
elidable full =
    T.length full > stubFloor
        && not (mustKeep full)
        && not (isResultStub full)

stubFloor :: Int
stubFloor = 200

{- | What no elision path may touch: the classes the model must read to decide
its next move. One rule, so compaction and the emit ledger cannot drift apart.
-}
mustKeep :: Text -> Bool
mustKeep chunk =
    carriesActionable chunk || carriesVerdict chunk || reportsFailure chunk

{- | Keys whose non-empty value makes a result actionable, and so exempt from
every elision path. A refusal is named by @notCommitted@, its cause by
@diagnostic@ and its remedy by @guidance@; none of the three may be dropped.
-}
actionableKeys :: [Text]
actionableKeys =
    ["diagnostic", "error", "stderr", "autofix", "notCommitted", "guidance"]

{- | Carries an actionable key under a value that says something: any shape but
an empty string, an empty container, null or false.
-}
carriesActionable :: Text -> Bool
carriesActionable chunk = any nonEmptyValue actionableKeys
  where
    nonEmptyValue k =
        let key = "\"" <> k <> "\":"
         in any
                (inhabited . T.drop (T.length key))
                [rest | (_, rest) <- T.breakOnAll key chunk]
    inhabited v0 =
        let v = T.stripStart v0
         in not (T.null v) && not (any (`T.isPrefixOf` v) emptyValues)
    emptyValues = ["\"\"", "[]", "{}", "null", "false"]

{- | A block the harness stamped with a verdict class. Matching the stamp
rather than a channel name keeps this true of whatever that channel is called,
and of the bare content string the emit ledger is handed.
-}
carriesVerdict :: Text -> Bool
carriesVerdict t = any ((`T.isInfixOf` t) . verdictMarker) verdictClasses

{- | A cell's own output reporting a failure. Prose only: a numeric field named
"error" is a metric, and contracting it loses nothing.
-}
reportsFailure :: Text -> Bool
reportsFailure = any (any failureLine . T.lines) . outputValues
  where
    failureLine l = unambiguous l || markedProse l
    unambiguous l = any (`T.isInfixOf` l) ["*** Exception", "Not in scope:", "CallStack"]
    markedProse l = any (prosePast l) failureMarkers
    prosePast l m = case T.breakOn (m <> ":") (T.toLower l) of
        (_, rest)
            | T.null rest -> False
            | otherwise -> isProse (T.drop (T.length m + 1) rest)
    isProse rest = case T.uncons (T.stripStart rest) of
        Just (c, _) -> not (isDigit c) && c /= '-'
        Nothing -> False

failureMarkers :: [Text]
failureMarkers = ["error", "exception", "failure", "failed", "warning"]

outputValues :: Text -> [Text]
outputValues = go
  where
    key = "\"oiOutput\":\""
    go t = case T.breakOn key t of
        (_, rest)
            | T.null rest -> []
            | otherwise ->
                let body = T.drop (T.length key) rest
                    (v, after) = T.breakOn "\"" (T.replace "\\\"" "  " body)
                 in v : (if T.null after then [] else go (T.drop 1 after))

toolContent :: Value -> Maybe Text
toolContent (Object o)
    | KM.lookup "role" o == Just (String "tool")
    , Just (String c) <- KM.lookup "content" o =
        Just c
toolContent _ = Nothing

toolNameOf :: Value -> Text
toolNameOf (Object o) = case KM.lookup "tool_name" o of
    Just (String t) -> t
    _ -> "tool"
toolNameOf _ = "tool"

withContent :: Text -> Value -> Value
withContent c (Object o) = Object (KM.insert "content" (String c) o)
withContent _ v = v

isResultStub :: Text -> Bool
isResultStub = T.isPrefixOf "[result #" . T.stripStart

{- | What replaces an elided result: its index, its provenance, its shape, and
the call that returns it in full.
-}
resultStubFor :: Int -> Text -> Text -> Text
resultStubFor n toolName full =
    "[result #"
        <> tshow n
        <> " from "
        <> toolName
        <> " ("
        <> tshow (T.length full)
        <> " chars, elided): "
        <> preview full
        <> " — "
        <> recallHint n
        <> "]"

{- | The shape of a result, not its first bytes: prefix position is
uncorrelated with which field the next prompt refers back to.
-}
preview :: Text -> Text
preview full = case decodeStrict (TE.encodeUtf8 full) of
    Just v@(Object _) -> encodeText (projectWithin previewBudget v)
    _ -> T.takeWhile (/= '\n') (T.take stubPreview full) <> "…"

stubPreview :: Int
stubPreview = 90

previewBudget :: Int
previewBudget = 400

{- | The most detailed projection that fits the budget. Keys never shrink, so
the last rung still names every field the result carries.
-}
projectWithin :: Int -> Value -> Value
projectWithin budget v = case [p | p <- rungs, fits (projectAt p v)] of
    (p : _) -> projectAt p v
    [] -> projectAt (0, 0) v
  where
    fits p = T.length (encodeText p) <= budget
    rungs = [(60, 3), (30, 2), (12, 1), (0, 0)]

{- | Keep every key and every scalar; replace what is too big to carry with a
statement of its size. Shape-preserving, so it needs no per-tool schema.
-}
projectAt :: (Int, Int) -> Value -> Value
projectAt (keptChars, keptItems) = go (0 :: Int)
  where
    go d (Object o)
        | d >= maxDepth = String (sizeOf (Object o))
        | otherwise = Object (KM.map (go (d + 1)) o)
    go d (Array a)
        | d >= maxDepth || keptItems <= 0 = String (sizeOf (Array a))
        | otherwise = toJSON (map (go (d + 1)) kept ++ rest)
      where
        items = toList a
        kept = take keptItems items
        rest =
            [ String ("… " <> tshow (length items - keptItems) <> " more")
            | length items > keptItems
            ]
    go _ (String s)
        | T.length s > keptChars =
            String (T.take keptChars s <> "… (" <> tshow (T.length s) <> " chars)")
    go _ v = v
    sizeOf v = tshow (T.length (encodeText v)) <> " chars"
    maxDepth = 3

dropThinking :: Value -> Value
dropThinking (Object o) = Object (KM.delete (K.fromText "thinking") o)
dropThinking v = v

encodeText :: Value -> Text
encodeText = TE.decodeUtf8 . LBS.toStrict . encode

tshow :: (Show a) => a -> Text
tshow = T.pack . show
