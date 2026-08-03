{-# LANGUAGE OverloadedStrings #-}

module Siza.Agent.Discover.Closure (
    bestHeldFor,
    blockedDenial,
    closedSummary,
    consultedOf,
    entityOf,
    giveUpLine,
    heldHitLine,
    heldHitsFor,
    heldPayload,
    kindRank,
    protectedBy,
    recordEvidence,
    stripTried,
    worldNote,
) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Pair)
import Data.Foldable (toList)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Siza.Agent.Discover.Beside (restatesHit)
import Siza.Agent.Discover.Types (typeClause)

entityOf :: Text -> Text
entityOf = T.toLower . T.takeWhile (/= ' ') . T.strip

worldNote :: Text
worldNote =
    "world changed: a dependency install or kernel restart landed since the \
    \last search — earlier install-state answers may be stale; this answer \
    \re-checked the live catalogue"

stripTried :: Set Text -> Text -> Text
stripTried tried next = case T.breakOn marker next of
    (_, rest) | T.null rest -> next
    (pre, rest) ->
        let body = T.drop (T.length marker) rest
            (listPart, after) = T.breakOn "." body
            kept =
                [ s
                | s <- map T.strip (T.splitOn "," listPart)
                , not (T.null s)
                , T.toLower s `Set.notMember` tried
                ]
         in if null kept
                then T.stripEnd pre <> T.drop 1 after
                else pre <> marker <> " " <> T.intercalate ", " kept <> after
  where
    marker = "Nearest held names:"

kindRank :: Value -> Int
kindRank h = case topText "matchKind" h of
    "exact" -> 0
    "prefix" -> 1
    "module" -> 2
    "type" -> 3
    "substring" -> 4
    "synonym" -> 5
    "semantic" -> 6
    _ -> 7

{- | Evidence is filed under the whole question — entity AND scope — and keeps
that answer's own ranked order. The latest answer to a question wins, so a
better-scoped call can dislodge a first exact hit.
-}
recordEvidence :: Text -> Value -> Map Text [Value] -> Map Text [Value]
recordEvidence cluster v acc = case hitsOf v of
    [] -> acc
    hs -> Map.insert (T.toLower (T.strip cluster)) hs acc

-- | The entity half of a cluster key; the scope half is everything from @\@@.
keyEntity :: Text -> Text
keyEntity = T.takeWhile (/= '@')

keyScope :: Text -> Text
keyScope = T.dropWhile (/= '@')

{- | Held hits for a question, never for a different one: a record answers
only under the scope it was recorded with, so a module-scoped call is never
answered from a module that scope excludes.
-}
heldHitsFor :: Map Text [Value] -> Text -> [Value]
heldHitsFor evidence key = case Map.lookup k evidence of
    Just hs -> hs
    Nothing -> case sortOn rankOf sweep of
        (hs : _) -> hs
        [] -> []
  where
    k = T.toLower (T.strip key)
    sweep =
        [ hs
        | (k', hs) <- Map.toAscList evidence
        , keyScope k' == keyScope k
        , any (names (keyEntity k)) hs
        ]
    rankOf hs = case hs of
        (h : _) -> kindRank h
        [] -> maxBound
    names e h =
        e `elem` [T.toLower (topText f h) | f <- ["name", "module", "package"]]

bestHeldFor :: Map Text [Value] -> Text -> Maybe Value
bestHeldFor evidence key = case heldHitsFor evidence key of
    (h : _) -> Just h
    [] -> Nothing

heldHitLine :: Value -> Text
heldHitLine h =
    "`"
        <> topText "name" h
        <> "`"
        <> sig
        <> " — "
        <> topText "module" h
        <> " ("
        <> T.intercalate
            ", "
            (filter (not . T.null) [pkgVer, topText "install" h, cabal])
        <> ")"
  where
    sig = case topText "type" h of
        "" -> ""
        t
            | T.length t > sigClamp ->
                " "
                    <> typeClause (T.take sigClamp t)
                    <> "… (truncated — run check_type "
                    <> topText "name" h
                    <> " for the full signature)"
            | otherwise -> " " <> typeClause t
    pkgVer = T.strip (topText "package" h <> " " <> topText "version" h)
    cabal = topText "cabal" h

sigClamp :: Int
sigClamp = 200

closedSummary :: Maybe Value -> Text -> Text -> Text
closedSummary (Just h) _ ownSummary =
    heldHitLine h <> " — already answered (" <> ownSummary <> ")."
closedSummary Nothing factsText ownSummary =
    ownSummary <> ". Already held" <> factsText <> "."

{- | What the closure states beside the hits the same answer carries. A best
held hit those hits already are is stated by them, in fields, and saying it
again in prose spends the caller's budget twice.
-}
giveUpLine :: [Value] -> Maybe Value -> [Text] -> Text
giveUpLine carried (Just h) _
    | restatesHit carried line = ""
    | otherwise = "already held: " <> line <> "."
  where
    line = heldHitLine h
giveUpLine _ Nothing consulted =
    "no match in any recorded answer (consulted: "
        <> T.intercalate ", " (if null consulted then ["none"] else consulted)
        <> ")."

protectedBy :: Set Text -> Map Text (Int, Text) -> Text -> Maybe Text
protectedBy seeded asserted c
    | global
    , c `Set.member` seeded =
        Just
            "part of the notebook environment (an imported module or a \
            \documented builtin) — it cannot be absent"
    | Just (n, s) <- Map.lookup c asserted = Just (previously n s)
    | global
    , ((n, s) : _) <-
        [a | (k, a) <- Map.toList asserted, nameOf k == c, k /= c] =
        Just (previously n s)
    | otherwise = Nothing
  where
    global = not ("@" `T.isInfixOf` c)
    nameOf = T.takeWhile (/= '@')
    previously n s =
        "previously found (call "
            <> T.pack (show n)
            <> ": "
            <> s
            <> ") and the catalogue has not changed since"

{- | The hits a duplicate hands back, projected the way a fresh answer
projects them. A repeat that returns less than the call it deduped is answered
by mutating the query, which costs more than the dedup saved.
-}
heldPayload :: [Value] -> [Pair]
heldPayload [] = []
heldPayload hs = ["hits" .= hs, "shown" .= length hs]

blockedDenial :: [Value] -> Text -> Text -> Value
blockedDenial held qn why =
    object $
        [ "query" .= qn
        , "state" .= ("duplicate" :: Text)
        , "ref" .= ("assertion ledger" :: Text)
        , "summary" .= ("'" <> qn <> "' is " <> why <> ".")
        ]
            <> heldPayload held

consultedOf :: Value -> [Text]
consultedOf v = case v of
    Object o
        | Just (Array rows) <- KM.lookup "consulted" o ->
            [s | row <- toList rows, let s = topText "source" row, not (T.null s)]
    _ -> []

hitsOf :: Value -> [Value]
hitsOf (Object o) = case KM.lookup "hits" o of
    Just (Array a) -> toList a
    _ -> []
hitsOf _ = []

topText :: Text -> Value -> Text
topText k (Object o) = case KM.lookup (K.fromText k) o of
    Just (String s) -> s
    _ -> ""
topText _ _ = ""
