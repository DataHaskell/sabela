{-# LANGUAGE OverloadedStrings #-}

{- | Denial-legal closure (docs/discover/search-api.md section 8.2): the
ledger records the best hit each answered cluster held, and every close or
give-up envelope runs one final union sweep over that recorded evidence —
a close either hands the held hit over (name, signature, provenance,
install state, cabal line) or is provably over an empty merged catalogue.
-}
module Siza.Agent.Discover.Closure (
    bestHeldFor,
    blockedDenial,
    closedSummary,
    consultedOf,
    entityOf,
    giveUpLine,
    heldHitLine,
    kindRank,
    protectedBy,
    recordEvidence,
    stripTried,
    worldNote,
) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

-- | The entity a ledger key names: its head word, scope brackets dropped.
entityOf :: Text -> Text
entityOf = T.toLower . T.takeWhile (/= ' ') . T.strip

-- | The world-change announcement riding the first post-change answer (R1.4).
worldNote :: Text
worldNote =
    "world changed: a dependency install or kernel restart landed since the \
    \last search — earlier install-state answers may be stale; this answer \
    \re-checked the live catalogue"

-- | Remove already-tried names from the nearest-names suggestion (R5.5).
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

-- | Stratum rank of a hit's matchKind; lower is stronger evidence.
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

{- | Record the best hit of a found envelope under its cluster name. Weaker
tiers are recorded too (section 8.2 hands over even weaker-tier evidence);
a stronger hit for the same cluster replaces a weaker one.
-}
recordEvidence :: Text -> Value -> Map Text Value -> Map Text Value
recordEvidence cluster v acc = case bestHit (hitsOf v) of
    Nothing -> acc
    Just h -> Map.insertWith keepStronger (T.toLower cluster) h acc
  where
    keepStronger new old = if kindRank new < kindRank old then new else old
    bestHit hs = case sortOn kindRank hs of
        (h : _) -> Just h
        [] -> Nothing

{- | The final ledger-union sweep for an entity a query named: a direct
cluster lookup, else any recorded hit whose name, module or package matches
the entity — so "no answer held anywhere" is quantified over everything the
session was ever told, never over one scope's record.
-}
bestHeldFor :: Map Text Value -> Text -> Maybe Value
bestHeldFor evidence entity = case Map.lookup e evidence of
    Just h -> Just h
    Nothing -> case sortOn kindRank (filter names (Map.elems evidence)) of
        (h : _) -> Just h
        [] -> Nothing
  where
    e = T.toLower (T.strip entity)
    names h =
        e
            `elem` [ T.toLower (topText k h)
                   | k <- ["name", "module", "package"]
                   ]

{- | One bounded line carrying a held hit's full provenance: name, signature,
module, package+version, install state, and its cabal line. A signature over
'sigClamp' chars is clamped WITH disclosure — the omission and its recovery
call are named, never bare-truncated (section 10 protect-by-key).
-}
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
                " :: "
                    <> T.take sigClamp t
                    <> "… (truncated — run check_type "
                    <> topText "name" h
                    <> " for the full signature)"
            | otherwise -> " :: " <> t
    pkgVer = T.strip (topText "package" h <> " " <> topText "version" h)
    cabal = topText "cabal" h

-- | The held-hit signature clamp; past it the line discloses and recovers.
sigClamp :: Int
sigClamp = 200

{- | The post-close reference for a SEEN key: leads with the held hit when
the sweep finds one, else replays the key's own summary — never a foreign
scope's — and never advises further searching. @factsText@ is the caller's
pre-rendered held-facts clause (kept out of this module's dependencies).
-}
closedSummary :: Maybe Value -> Text -> Text -> Text
closedSummary (Just h) _ ownSummary =
    heldHitLine h
        <> " — already answered ("
        <> ownSummary
        <> "). Searching further cannot help because you already hold the \
           \answer; act on it, or state the blocker plainly."
closedSummary Nothing factsText ownSummary =
    ownSummary
        <> ". Discovery is closed: act on what is held"
        <> factsText
        <> ", or state the blocker plainly."

{- | The rung-3 give-up line (R5.4): hand over the held hit, or state the
scoped emptiness naming what was consulted — the only legal bare cannot-help.
-}
giveUpLine :: Maybe Value -> [Text] -> Text
giveUpLine (Just h) _ =
    "Searching further cannot help — you already hold the answer: "
        <> heldHitLine h
        <> ". Act on it, or state the blocker plainly."
giveUpLine Nothing consulted =
    "Searching further cannot help: nothing was found for this in any \
    \recorded answer (consulted: "
        <> T.intercalate ", " (if null consulted then ["none"] else consulted)
        <> ")."

{- | Why a denial of cluster @c@ is illegal, if it is (section 11.1): a
turn-0 environment fact, or an asserted fact whose scope COVERS the denial's
— a scoped find covers a global denial, never the reverse.
-}
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

-- | The blocked-denial reference: the held assertion, never the false miss.
blockedDenial :: Text -> Text -> Value
blockedDenial qn why =
    object
        [ "query" .= qn
        , "state" .= ("duplicate" :: Text)
        , "ref" .= ("assertion ledger" :: Text)
        , "summary"
            .= ("'" <> qn <> "' is " <> why <> ". Trust the held fact and act on it.")
        ]

-- | The source names an envelope's consulted rows carry.
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
