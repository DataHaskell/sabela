{-# LANGUAGE OverloadedStrings #-}

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

recordEvidence :: Text -> Value -> Map Text Value -> Map Text Value
recordEvidence cluster v acc = case bestHit (hitsOf v) of
    Nothing -> acc
    Just h -> Map.insertWith keepStronger (T.toLower cluster) h acc
  where
    keepStronger new old = if kindRank new < kindRank old then new else old
    bestHit hs = case sortOn kindRank hs of
        (h : _) -> Just h
        [] -> Nothing

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

sigClamp :: Int
sigClamp = 200

closedSummary :: Maybe Value -> Text -> Text -> Text
closedSummary (Just h) _ ownSummary =
    heldHitLine h <> " — already answered (" <> ownSummary <> ")."
closedSummary Nothing factsText ownSummary =
    ownSummary <> ". Already held" <> factsText <> "."

giveUpLine :: Maybe Value -> [Text] -> Text
giveUpLine (Just h) _ = "already held: " <> heldHitLine h <> "."
giveUpLine Nothing consulted =
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

blockedDenial :: Text -> Text -> Value
blockedDenial qn why =
    object
        [ "query" .= qn
        , "state" .= ("duplicate" :: Text)
        , "ref" .= ("assertion ledger" :: Text)
        , "summary"
            .= ("'" <> qn <> "' is " <> why <> ". Trust the held fact and act on it.")
        ]

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
