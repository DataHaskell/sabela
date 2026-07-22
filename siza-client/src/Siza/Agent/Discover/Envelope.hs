{- | The discover envelope's declared budget and schema (search-api.md
sections 5 and 10): one shape for every outcome, a 2,500-char cap enforced by
construction (shrink hits\/exports, never blind truncation), and the schema
vocabulary the tool description is generated from (R1.7).
-}
module Siza.Agent.Discover.Envelope (
    badRequest,
    envelopeCharBudget,
    envelopeChars,
    boundEnvelope,
    envelopeViolations,
    schemaPromise,
    envelopeKeys,
    goalKeys,
    hitKeys,
    requiredHitKeys,
    envelopeStates,
    stringViols,
) where

import Data.Aeson (Value (..), encode, object, toJSON, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Sabela.AI.LeakShape (embeddedSerialisation, leakyToken)
import Siza.Agent.Discover.Types (InstallState, installText)

-- | A malformed request (blank query, bad bounds) is an argument error (R2.6).
badRequest :: Text -> Text -> Value
badRequest q reason =
    object
        [ "query" .= q
        , "state" .= ("bad_request" :: Text)
        , "reason" .= reason
        ]

-- | The hard serialised cap; the typical envelope stays well under 1k (R3.9).
envelopeCharBudget :: Int
envelopeCharBudget = 2500

-- | The serialised size of an envelope, as the model would receive it.
envelopeChars :: Value -> Int
envelopeChars = T.length . TE.decodeUtf8 . LBS.toStrict . encode

-- | Every legal top-level field of the one envelope shape (R3.6).
envelopeKeys :: [Text]
envelopeKeys =
    [ "query"
    , "interpreted"
    , "state"
    , "card"
    , "hits"
    , "shown"
    , "omitted"
    , "total"
    , "narrow"
    , "consulted"
    , "next"
    , "reason"
    , "ref"
    , "summary"
    , "worldChange"
    , "goal"
    ]

{- | The section 8.3 goal disclosure's declared shape: present iff the
cluster has a resolved goal; @type@ and @satisfied@ are required;
@derivedFrom@ cites the consumer fact the goal derives from (R9-T2).
-}
goalKeys :: [Text]
goalKeys = ["type", "satisfied", "note", "derivedFrom"]

-- | The four honest states; backend unavailability is per-source, never a state.
envelopeStates :: [Text]
envelopeStates = ["found", "not_found", "bad_request", "duplicate"]

-- | Per-hit provenance always present (R3.5); see 'hitKeys' for optionals.
requiredHitKeys :: [Text]
requiredHitKeys =
    ["name", "module", "package", "version", "install", "matchKind", "origin"]

hitKeys :: [Text]
hitKeys = requiredHitKeys ++ ["type", "cabal", "use"]

{- | The description clause generated from the schema itself, so the advertised
contract cannot drift from the delivered envelope (R1.7).
-}
schemaPromise :: Text
schemaPromise =
    "Every hit names its "
        <> T.intercalate ", " (drop 1 requiredHitKeys)
        <> "; install is one of "
        <> T.intercalate " | " (map installText [minBound .. maxBound :: InstallState])
        <> "; a hidden or absent-known package carries its -- cabal: \
           \build-depends: line."

{- | Enforce the budget by construction: fold card exports into
@moreExports@, then shed trailing hits into @omitted@ — counts always
reconcile (R3.4). Load-bearing fields (a hit's @type@\/@cabal@, @next@) are
never truncated (section 10): the bound sheds hits, never protected fields.
Clamping unprotected advice text is the never-hit backstop.
-}
boundEnvelope :: Value -> Value
boundEnvelope = shrinkWith [shrinkCard, dropLastHit, clampNotes]

shrinkWith :: [Value -> Maybe Value] -> Value -> Value
shrinkWith steps v
    | envelopeChars v <= envelopeCharBudget = v
    | otherwise = case tryEach steps of
        Just v' -> shrinkWith steps v'
        Nothing -> v
  where
    tryEach [] = Nothing
    tryEach (s : rest) = case s v of
        Just v' | v' /= v -> Just v'
        _ -> tryEach rest

maxTypeChars :: Int
maxTypeChars = 200

-- | Fold the card's last export line into a @moreExports@ count.
shrinkCard :: Value -> Maybe Value
shrinkCard = overKey "card" shrunk
  where
    shrunk (Object c)
        | Just (Array es) <- KM.lookup "exports" c
        , let es' = toList es
        , not (null es') =
            Just
                . Object
                . KM.insert "exports" (toJSON (init es'))
                . KM.insert "moreExports" (Number (fromIntegral (more c + 1)))
                $ c
    shrunk _ = Nothing
    more c = case KM.lookup "moreExports" c of
        Just (Number n) -> round n :: Int
        _ -> 0

-- | Shed the last (worst-ranked) hit; @shown@\/@omitted@ move with it.
dropLastHit :: Value -> Maybe Value
dropLastHit (Object o)
    | Just (Array hits) <- KM.lookup "hits" o
    , let hits' = toList hits
    , not (null hits') =
        Just
            . Object
            . KM.insert "hits" (toJSON (init hits'))
            . KM.insert "shown" (Number (fromIntegral (length hits' - 1)))
            . KM.insert "omitted" (Number (fromIntegral (omitted + 1)))
            $ o
  where
    omitted = case KM.lookup "omitted" o of
        Just (Number n) -> round n :: Int
        _ -> 0
dropLastHit _ = Nothing

{- | Last-resort backstop: clamp free-text advice fields. @next@ is a
load-bearing key (section 10) and is never clamped.
-}
clampNotes :: Value -> Maybe Value
clampNotes (Object o)
    | any clampable ["summary", "reason"] =
        Just (Object (foldr (adjustKey clampStr) o ["summary", "reason"]))
  where
    clampable k = case KM.lookup (K.fromText k) o of
        Just (String t) -> T.length t > maxTypeChars
        _ -> False
clampNotes _ = Nothing

-- | Clamp a string value to 'maxTypeChars'; other values pass unchanged.
clampStr :: Value -> Value
clampStr (String t)
    | T.length t > maxTypeChars = String (T.take maxTypeChars t <> "…")
clampStr v = v

adjustKey :: (Value -> Value) -> Text -> KM.KeyMap Value -> KM.KeyMap Value
adjustKey f k o = case KM.lookup (K.fromText k) o of
    Just v -> KM.insert (K.fromText k) (f v) o
    Nothing -> o

-- | Apply a partial rewrite to one key of an object envelope.
overKey :: Text -> (Value -> Maybe Value) -> Value -> Maybe Value
overKey k f (Object o) = do
    v <- KM.lookup (K.fromText k) o
    v' <- f v
    pure (Object (KM.insert (K.fromText k) v' o))
overKey _ _ _ = Nothing

{- | Schema violations of an envelope (R3.6, R3.10): unknown fields, a state
outside the declared four, a hit missing required provenance, a serialisation
format inside a string, or a package-hash-qualified name.
-}
envelopeViolations :: Value -> [Text]
envelopeViolations v@(Object o) =
    keyViols ++ stateViols ++ hitViols ++ goalViols ++ stringViols v
  where
    goalViols = case KM.lookup "goal" o of
        Nothing -> []
        Just (Object g) ->
            [ "unknown goal field: " <> K.toText k
            | k <- KM.keys g
            , K.toText k `notElem` goalKeys
            ]
                ++ ["goal.type missing" | T.null (textAt "type" g)]
                ++ [ "goal.satisfied missing or not a bool"
                   | not (isBoolAt "satisfied" g)
                   ]
        Just _ -> ["goal is not an object"]
    isBoolAt k g = case KM.lookup k g of
        Just (Bool _) -> True
        _ -> False
    keyViols =
        [ "unknown envelope field: " <> K.toText k
        | k <- KM.keys o
        , K.toText k `notElem` envelopeKeys
        ]
    state = textAt "state" o
    stateViols =
        ["state missing or unknown: " <> state | state `notElem` envelopeStates]
    hitViols = case KM.lookup "hits" o of
        Just (Array hs) -> concatMap hitViol (toList hs)
        Just _ -> ["hits is not an array"]
        Nothing -> []
envelopeViolations _ = ["envelope is not an object"]

hitViol :: Value -> [Text]
hitViol (Object h) =
    [ "unknown hit field: " <> K.toText k
    | k <- KM.keys h
    , K.toText k `notElem` hitKeys
    ]
        ++ [ "hit missing required field: " <> k
           | k <- requiredHitKeys
           , T.null (textAt k h)
           ]
hitViol _ = ["hit is not an object"]

{- | Deep string scan (shared shape detectors, "Sabela.AI.LeakShape"): no
embedded serialisation, no package-hash or version-hash-qualified names.
-}
stringViols :: Value -> [Text]
stringViols (Object o) = concatMap stringViols (KM.elems o)
stringViols (Array a) = concatMap stringViols (toList a)
stringViols (String s) =
    [ "serialisation inside a string: " <> T.take 40 s
    | embeddedSerialisation s
    ]
        ++ [ "package-hash name: " <> T.take 60 s
           | any leakyToken (T.words s)
           ]
stringViols _ = []

textAt :: Text -> KM.KeyMap Value -> Text
textAt k o = case KM.lookup (K.fromText k) o of
    Just (String s) -> s
    _ -> ""
