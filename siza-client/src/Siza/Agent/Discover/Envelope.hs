module Siza.Agent.Discover.Envelope (
    badRequest,
    envelopeCharBudget,
    envelopeChars,
    boundEnvelope,
    envelopeViolations,
    maxTypeChars,
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
import Siza.Agent.Discover.Affordance (clashBudget)
import Siza.Agent.Discover.Evict (
    adjustKey,
    clampStr,
    dropAmbiguousWith,
    dropProducerHint,
    elidableFields,
    elidedViolations,
    maxTypeChars,
    overKey,
    shrinkNarrow,
    shrinkSummary,
 )
import Siza.Agent.Discover.Types (InstallState, installText)

badRequest :: Text -> Text -> Value
badRequest q reason =
    object
        [ "query" .= q
        , "state" .= ("bad_request" :: Text)
        , "reason" .= reason
        ]

{- | The per-payload character budget. Measured over the wave-2 live rounds,
the median payload sat at 2307 against a 2500 cap, so the cap was not binding
and every advisory field rode free; 2000 makes the ladder below decide.
-}
envelopeCharBudget :: Int
envelopeCharBudget = 2000

envelopeChars :: Value -> Int
envelopeChars = T.length . TE.decodeUtf8 . LBS.toStrict . encode

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
    , "elided"
    ]

goalKeys :: [Text]
goalKeys = ["type", "satisfied", "note", "derivedFrom"]

envelopeStates :: [Text]
envelopeStates = ["found", "not_found", "bad_request", "duplicate"]

{- | Required means computed on every path. `module`, `package` and `version`
are stated when known and omitted when not, rather than minted as a
placeholder the caller cannot act on.
-}
requiredHitKeys :: [Text]
requiredHitKeys = ["name", "install", "matchKind", "origin"]

optionalHitKeys :: [Text]
optionalHitKeys =
    ["module", "package", "version", "type", "cabal", "use", "ambiguousWith"]

hitKeys :: [Text]
hitKeys = requiredHitKeys ++ optionalHitKeys

{- | What the schema guarantees, stated no more strongly than the merge can
back: `use` and `ambiguousWith` need a computed module and a name Haskell can
write, which a package-only or catalogue-only hit does not have.
-}
schemaPromise :: Text
schemaPromise =
    "Every hit names its "
        <> T.intercalate ", " (drop 1 requiredHitKeys)
        <> "; "
        <> T.intercalate ", " optionalHitKeys
        <> " are stated when known and omitted when not; a hit naming both an "
        <> "importable module and a writable name says in `use` how to bring it "
        <> "into scope, qualified when another shown hit answers to the same "
        <> "bare name, and the first such hit counts the others in "
        <> "`ambiguousWith` and names one, within a "
        <> tShow clashBudget
        <> "-character cap per answer; install is one of "
        <> T.intercalate " | " (map installText [minBound .. maxBound :: InstallState])
        <> "; a hidden or absent-known package carries its -- cabal: \
           \build-depends: line. "
        <> elidedPromise

{- | The disclosure half of the budget: an over-wide answer sheds advice, and
says which and how much, so a reader can ask for the class back rather than
read its absence as "there was none".
-}
elidedPromise :: Text
elidedPromise =
    "An answer over its character budget sheds advisory fields and names each \
    \shed class in `elided` as {field, count}; the shed-able classes are "
        <> T.intercalate ", " elidableFields
        <> ". Ask again narrowed, or for the named class, to get one back."

{- | Shed the least load-bearing bytes first: advice, then notes, then card
width, then hits. The last hit is never shed, so an envelope can come back
over budget.
-}
boundEnvelope :: Value -> Value
boundEnvelope =
    shrinkWith
        [ dropProducerHint
        , dropAmbiguousWith
        , shrinkNarrow
        , shrinkCard
        , shrinkSummary
        , dropLastHit
        , clampHitTypes
        , clampNotes
        ]

exportFloor :: Int
exportFloor = 8

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

shrinkCard :: Value -> Maybe Value
shrinkCard = overKey "card" shrunk
  where
    shrunk (Object c)
        | Just (Array es) <- KM.lookup "exports" c
        , let es' = toList es
        , length es' > exportFloor =
            Just
                . Object
                . KM.insert "exports" (toJSON (init es'))
                . KM.insert "moreExports" (Number (fromIntegral (more c + 1)))
                $ c
    shrunk _ = Nothing
    more c = case KM.lookup "moreExports" c of
        Just (Number n) -> round n :: Int
        _ -> 0

{- | Shed the tail of the hit list to fit the budget, never the last hit: the
hits are the answer, and an envelope that sheds all of them reports nothing.
-}
dropLastHit :: Value -> Maybe Value
dropLastHit (Object o)
    | Just (Array hits) <- KM.lookup "hits" o
    , let hits' = toList hits
    , length hits' > 1 =
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

{- | Clamp the signatures of the hits the envelope must keep, so a single
oversized hit still shrinks toward the budget instead of stopping above it.
-}
clampHitTypes :: Value -> Maybe Value
clampHitTypes (Object o)
    | Just (Array hits) <- KM.lookup "hits" o
    , let hits' = map clampType (toList hits)
    , hits' /= toList hits =
        Just (Object (KM.insert "hits" (toJSON hits') o))
  where
    clampType (Object h) = Object (adjustKey clampStr "type" h)
    clampType x = x
clampHitTypes _ = Nothing

clampNotes :: Value -> Maybe Value
clampNotes (Object o)
    | any clampable ["summary", "reason"] =
        Just (Object (foldr (adjustKey clampStr) o ["summary", "reason"]))
  where
    clampable k = case KM.lookup (K.fromText k) o of
        Just (String t) -> T.length t > maxTypeChars
        _ -> False
clampNotes _ = Nothing

envelopeViolations :: Value -> [Text]
envelopeViolations v@(Object o) =
    keyViols
        ++ stateViols
        ++ hitViols
        ++ goalViols
        ++ elidedViolations v
        ++ stringViols v
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
        Just (Array hs) -> concatMap hitViol hs
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

stringViols :: Value -> [Text]
stringViols (Object o) = concatMap stringViols (KM.elems o)
stringViols (Array a) = concatMap stringViols a
stringViols (String s) =
    [ "serialisation inside a string: " <> T.take 40 s
    | embeddedSerialisation s
    ]
        ++ [ "package-hash name: " <> T.take 60 s
           | any leakyToken (T.words s)
           ]
stringViols _ = []

tShow :: Int -> Text
tShow = T.pack . show

textAt :: Text -> KM.KeyMap Value -> Text
textAt k o = case KM.lookup (K.fromText k) o of
    Just (String s) -> s
    _ -> ""
