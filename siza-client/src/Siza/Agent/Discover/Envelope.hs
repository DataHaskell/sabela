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

badRequest :: Text -> Text -> Value
badRequest q reason =
    object
        [ "query" .= q
        , "state" .= ("bad_request" :: Text)
        , "reason" .= reason
        ]

envelopeCharBudget :: Int
envelopeCharBudget = 2500

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
    ]

goalKeys :: [Text]
goalKeys = ["type", "satisfied", "note", "derivedFrom"]

envelopeStates :: [Text]
envelopeStates = ["found", "not_found", "bad_request", "duplicate"]

requiredHitKeys :: [Text]
requiredHitKeys =
    ["name", "module", "package", "version", "install", "matchKind", "origin"]

hitKeys :: [Text]
hitKeys = requiredHitKeys ++ ["type", "cabal", "use"]

schemaPromise :: Text
schemaPromise =
    "Every hit names its "
        <> T.intercalate ", " (drop 1 requiredHitKeys)
        <> "; install is one of "
        <> T.intercalate " | " (map installText [minBound .. maxBound :: InstallState])
        <> "; a hidden or absent-known package carries its -- cabal: \
           \build-depends: line."

boundEnvelope :: Value -> Value
boundEnvelope = shrinkWith [dropLastHit, shrinkCard, clampNotes]

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

maxTypeChars :: Int
maxTypeChars = 200

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

clampNotes :: Value -> Maybe Value
clampNotes (Object o)
    | any clampable ["summary", "reason"] =
        Just (Object (foldr (adjustKey clampStr) o ["summary", "reason"]))
  where
    clampable k = case KM.lookup (K.fromText k) o of
        Just (String t) -> T.length t > maxTypeChars
        _ -> False
clampNotes _ = Nothing

clampStr :: Value -> Value
clampStr (String t)
    | T.length t > maxTypeChars = String (T.take maxTypeChars t <> "…")
clampStr v = v

adjustKey :: (Value -> Value) -> Text -> KM.KeyMap Value -> KM.KeyMap Value
adjustKey f k o = case KM.lookup (K.fromText k) o of
    Just v -> KM.insert (K.fromText k) (f v) o
    Nothing -> o

overKey :: Text -> (Value -> Maybe Value) -> Value -> Maybe Value
overKey k f (Object o) = do
    v <- KM.lookup (K.fromText k) o
    v' <- f v
    pure (Object (KM.insert (K.fromText k) v' o))
overKey _ _ _ = Nothing

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
