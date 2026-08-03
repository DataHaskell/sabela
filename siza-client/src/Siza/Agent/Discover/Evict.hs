{-# LANGUAGE OverloadedStrings #-}

{- | Which envelope fields may be shed under budget pressure, and what a shed
field owes. A factual field is clamped and says so in place; an advisory field
is removed and named in @elided@.
-}
module Siza.Agent.Discover.Evict (
    adjustKey,
    clampStr,
    dropAmbiguousWith,
    dropProducerHint,
    elidableFields,
    elidedEntries,
    elidedViolations,
    evictionViolations,
    hitsOfV,
    maxTypeChars,
    overKey,
    producerMarker,
    shrinkNarrow,
    shrinkNext,
    shrinkSummary,
) where

import Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

{- | The advisory field classes an over-budget envelope may shed. Each is
computed from something the envelope still states, so a reader that needs one
back can ask for it.
-}
elidableFields :: [Text]
elidableFields = ["producer", "ambiguousWith", "narrow", "summary", "next"]

-- | The lexical marker a producer hint is appended to a hit's `use` with.
producerMarker :: Text
producerMarker = "; producer: "

maxTypeChars :: Int
maxTypeChars = 200

noteSep :: Text
noteSep = "; "

dropProducerHint :: Value -> Maybe Value
dropProducerHint v = case mapHits stripProducer v of
    (v', n) | n > 0 -> Just (disclose "producer" n v')
    _ -> Nothing

stripProducer :: Value -> Value
stripProducer (Object h) = Object (adjustKey cut "use" h)
  where
    cut (String s)
        | producerMarker `T.isInfixOf` s = String (fst (T.breakOn producerMarker s))
    cut x = x
stripProducer x = x

dropAmbiguousWith :: Value -> Maybe Value
dropAmbiguousWith v = case mapHits (delKey "ambiguousWith") v of
    (v', n) | n > 0 -> Just (disclose "ambiguousWith" n v')
    _ -> Nothing

{- | Shed one trailing note from a multi-note disclosure. The last note is
kept: an envelope that discloses nothing about what it filtered is not cheaper
to act on, only quieter.
-}
shrinkNarrow :: Value -> Maybe Value
shrinkNarrow = shedNote "narrow"

shrinkSummary :: Value -> Maybe Value
shrinkSummary = shedNote "summary"

{- | The next step is advice, derived from what the envelope still states, so
its trailing notes go before any field carrying a result. The first note is
kept: it is the step, and the rest qualifies it.
-}
shrinkNext :: Value -> Maybe Value
shrinkNext = shedNote "next"

shedNote :: Text -> Value -> Maybe Value
shedNote k (Object o) = case KM.lookup (K.fromText k) o of
    Just (String s)
        | notes <- T.splitOn noteSep s
        , length notes > 1 ->
            Just
                . disclose k 1
                . Object
                . KM.insert
                    (K.fromText k)
                    (String (T.intercalate noteSep (init notes)))
                $ o
    _ -> Nothing
shedNote _ _ = Nothing

{- | Record that a field class was shed, adding to any count already there so
two passes of the same step disclose their total rather than the last one.
-}
disclose :: Text -> Int -> Value -> Value
disclose k n (Object o) =
    Object
        (KM.insert "elided" (toJSON (map entry (bump (elidedEntries (Object o))))) o)
  where
    entry (f, c) = object ["field" .= f, "count" .= c]
    bump es = case break ((== k) . fst) es of
        (pre, (_, c) : post) -> pre ++ [(k, c + n)] ++ post
        _ -> es ++ [(k, n)]
disclose _ _ v = v

elidedEntries :: Value -> [(Text, Int)]
elidedEntries (Object o) = case KM.lookup "elided" o of
    Just (Array es) -> [(f, c) | e <- toList es, Just (f, c) <- [entryOf e]]
    _ -> []
  where
    entryOf (Object e) = case (KM.lookup "field" e, KM.lookup "count" e) of
        (Just (String f), Just (Number c)) -> Just (f, round c)
        _ -> Nothing
    entryOf _ = Nothing
elidedEntries _ = []

{- | The eviction law: a class that shrank between the bounder's input and its
output is named in @elided@ with exactly the count that went, and no class is
named that did not shrink. Stated over the pair, which is pure and checkable.
-}
evictionViolations :: Value -> Value -> [Text]
evictionViolations before after =
    concatMap check elidableFields ++ cardViolations before after
  where
    check f
        | removed == declared = []
        | declared == 0 = ["evicted without disclosure: " <> f]
        | otherwise =
            [ "elided "
                <> f
                <> " discloses "
                <> tShow declared
                <> " but "
                <> tShow removed
                <> " went"
            ]
      where
        removed = classCount f before - classCount f after
        declared = at f after - at f before
    at f v = fromMaybe 0 (lookup f (elidedEntries v))

{- | The clamp half of the law: a card is narrowed in place, and the rows it
still lists plus the rows it says it dropped must be the rows it had, or a
reader takes a short card for a small module.
-}
cardViolations :: Value -> Value -> [Text]
cardViolations before after
    | had == kept = []
    | otherwise =
        [ "card narrowed from "
            <> tShow had
            <> " rows but accounts for "
            <> tShow kept
        ]
  where
    had = cardRows before
    kept = cardRows after

cardRows :: Value -> Int
cardRows v = case v of
    Object o -> case KM.lookup "card" o of
        Just (Object c) -> listed c + counted c
        _ -> 0
    _ -> 0
  where
    listed c = case KM.lookup "exports" c of
        Just (Array es) -> length (toList es)
        _ -> 0
    counted c = case KM.lookup "moreExports" c of
        Just (Number x) -> round x
        _ -> 0

-- | How much of a field class an envelope carries, in shed-able units.
classCount :: Text -> Value -> Int
classCount "producer" v =
    length [() | h <- hitsOfV v, producerMarker `T.isInfixOf` hitStr "use" h]
classCount "ambiguousWith" v =
    length [() | h <- hitsOfV v, not (T.null (hitStr "ambiguousWith" h))]
classCount k v = case v of
    Object o -> case KM.lookup (K.fromText k) o of
        Just (String s) -> length (T.splitOn noteSep s)
        _ -> 0
    _ -> 0

{- | What an @elided@ block must look like on its own: a named class, a
positive count, and nothing left behind of a class reported gone.
-}
elidedViolations :: Value -> [Text]
elidedViolations v@(Object o) = case KM.lookup "elided" o of
    Nothing -> []
    Just (Array es) -> concatMap entryViol (toList es) ++ residue
    Just _ -> ["elided is not an array"]
  where
    entryViol (Object e) =
        [ "unknown elided field: " <> K.toText k
        | k <- KM.keys e
        , K.toText k `notElem` ["field", "count"]
        ]
            ++ [ "elided names an unshed-able field: " <> f
               | Just (String f) <- [KM.lookup "field" e]
               , f `notElem` elidableFields
               ]
            ++ ["elided entry missing field or count" | not (wellFormed e)]
            ++ [ "elided count is not positive"
               | Just (Number c) <- [KM.lookup "count" e]
               , c <= 0
               ]
    entryViol _ = ["elided entry is not an object"]
    wellFormed e = case (KM.lookup "field" e, KM.lookup "count" e) of
        (Just (String f), Just (Number _)) -> not (T.null f)
        _ -> False
    residue =
        [ "elided " <> f <> " but the envelope still carries it"
        | (f, _) <- elidedEntries v
        , f `elem` ["producer", "ambiguousWith"]
        , classCount f v > 0
        ]
elidedViolations _ = []

mapHits :: (Value -> Value) -> Value -> (Value, Int)
mapHits f v@(Object o) = case KM.lookup "hits" o of
    Just (Array hs) ->
        let hs' = map f (toList hs)
            n = length [() | (a, b) <- zip (toList hs) hs', a /= b]
         in (Object (KM.insert "hits" (toJSON hs') o), n)
    _ -> (v, 0)
mapHits _ v = (v, 0)

hitsOfV :: Value -> [Value]
hitsOfV (Object o) = case KM.lookup "hits" o of
    Just (Array a) -> toList a
    _ -> []
hitsOfV _ = []

hitStr :: Text -> Value -> Text
hitStr k (Object h) = case KM.lookup (K.fromText k) h of
    Just (String s) -> s
    _ -> ""
hitStr _ _ = ""

delKey :: Text -> Value -> Value
delKey k (Object h) = Object (KM.delete (K.fromText k) h)
delKey _ v = v

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

tShow :: Int -> Text
tShow = T.pack . show
