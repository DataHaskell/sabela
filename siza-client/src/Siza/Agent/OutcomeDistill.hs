{-# LANGUAGE OverloadedStrings #-}

module Siza.Agent.OutcomeDistill (
    distillOutcome,
    outcomeCharBudget,
    outcomeHeadBudget,
    stripEscapes,
) where

import Data.Aeson (Value (..), decode, encode, toJSON)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

outcomeHeadBudget :: Int
outcomeHeadBudget = 1200

outcomeCharBudget :: Int
outcomeCharBudget = 2500

distillOutcome :: Value -> Value
distillOutcome v
    | not (containsOutputArray v) = v
    | otherwise = hardBound (shedOutputs (distillNode v))

containsOutputArray :: Value -> Bool
containsOutputArray (Object o) =
    outputHere || any containsOutputArray (KM.elems o)
  where
    outputHere = case KM.lookup "outputs" o of
        Just (Array _) -> True
        _ -> False
containsOutputArray (Array a) = any containsOutputArray a
containsOutputArray _ = False

distillNode :: Value -> Value
distillNode (Object o) =
    Object (boundOutputs (KM.mapWithKey descend o))
  where
    descend k v
        | k == "outputs" = v
        | otherwise = distillNode v
distillNode (Array a) = Array (fmap distillNode a)
distillNode v = v

boundOutputs :: KM.KeyMap Value -> KM.KeyMap Value
boundOutputs o = case KM.lookup "outputs" o of
    Just (Array items)
        | not (null items)
        , all outputShaped items ->
            KM.insert "outputCount" (intVal (length items)) $
                KM.insert "outputs" (Array (fmap distillItem items)) o
    _ -> o

outputShaped :: Value -> Bool
outputShaped (Object h) = KM.member "oiOutput" h
outputShaped _ = False

{- | A bounded view of one output, chosen from the output's own mime tag: a
JSON body discloses its top-level keys, anything else its line count. The
head stays a verbatim prefix under every view, so nothing is rewritten.
-}
distillItem :: Value -> Value
distillItem (Object h) =
    Object
        . KM.union (KM.fromList (mimeView mime clean))
        . KM.insert "chars" (intVal (T.length raw))
        . KM.insert "oiOutput" (String (T.take outcomeHeadBudget clean))
        $ h
  where
    raw = case KM.lookup "oiOutput" h of
        Just (String s) -> s
        _ -> ""
    clean = stripEscapes raw
    mime = case KM.lookup "oiMime" h of
        Just (String s) -> s
        _ -> ""
distillItem v = v

mimeView :: Text -> Text -> [(K.Key, Value)]
mimeView mime body
    | mime == "application/json" = [("jsonKeys", toJSON (topLevelKeys body))]
    | otherwise = [("lines", intVal (length (T.lines body)))]

{- | The keys a JSON body really has, read back from the body rather than
asserted, and empty when it does not parse.
-}
topLevelKeys :: Text -> [Text]
topLevelKeys body = case decode (LBS.fromStrict (TE.encodeUtf8 body)) of
    Just (Object o) -> map K.toText (KM.keys o)
    _ -> []

shedOutputs :: Value -> Value
shedOutputs v
    | serialisedChars v <= outcomeCharBudget = v
    | otherwise = maybe v shedOutputs (dropOneOutput v)

hardBound :: Value -> Value
hardBound v
    | serialisedChars v <= outcomeCharBudget = v
    | serialisedChars compact <= outcomeCharBudget = compact
    | otherwise = frameOnly compact
  where
    compact = shedOutputs (capStrings 280 v)

capStrings :: Int -> Value -> Value
capStrings n (String s) = String (T.take n (stripEscapes s))
capStrings n (Object o) = Object (fmap (capStrings n) o)
capStrings n (Array a) = Array (fmap (capStrings n) a)
capStrings _ v = v

{- | The last resort keeps the frame AND everything that says what happened,
fitting them by narrowing one ladder of bounds computed from the original
value, so a shortened list still states how many entries it is short of.
-}
frameOnly :: Value -> Value
frameOnly v@(Object _) =
    case [f | b <- frameLadder, let f = frameWith b v, fits f] of
        (f : _) -> f
        [] -> frameWith (last frameLadder) v
  where
    fits f = serialisedChars f <= outcomeCharBudget
frameOnly _ = Object (KM.singleton "distilled" (Bool True))

{- | Keeps the stable keys at whatever depth the envelope puts them: a write
ack nests its whole summary, values included, one level down.
-}
frameWith :: (Int, Int) -> Value -> Value
frameWith b@(arrCap, strCap) (Object o) =
    Object . KM.insert "distilled" (Bool True) . KM.fromList $
        nested <> kept
  where
    nested =
        [("execution", frameWith b inner) | Just inner <- [KM.lookup "execution" o]]
    kept =
        [ (K.fromText k, capStrings strCap (capArray arrCap v))
        | k <- stableKeys
        , Just v <- [KM.lookup (K.fromText k) o]
        ]
frameWith _ v = v

-- | What a result must still say about itself after the hardest bound.
stableKeys :: [Text]
stableKeys =
    [ "outcome"
    , "ok"
    , "cellId"
    , "outputCount"
    , "omittedOutputs"
    , "values"
    , "artefacts"
    , "guidance"
    , "warnings"
    , "error"
    , "diagnostics"
    , "notCommitted"
    ]

-- | Array cap and string cap, narrowing until the frame fits.
frameLadder :: [(Int, Int)]
frameLadder = [(12, 280), (8, 200), (4, 140), (2, 90), (1, 60), (0, 40)]

{- | Shortens a list to @n@ entries and says how many it left out, so a
shortened list is never mistaken for a complete one.
-}
capArray :: Int -> Value -> Value
capArray n (Array a)
    | len > n = toJSON (take n xs <> [String ("…" <> tShow (len - n) <> " more")])
  where
    xs = toList a
    len = length xs
capArray _ v = v

tShow :: Int -> Text
tShow = T.pack . show

serialisedChars :: Value -> Int
serialisedChars = T.length . TE.decodeUtf8 . LBS.toStrict . encode

dropOneOutput :: Value -> Maybe Value
dropOneOutput (Object o)
    | Just (Array items) <- KM.lookup "outputs" o
    , not (null items) =
        Just
            ( Object
                (bumpOmitted (KM.insert "outputs" (toJSON (init (toList items))) o))
            )
    | Just ex <- KM.lookup "execution" o
    , Just ex' <- dropOneOutput ex =
        Just (Object (KM.insert "execution" ex' o))
dropOneOutput _ = Nothing

bumpOmitted :: KM.KeyMap Value -> KM.KeyMap Value
bumpOmitted = KM.insertWith addNum "omittedOutputs" (intVal 1)
  where
    addNum _ (Number n) = Number (n + 1)
    addNum new _ = new

intVal :: Int -> Value
intVal = Number . fromIntegral

stripEscapes :: Text -> Text
stripEscapes = stripCtrl . stripAnsi

stripAnsi :: Text -> Text
stripAnsi t = case T.break (== '\ESC') t of
    (pre, rest)
        | T.null rest -> pre
        | otherwise -> pre <> stripAnsi (dropSeq (T.drop 1 rest))
  where
    dropSeq s = case T.uncons s of
        Just ('[', s') -> T.drop 1 (T.dropWhile (not . isFinal) s')
        _ -> s
    isFinal c = c >= '@' && c <= '~'

stripCtrl :: Text -> Text
stripCtrl = T.filter (\c -> c >= ' ' || c `elem` ("\n\t" :: String))
