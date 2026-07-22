{-# LANGUAGE OverloadedStrings #-}

{- | Model-context distillation of a cell-execution outcome (R10-T5): before an
outcome enters the model's context at @renderOutcome@, every rendered output is
replaced by a bounded, escape-stripped head plus its true char count, and the
carrying object gains an @outputCount@. The multi-thousand-char ANSI wall a
chart cell prints (barChart turn-27) never crosses the surface. Keyed on the
output SHAPE (an @oiOutput@-carrying item), never on the cell's content or
library; the human-visible notebook render is a separate server path (the
'Sabela.Model.OutputItem' wire encoding) and is untouched by this transform.
-}
module Siza.Agent.OutcomeDistill (
    distillOutcome,
    outcomeCharBudget,
    outcomeHeadBudget,
    stripEscapes,
) where

import Data.Aeson (Value (..), encode, toJSON)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- | Per-output escape-stripped head cap; a legit small output survives whole.
outcomeHeadBudget :: Int
outcomeHeadBudget = 1200

-- | The distilled outcome's hard serialised cap; excess outputs are shed.
outcomeCharBudget :: Int
outcomeCharBudget = 2500

{- | Distil a tool outcome for the model's context: bound every @outputs@ array
in place, then shed trailing outputs while the whole envelope exceeds the
budget. A value with no output-shaped array (a discover envelope, a check_type
reply) passes through unchanged.
-}
distillOutcome :: Value -> Value
distillOutcome v
    | not (containsOutputArray v) = v
    | otherwise = hardBound (shedOutputs (distillNode v))

-- | Whether this is an execution-shaped value that owns an @outputs@ array.
-- Non-execution tool envelopes remain byte-identical.
containsOutputArray :: Value -> Bool
containsOutputArray (Object o) =
    outputHere || any containsOutputArray (KM.elems o)
  where
    outputHere = case KM.lookup "outputs" o of
        Just (Array _) -> True
        _ -> False
containsOutputArray (Array a) = any containsOutputArray a
containsOutputArray _ = False

-- | Rewrite every @outputs@ array (top-level or nested) to bounded previews.
distillNode :: Value -> Value
distillNode (Object o) =
    Object (boundOutputs (KM.mapWithKey descend o))
  where
    descend k v
        | k == "outputs" = v
        | otherwise = distillNode v
distillNode (Array a) = Array (fmap distillNode a)
distillNode v = v

{- | If this object carries an output-shaped @outputs@ array, replace each item
with its bounded preview and record the original @outputCount@.
-}
boundOutputs :: KM.KeyMap Value -> KM.KeyMap Value
boundOutputs o = case KM.lookup "outputs" o of
    Just (Array items)
        | not (null items)
        , all outputShaped items ->
            KM.insert "outputCount" (intVal (length items)) $
                KM.insert "outputs" (Array (fmap distillItem items)) o
    _ -> o

-- | An 'Sabela.Model.OutputItem'-shaped value: an object carrying @oiOutput@.
outputShaped :: Value -> Bool
outputShaped (Object h) = KM.member "oiOutput" h
outputShaped _ = False

{- | One bounded output preview: keep @oiMime@, replace @oiOutput@ with its
escape-stripped, capped head, and record the true @chars@. One declared shape.
-}
distillItem :: Value -> Value
distillItem (Object h) =
    Object
        . KM.insert "chars" (intVal (T.length raw))
        . KM.insert "oiOutput" (String (T.take outcomeHeadBudget (stripEscapes raw)))
        $ h
  where
    raw = case KM.lookup "oiOutput" h of
        Just (String s) -> s
        _ -> ""
distillItem v = v

{- | Shed trailing outputs (bumping @omittedOutputs@, counts still reconcile)
until the distilled envelope fits the budget. Only output previews are shed —
diagnostics in @outcome@/@warnings@ are load-bearing and never dropped.
-}
shedOutputs :: Value -> Value
shedOutputs v
    | serialisedChars v <= outcomeCharBudget = v
    | otherwise = maybe v shedOutputs (dropOneOutput v)

-- | Outputs are normally enough to shed. If another execution field (for
-- example a warning wall) still breaches the contract, cap all textual detail;
-- if a pathological object remains too large, retain only the stable execution
-- frame. Thus the declared budget is a real postcondition, not a best effort.
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

frameOnly :: Value -> Value
frameOnly (Object o) =
    Object $
        KM.insert "distilled" (Bool True) $
            KM.fromList
                [ (K.fromText k, capStrings 280 v)
                | k <- stableKeys
                , Just v <- [KM.lookup (K.fromText k) o]
                ]
  where
    stableKeys =
        ["outcome", "ok", "cellId", "outputCount", "omittedOutputs"]
frameOnly _ = Object (KM.singleton "distilled" (Bool True))

serialisedChars :: Value -> Int
serialisedChars = T.length . TE.decodeUtf8 . LBS.toStrict . encode

{- | Drop the last item of the first @outputs@ array (top-level, then nested
@execution@), recording the drop in @omittedOutputs@.
-}
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

{- | Strip ANSI CSI escape sequences and any remaining control bytes (keeping
newline and tab), so no raw terminal escape or internal byte reaches the model
(R3.10). Keyed on byte shape, never on content.
-}
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
