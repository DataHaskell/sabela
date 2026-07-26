{-# LANGUAGE OverloadedStrings #-}

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
