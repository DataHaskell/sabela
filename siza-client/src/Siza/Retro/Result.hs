{-# LANGUAGE OverloadedStrings #-}

{- | What a rendered tool result says about itself.
Every reading here is taken from the bytes the model actually saw, so an
elided result yields only what its stub still shows.
-}
module Siza.Retro.Result (
    Elision (..),
    WriteOutcome (..),
    elisionOf,
    isDoneSignal,
    isDuplicate,
    jsonInt,
    jsonString,
    normaliseSource,
    visibleText,
    writeOutcome,
) where

import Data.Char (isDigit, isSpace)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Verdict (VerdictClass (VerdictOk), verdictMarker)

-- | A result the harness replaced with a stub, and the size it stood for.
data Elision = Elision
    { elTool :: Text
    , elFullChars :: Int
    , elPreview :: Text
    }
    deriving (Eq, Show)

{- | Recognises the stub 'Siza.Agent.Compact.resultStubFor' writes:
@[result #N from TOOL (M chars, elided): PREVIEW… — read it with …]@.
-}
elisionOf :: Text -> Maybe Elision
elisionOf t = do
    afterHash <- T.stripPrefix "[result #" (T.strip t)
    afterFrom <- stripThrough " from " afterHash
    let (tool, afterTool) = T.breakOn " (" afterFrom
    body <- stripThrough " chars, elided): " afterTool
    n <- readInt (T.takeWhile isDigit (T.drop 2 afterTool))
    pure (Elision tool n (fst (T.breakOn "… — read it with " body)))
  where
    stripThrough needle s = case T.breakOn needle s of
        (_, r) | T.null r -> Nothing
        (_, r) -> Just (T.drop (T.length needle) r)

-- | The bytes a result still shows: its stub preview, or the result itself.
visibleText :: Text -> Text
visibleText t = maybe t elPreview (elisionOf t)

data WriteOutcome
    = Committed (Maybe Int)
    | Rejected
    | Unresolved
    deriving (Eq, Show)

{- | Classifies a write's result. Only a refusal the harness names, or an ack
carrying a cell id, is read as such; anything else stays unresolved.
-}
writeOutcome :: Text -> WriteOutcome
writeOutcome raw
    | any (`T.isPrefixOf` t) ["CODE ISSUE", "NOT RUN"] = Rejected
    | hasKey "notCommitted" = Rejected
    | "TOOL ERROR" `T.isPrefixOf` t = Unresolved
    | Just cid <- jsonInt "cellId" t = Committed (Just cid)
    | otherwise = Unresolved
  where
    t = T.stripStart (visibleText raw)
    hasKey k = ("\"" <> k <> "\"") `T.isInfixOf` t

{- | A result that reports a fact the harness already held, rather than one it
went and got.
-}
isDuplicate :: Text -> Bool
isDuplicate raw = any (`T.isInfixOf` visibleText raw) heldFactMarkers

{- | How a result says it is a fact already held: discover's dedup state, a
write ack's duplicate flag, and the back-reference 'Siza.Agent.EmitLedger'
writes over a block it has already emitted.
-}
heldFactMarkers :: [Text]
heldFactMarkers =
    [ "\"state\":\"duplicate\""
    , "\"duplicate\":true"
    , "as established turn"
    ]

-- | A harness message asserting the task is done.
isDoneSignal :: Text -> Bool
isDoneSignal = T.isPrefixOf (verdictMarker VerdictOk) . T.stripStart

{- | Reads a JSON string field out of text that may have been cut short.
An unterminated value yields Nothing, so a truncated payload is never
mistaken for a short one.
-}
jsonString :: Text -> Text -> Maybe Text
jsonString key t = case T.breakOn needle t of
    (_, r) | T.null r -> Nothing
    (_, r) -> scan [] (T.drop (T.length needle) r)
  where
    needle = "\"" <> key <> "\":\""
    scan acc s = case T.uncons s of
        Nothing -> Nothing
        Just ('"', _) -> Just (T.pack (reverse acc))
        Just ('\\', s') -> case T.uncons s' of
            Nothing -> Nothing
            Just (c, s'') -> scan (c : '\\' : acc) s''
        Just (c, s') -> scan (c : acc) s'

-- | Reads a JSON integer field, whether or not the payload is complete.
jsonInt :: Text -> Text -> Maybe Int
jsonInt key t = case T.breakOn needle t of
    (_, r) | T.null r -> Nothing
    (_, r) -> readInt (T.takeWhile isDigit (T.drop (T.length needle) r))
  where
    needle = "\"" <> key <> "\":"

{- | Compares two submissions up to whitespace, and up to whitespace only.
The source arrives JSON-escaped, so the escapes are read back before the
whitespace is squashed.
-}
normaliseSource :: Text -> Text
normaliseSource = T.unwords . T.words . unescape
  where
    unescape = T.pack . go . T.unpack
    go [] = []
    go ('\\' : c : cs) = escaped c : go cs
    go (c : cs) = c : go cs
    escaped 'n' = '\n'
    escaped 't' = '\t'
    escaped 'r' = '\r'
    escaped c = if isSpace c then ' ' else c

readInt :: Text -> Maybe Int
readInt t = case reads (T.unpack t) of
    [(n, "")] -> Just n
    _ -> Nothing
