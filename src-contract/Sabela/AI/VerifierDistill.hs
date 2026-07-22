{-# LANGUAGE OverloadedStrings #-}

{- | The verifier-surface boundary distiller (5.2, R3.6\/R3.9\/R3.10): keep
the writable signature line(s), drop leak-shaped trailing segments, distil a
pure diagnostic to ONE bounded @error:@ line. Keyed on leak shape only.
-}
module Sabela.AI.VerifierDistill (
    answerVerdict,
    distillBudget,
    distillInfo,
    distillTypeAnswer,
) where

import Data.Aeson (Value (..), decodeStrict)
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Sabela.AI.Grammar.Synth (sanitizeTypeText)
import Sabela.AI.Health (healthOfTypeQuery, isClean)
import Sabela.AI.LeakShape (infoDumpLine, leakyLine)
import Sabela.AI.Verdict (VerdictClass (..), verdictTag)

-- | The declared budget of any distilled verifier answer (R3.9).
distillBudget :: Int
distillBudget = 2500

{- | Distil a type-query answer: the head segment keeps its clean lines, a
trailing segment survives only if wholly clean of leak\/dump shapes, and an
answer with nothing clean left becomes one distilled diagnostic line.
-}
distillTypeAnswer :: Text -> Text
distillTypeAnswer t
    | T.null (T.strip kept) = diagnosticLine t
    | otherwise = T.take distillBudget kept
  where
    kept = T.intercalate "\n\n" (headSeg ++ tailSegs)
    segs = segments t
    headSeg = take 1 (filter (not . T.null) (map cleanSegment (take 1 segs)))
    tailSegs =
        [ cleaned
        | seg <- drop 1 segs
        , not (dumpShaped seg)
        , let cleaned = cleanSegment seg
        , not (T.null cleaned)
        ]

{- | Scrub the @:info@ channel: leak-shaped and provenance lines never reach
'Sabela.AI.Capabilities.Query.recordDecl' or any composed answer.
-}
distillInfo :: Text -> Text
distillInfo = T.intercalate "\n\n" . filter (not . T.null) . map clean . segments
  where
    clean = T.take distillBudget . cleanSegment

-- | Blank-line-separated segments, blank-free.
segments :: Text -> [Text]
segments = filter (not . T.null . T.strip) . T.splitOn "\n\n"

-- | Keep a segment's writable lines: no leak shapes, no provenance comments.
cleanSegment :: Text -> Text
cleanSegment seg =
    T.strip . sanitizeTypeText . T.intercalate "\n" $
        [ l
        | l <- T.lines seg
        , not (leakyLine l)
        , not ("-- Defined in" `T.isInfixOf` l)
        ]

-- | A trailing segment that is a dump, not an answer: any leaky or info line.
dumpShaped :: Text -> Bool
dumpShaped seg = any (\l -> leakyLine l || infoDumpLine l) (T.lines seg)

{- | ONE bounded diagnostic line: a GHC diagnostics-as-JSON blob yields its
message; anything else yields its first clean line. Always @error:@-tagged so
the verdict stays in the closed vocabulary.
-}
diagnosticLine :: Text -> Text
diagnosticLine t = "error: " <> T.take 200 body
  where
    body = case concatMap jsonMessage (segments t) of
        (m : _) -> m
        [] -> firstCleanLine
    firstCleanLine =
        case [sanitizeTypeText l | l <- T.lines t, not (leakyLine l)] of
            (l : _) -> stripErr (T.strip l)
            [] -> "unreadable diagnostic (leak-shaped output suppressed)"
    stripErr l = maybe l T.stripStart (T.stripPrefix "error:" l)

-- | The @message@ strings of a GHC diagnostics-as-JSON segment, if it is one.
jsonMessage :: Text -> [Text]
jsonMessage seg = case decodeStrict (TE.encodeUtf8 (T.strip seg)) of
    Just (Object o) -> case KM.lookup "message" o of
        Just (Array ms) -> [T.unwords [m | String m <- toList ms]]
        Just (String m) -> [m]
        _ -> []
    _ -> []

{- | The closed-vocabulary verdict of a distilled answer (section 5.3):
silence can never impersonate a pass.
-}
answerVerdict :: Text -> Text
answerVerdict t
    | T.null (T.strip t) = verdictTag VerdictCouldNotRun
    | isClean (healthOfTypeQuery t) = verdictTag VerdictOk
    | otherwise = verdictTag VerdictDiagnostic
