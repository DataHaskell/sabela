{-# LANGUAGE OverloadedStrings #-}

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

distillBudget :: Int
distillBudget = 2500

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

distillInfo :: Text -> Text
distillInfo = T.intercalate "\n\n" . filter (not . T.null) . map clean . segments
  where
    clean = T.take distillBudget . cleanSegment

segments :: Text -> [Text]
segments = filter (not . T.null . T.strip) . T.splitOn "\n\n"

cleanSegment :: Text -> Text
cleanSegment seg =
    T.strip . sanitizeTypeText . T.intercalate "\n" $
        [ l
        | l <- T.lines seg
        , not (leakyLine l)
        , not ("-- Defined in" `T.isInfixOf` l)
        ]

dumpShaped :: Text -> Bool
dumpShaped seg = any (\l -> leakyLine l || infoDumpLine l) (T.lines seg)

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

jsonMessage :: Text -> [Text]
jsonMessage seg = case decodeStrict (TE.encodeUtf8 (T.strip seg)) of
    Just (Object o) -> case KM.lookup "message" o of
        Just (Array ms) -> [T.unwords [m | String m <- toList ms]]
        Just (String m) -> [m]
        _ -> []
    _ -> []

answerVerdict :: Text -> Text
answerVerdict t
    | T.null (T.strip t) = verdictTag VerdictCouldNotRun
    | isClean (healthOfTypeQuery t) = verdictTag VerdictOk
    | otherwise = verdictTag VerdictDiagnostic
