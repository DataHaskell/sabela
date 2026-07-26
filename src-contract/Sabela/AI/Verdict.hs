{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Verdict (
    VerdictClass (..),
    verdictClasses,
    verdictTag,
    verdictVocabulary,
    verdictMarker,
    parseVerdict,
) where

import Data.Text (Text)
import qualified Data.Text as T

data VerdictClass
    = VerdictOk
    | VerdictDiagnostic
    | VerdictCouldNotRun
    | VerdictInfra
    deriving (Bounded, Enum, Eq, Show)

verdictClasses :: [VerdictClass]
verdictClasses = [minBound .. maxBound]

verdictTag :: VerdictClass -> Text
verdictTag VerdictOk = "ok"
verdictTag VerdictDiagnostic = "diagnostic"
verdictTag VerdictCouldNotRun = "could-not-run"
verdictTag VerdictInfra = "no-verdict-infra"

verdictVocabulary :: [Text]
verdictVocabulary = map verdictTag verdictClasses

verdictMarker :: VerdictClass -> Text
verdictMarker c = "[verdict: " <> verdictTag c <> "]"

parseVerdict :: Text -> Maybe VerdictClass
parseVerdict t =
    lookupBy marked
        `orElse` lookupBy jsonField
        `orElse` transportClass
  where
    lookupBy f = case [c | c <- verdictClasses, f c] of
        (c : _) -> Just c
        [] -> Nothing
    marked c = verdictMarker c `T.isInfixOf` t
    jsonField c =
        ("\"verdict\":\"" <> verdictTag c <> "\"") `T.isInfixOf` noSpace
    noSpace = T.filter (/= ' ') t
    transportClass
        | anyPrefix ["[infra]", "[kernel]"] = Just VerdictInfra
        | anyPrefix ["[payload]"] = Just VerdictCouldNotRun
        | otherwise = Nothing
    anyPrefix = any (`T.isPrefixOf` T.stripStart t)
    orElse (Just x) _ = Just x
    orElse Nothing y = y
