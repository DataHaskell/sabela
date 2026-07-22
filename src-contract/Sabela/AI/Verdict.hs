{-# LANGUAGE OverloadedStrings #-}

{- | The closed verdict vocabulary every verifier surface answers from
(search-api.md section 5.3): ok \/ diagnostic \/ could-not-run \/ no-verdict
(infra). 'parseVerdict' is the decoder the R8.4 lint runs — silence decodes to
'Nothing', and a transport-swallowed answer decodes as 'VerdictInfra', so
neither can impersonate a pass.
-}
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

-- | Every way a verifier surface can answer; there is no fifth way.
data VerdictClass
    = VerdictOk
    | VerdictDiagnostic
    | VerdictCouldNotRun
    | VerdictInfra
    deriving (Bounded, Enum, Eq, Show)

-- | The whole enum, for totality properties.
verdictClasses :: [VerdictClass]
verdictClasses = [minBound .. maxBound]

verdictTag :: VerdictClass -> Text
verdictTag VerdictOk = "ok"
verdictTag VerdictDiagnostic = "diagnostic"
verdictTag VerdictCouldNotRun = "could-not-run"
verdictTag VerdictInfra = "no-verdict-infra"

-- | The closed set of tags; anything outside it fails the envelope validator.
verdictVocabulary :: [Text]
verdictVocabulary = map verdictTag verdictClasses

-- | The in-band marker a verifier-channel message carries.
verdictMarker :: VerdictClass -> Text
verdictMarker c = "[verdict: " <> verdictTag c <> "]"

{- | Decode a verifier-surface answer to its verdict. Recognised: the explicit
marker, a JSON @\"verdict\":\"tag\"@ field, and the transport failure envelope
(@[infra]@\/@[kernel]@ = 'VerdictInfra', @[payload]@ = 'VerdictCouldNotRun').
Anything else — silence included — is 'Nothing': undecodable, a lint issue.
-}
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
