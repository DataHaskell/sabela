{-# LANGUAGE OverloadedStrings #-}

{- | The scripted server side of G3's typecheck-only @try@ route, shared by
every spec that drives the harness hole probe: a @_ :: T@ candidate is
answered with the conclusions 'Sabela.AI.HoleProbe.holeProbeFacts' renders,
any other candidate is accepted whole. No spec re-invents this shape.
-}
module Test.ProbeFixtures (
    probeCode,
    probeFactFor,
    scriptedTryOutcome,
    tryAccepts,
) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.HoleProbe (holeProbeProvenance)

-- | The goal type a probe candidate asks about, when it is one.
probeCode :: Value -> Maybe Text
probeCode args = T.stripPrefix "_ :: " (T.strip (argText "code" args))

-- | One rendered probe conclusion; an empty producer list is a real answer.
probeFactFor :: Text -> [Text] -> Text
probeFactFor ty producers
    | null producers =
        "no producer of `" <> ty <> "` found in scope (" <> holeProbeProvenance <> ")"
    | otherwise =
        "`"
            <> ty
            <> "` is produced by: "
            <> T.intercalate ", " ["`" <> p <> "`" | p <- producers]
            <> " ("
            <> holeProbeProvenance
            <> ")"

-- | The typecheck-only answer envelope for a hole of @ty@.
probeAnswer :: Text -> [Text] -> Value
probeAnswer ty producers =
    object
        [ "route" .= ("typecheck_only" :: Text)
        , "verdict" .= ("ok" :: Text)
        , "outcome" .= ("hole_fits" :: Text)
        , "evaluated" .= False
        , "answer" .= [probeFactFor ty producers]
        ]

-- | @try@ accepted the candidate whole, so the harness may surface it.
tryAccepts :: Value
tryAccepts =
    object
        [ "route" .= ("disposable_scratch" :: Text)
        , "verdict" .= ("ok" :: Text)
        , "outcome" .= ("ok" :: Text)
        ]

{- | The scripted @try@ backend over a producer table: a probe is answered
from the table (absent means no producer), anything else is accepted.
-}
scriptedTryOutcome :: [(Text, [Text])] -> Value -> Value
scriptedTryOutcome table args = case probeCode args of
    Just ty -> probeAnswer ty (concat [ps | (t, ps) <- table, t == ty])
    Nothing -> tryAccepts

argText :: Text -> Value -> Text
argText k (Object o) = case KM.lookup (K.fromText k) o of
    Just (String s) -> s
    _ -> ""
argText _ _ = ""
