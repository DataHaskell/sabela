{-# LANGUAGE OverloadedStrings #-}

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

probeCode :: Value -> Maybe Text
probeCode args = T.stripPrefix "_ :: " (T.strip (argText "code" args))

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

probeAnswer :: Text -> [Text] -> Value
probeAnswer ty producers =
    object
        [ "route" .= ("typecheck_only" :: Text)
        , "verdict" .= ("ok" :: Text)
        , "outcome" .= ("hole_fits" :: Text)
        , "evaluated" .= False
        , "answer" .= [probeFactFor ty producers]
        ]

tryAccepts :: Value
tryAccepts =
    object
        [ "route" .= ("disposable_scratch" :: Text)
        , "verdict" .= ("ok" :: Text)
        , "outcome" .= ("ok" :: Text)
        ]

scriptedTryOutcome :: [(Text, [Text])] -> Value -> Value
scriptedTryOutcome table args = case probeCode args of
    Just ty -> probeAnswer ty (concat [ps | (t, ps) <- table, t == ty])
    Nothing -> tryAccepts

argText :: Text -> Value -> Text
argText k (Object o) = case KM.lookup (K.fromText k) o of
    Just (String s) -> s
    _ -> ""
argText _ _ = ""
