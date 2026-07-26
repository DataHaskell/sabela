{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.HoleProbe (
    HoleProbeAnswer (..),
    holeProbeAnswers,
    holeProbeFacts,
    holeProbeJson,
    holeProbeProvenance,
    maxProbeProducers,
    probeAnswered,
    probedProducer,
) where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.HoleFits (HoleFit (..), parseHoleFits)

data HoleProbeAnswer = HoleProbeAnswer
    { hpaGoalType :: Text
    , hpaProducers :: [Text]
    }
    deriving (Eq, Show)

holeProbeProvenance :: Text
holeProbeProvenance = "via: hole-probe"

maxProbeProducers :: Int
maxProbeProducers = 5

foundHoleMarker :: Text
foundHoleMarker = "Found hole: _ :: "

holeProbeAnswers :: Text -> [HoleProbeAnswer]
holeProbeAnswers diagnostic = dedupOnGoal (map answerOf (holeChunks diagnostic))
  where
    answerOf chunk =
        HoleProbeAnswer
            (goalTypeOf chunk)
            (take maxProbeProducers (producersOf chunk))

holeChunks :: Text -> [Text]
holeChunks diagnostic = case T.splitOn foundHoleMarker diagnostic of
    [] -> []
    (_ : rest) -> rest

goalTypeOf :: Text -> Text
goalTypeOf = T.strip . T.takeWhile (/= '\n')

producersOf :: Text -> [Text]
producersOf chunk = nubText [hfWrite f | f <- parseHoleFits chunk, not (hfRefined f)]

dedupOnGoal :: [HoleProbeAnswer] -> [HoleProbeAnswer]
dedupOnGoal = go []
  where
    go _ [] = []
    go seen (a : as)
        | hpaGoalType a `elem` seen = go seen as
        | otherwise = a : go (hpaGoalType a : seen) as

nubText :: [Text] -> [Text]
nubText = go []
  where
    go _ [] = []
    go seen (x : xs)
        | x `elem` seen = go seen xs
        | otherwise = x : go (x : seen) xs

holeProbeFacts :: Text -> [Text]
holeProbeFacts = map factOf . holeProbeAnswers

factOf :: HoleProbeAnswer -> Text
factOf a
    | null (hpaProducers a) =
        "no producer of `"
            <> hpaGoalType a
            <> "` found in scope ("
            <> holeProbeProvenance
            <> ")"
    | otherwise =
        "`"
            <> hpaGoalType a
            <> "` is produced by: "
            <> T.intercalate ", " (map tick (hpaProducers a))
            <> " ("
            <> holeProbeProvenance
            <> ")"
  where
    tick n = "`" <> n <> "`"

holeProbeJson :: Text -> Maybe Value
holeProbeJson diagnostic = case holeProbeAnswers diagnostic of
    [] -> Nothing
    answers ->
        Just
            ( object
                [ "provenance" .= holeProbeProvenance
                , "holes" .= map answerJson answers
                , "facts" .= holeProbeFacts diagnostic
                ]
            )

probeAnswered :: [Text] -> Text -> Bool
probeAnswered facts ty = any (isProbeFactFor ty) facts

probedProducer :: [Text] -> Text -> Maybe Text
probedProducer facts ty =
    case [n | f <- facts, isProbeFactFor ty f, n <- namedProducers f] of
        (n : _) -> Just n
        [] -> Nothing

isProbeFactFor :: Text -> Text -> Bool
isProbeFactFor ty f =
    holeProbeProvenance `T.isInfixOf` f
        && any (`T.isPrefixOf` f) ["`" <> ty <> "` is produced by: ", noProducerPrefix]
  where
    noProducerPrefix = "no producer of `" <> ty <> "` found"

namedProducers :: Text -> [Text]
namedProducers f = case T.breakOn producedBy f of
    (_, rest)
        | not (T.null rest) ->
            [ n
            | chunk <- T.splitOn ", " (T.drop (T.length producedBy) rest)
            , Just n <- [backticked chunk]
            ]
    _ -> []
  where
    producedBy = "` is produced by: "
    backticked chunk = do
        body <- T.stripPrefix "`" (T.strip chunk)
        let (n, rest) = T.breakOn "`" body
        if T.null rest || T.null n then Nothing else Just n

answerJson :: HoleProbeAnswer -> Value
answerJson a =
    object
        [ "goalType" .= hpaGoalType a
        , "producers" .= hpaProducers a
        ]
