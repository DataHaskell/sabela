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
import Data.Char (isAlphaNum, isLower)
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
producersOf chunk =
    nubText
        [ hfWrite f
        | f <- parseHoleFits chunk
        , not (hfRefined f)
        , not (isBottom (hfType f))
        ]

{- | Whether a fit inhabits every type rather than producing this one. GHC
offers @undefined@ and the arithmetic bottoms for any goal, and listing them as
producers reports an uninhabited type as a solved one.
-}
isBottom :: Text -> Bool
isBottom ty = not (constrained ty) && typeVarish (resultOf ty)
  where
    constrained = T.isInfixOf "=>"
    typeVarish t = case T.uncons (T.strip t) of
        Just (c, rest) -> isLower c && T.all (\x -> isAlphaNum x || x == '\'') rest
        Nothing -> False

{- | The type a fit yields once every argument is supplied. Splitting on the
top-level arrow is enough here: a fit whose result is parenthesised or higher
rank is not a bottom, and reads as a producer either way.
-}
resultOf :: Text -> Text
resultOf ty = case reverse (T.splitOn "->" (stripForall ty)) of
    (r : _) -> T.strip r
    [] -> T.strip ty
  where
    stripForall t = case T.breakOn "." (T.strip t) of
        (before, rest)
            | "forall" `T.isPrefixOf` T.strip before
            , not (T.null rest) ->
                T.drop 1 rest
        _ -> t

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
