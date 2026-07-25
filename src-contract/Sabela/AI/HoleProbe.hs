{-# LANGUAGE OverloadedStrings #-}

{- | G3: a typed hole is a question to the compiler, and this turns GHC's
answer into plain statements. Given a compiler diagnostic that reported one
or more typed holes, 'holeProbeAnswers' pairs each hole's goal type with the
producers GHC listed for it; 'holeProbeFacts' renders those as ledger-ready
sentences carrying the 'holeProbeProvenance' tag. Nothing here runs, compiles
or commits — it only reads a diagnostic the harness already has.
-}
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

{- | One hole the compiler reported: its goal type and the in-scope names GHC
listed as producing it. An empty 'hpaProducers' is a real answer — no
producer of that type is in scope — never a missing one.
-}
data HoleProbeAnswer = HoleProbeAnswer
    { hpaGoalType :: Text
    , hpaProducers :: [Text]
    }
    deriving (Eq, Show)

-- | The tag every hole-probe fact carries, so its origin stays readable.
holeProbeProvenance :: Text
holeProbeProvenance = "via: hole-probe"

-- | How many producers a single fact names before it stops listing.
maxProbeProducers :: Int
maxProbeProducers = 5

-- | The marker GHC writes for every typed hole it reports.
foundHoleMarker :: Text
foundHoleMarker = "Found hole: _ :: "

{- | One answer per distinct goal type in the diagnostic, in first-reported
order. A diagnostic with no typed hole yields @[]@.
-}
holeProbeAnswers :: Text -> [HoleProbeAnswer]
holeProbeAnswers diagnostic = dedupOnGoal (map answerOf (holeChunks diagnostic))
  where
    answerOf chunk =
        HoleProbeAnswer
            (goalTypeOf chunk)
            (take maxProbeProducers (producersOf chunk))

{- | The diagnostic text belonging to each reported hole: from its @Found
hole@ marker to the next one (or the end), so one hole's valid-fit block can
never be read as another's.
-}
holeChunks :: Text -> [Text]
holeChunks diagnostic = case T.splitOn foundHoleMarker diagnostic of
    [] -> []
    (_ : rest) -> rest

-- | The goal type a chunk opens with: the remainder of the @Found hole@ line.
goalTypeOf :: Text -> Text
goalTypeOf = T.strip . T.takeWhile (/= '\n')

-- | The plain (non-refinement) fit names GHC listed in this hole's block.
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

{- | The probe's conclusions as plain statements — what produces the type, or
that nothing in scope does. Never a recommendation, never code to transcribe.
-}
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

{- | The wire block a probe-answering envelope carries: the per-hole answers
and their rendered facts. 'Nothing' when the diagnostic reported no hole.
-}
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

{- | Has a probe already answered for this goal type — either way? A held
"no producer" answer is an answer, so it stops the harness re-asking.
-}
probeAnswered :: [Text] -> Text -> Bool
probeAnswered facts ty = any (isProbeFactFor ty) facts

{- | The first producer a held probe fact names for @ty@, ready to write at
that slot. 'Nothing' when nothing was found, or nothing has been probed.
-}
probedProducer :: [Text] -> Text -> Maybe Text
probedProducer facts ty =
    case [n | f <- facts, isProbeFactFor ty f, n <- namedProducers f] of
        (n : _) -> Just n
        [] -> Nothing

-- | Is @f@ a hole-probe fact about goal type @ty@?
isProbeFactFor :: Text -> Text -> Bool
isProbeFactFor ty f =
    holeProbeProvenance `T.isInfixOf` f
        && any (`T.isPrefixOf` f) ["`" <> ty <> "` is produced by: ", noProducerPrefix]
  where
    noProducerPrefix = "no producer of `" <> ty <> "` found"

-- | The back-ticked names a "produced by" fact lists, in stated order.
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
