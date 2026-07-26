{-# LANGUAGE OverloadedStrings #-}

module Siza.Agent.Discover.Candidate (
    candidateCell,
    candidateCellFrom,
    candidateClause,
    candidateClauseAgainst,
    candidateClauseFrom,
    candidateGaps,
    candidateNames,
    writableDraft,
) where

import Data.List (minimumBy, nub)
import Data.Maybe (isNothing, listToMaybe, mapMaybe, maybeToList)
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.HoleProbe (probedProducer)
import Sabela.AI.TypedHole (containsTypedHole)
import Siza.Agent.Discover.Facts (installFactKey)
import Siza.Agent.Discover.Goal (argTypesOf, genuineGaps, literalFill)
import Siza.Agent.Discover.Ledger (normaliseSource)

consumerOf :: Text -> Maybe (Text, Text, Text, Text)
consumerOf f = do
    body <- T.stripPrefix "`" f
    let (name, rest) = T.breakOn "` :: " body
    sigProv <- T.stripPrefix "` :: " rest
    let (sig0, prov) = T.breakOn " — found in " sigProv
    provBody <- T.stripPrefix " — found in " prov
    let (m, pkgPart) = T.breakOn " (" provBody
        pkg = T.takeWhile (/= ')') (T.drop 2 pkgPart)
    if T.null name || T.null (T.strip sig0)
        then Nothing
        else Just (name, T.strip sig0, T.strip m, T.strip pkg)

seedConsumer :: [Text] -> Maybe (Text, Text, Text, Text)
seedConsumer facts = case mapMaybe consumerOf facts of
    [] -> Nothing
    cs -> Just (minimumBy (comparing gapCount) cs)
  where
    heldSigs = [sig | (_, sig, _, _) <- mapMaybe consumerOf facts]
    gapCount (_, sig, _, _) = length (genuineGaps heldSigs sig)

cabalFor :: Text -> [Text] -> Maybe Text
cabalFor pkg facts =
    listToMaybe
        [ T.strip (T.takeWhile (/= '—') rest)
        | f <- facts
        , installFactKey f == Just pkg
        , let (_, rest) = T.breakOn "-- cabal:" f
        , not (T.null rest)
        ]

writableDraft :: Text -> Bool
writableDraft d = not (T.null (T.strip d)) && not (containsTypedHole d)

candidateCell :: [Text] -> Maybe Text
candidateCell = candidateCellFrom Nothing

candidateCellFrom :: Maybe Text -> [Text] -> Maybe Text
candidateCellFrom (Just draft) _ | writableDraft draft = Just (T.stripEnd draft)
candidateCellFrom _ facts = do
    (name, sig, m, pkg) <- seedConsumer facts
    args <- traverse (fillArg facts) (argTypesOf sig)
    let importLine = ["import " <> m | not (T.null m)]
        cabalLine = maybeToList (cabalFor pkg facts)
    pure
        ( T.intercalate
            "\n"
            (cabalLine ++ importLine ++ [T.unwords (name : args)])
        )

fillArg :: [Text] -> Text -> Maybe Text
fillArg facts t = case literalFill t of
    Just lit -> Just lit
    Nothing -> probedProducer facts t

candidateGaps :: [Text] -> [Text]
candidateGaps facts = case seedConsumer facts of
    Nothing -> []
    Just (_, sig, _, _) ->
        nub [t | t <- argTypesOf sig, isNothing (fillArg facts t)]

candidateNames :: [Text] -> [Text]
candidateNames facts = case seedConsumer facts of
    Nothing -> []
    Just (name, _, m, pkg) -> [x | x <- [name, m, pkg], not (T.null x)]

candidateClause :: [Text] -> Text
candidateClause = candidateClauseFrom Nothing

candidateClauseFrom :: Maybe Text -> [Text] -> Text
candidateClauseFrom = candidateClauseAgainst Set.empty

candidateClauseAgainst :: Set Text -> Maybe Text -> [Text] -> Text
candidateClauseAgainst refuted mDraft facts =
    case candidateCellFrom mDraft facts of
        Just src
            | normaliseSource src `Set.notMember` refuted -> framing <> src
        _ -> ""
  where
    framing =
        "A candidate assembled from facts held this session. It has NOT been \
        \compiled, so treat it as a proposal, not an answer: write it with \
        \insert_cell if it fits the goal, and let the compiler decide.\n"
