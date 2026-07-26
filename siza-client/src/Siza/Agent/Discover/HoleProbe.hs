{-# LANGUAGE OverloadedStrings #-}

module Siza.Agent.Discover.HoleProbe (
    ProbeDispatch,
    factSignature,
    groundedTarget,
    probeGroundedType,
    probeTargetType,
    resolveCandidate,
    synthesisRoundCap,
) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.HoleProbe (
    holeProbeFacts,
    holeProbeProvenance,
    probeAnswered,
 )
import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.Discover.Candidate (candidateCellFrom, candidateGaps)
import Siza.Agent.Discover.Facts (foldFacts)
import Siza.Agent.Discover.Goal (producesGoal)

type ProbeDispatch = ToolCall -> IO (Either Text ToolOutcome)

synthesisRoundCap :: Int
synthesisRoundCap = 3

probeTargetType :: ProbeDispatch -> Text -> IO [Text]
probeTargetType dispatch ty = do
    r <- dispatch (ToolCall "try" (object ["code" .= ("_ :: " <> ty)]))
    pure $ case answerFacts (payloadOf r) of
        [] -> [noProducerFact ty]
        facts -> facts

probeGroundedType :: ProbeDispatch -> [Text] -> Text -> IO [Text]
probeGroundedType dispatch held ty
    | groundedTarget held ty = probeTargetType dispatch ty
    | otherwise = pure []

groundedTarget :: [Text] -> Text -> Bool
groundedTarget held ty =
    not (T.null (T.strip ty))
        && any (producesGoal ty) (mapMaybe factSignature held)

factSignature :: Text -> Maybe Text
factSignature f = case T.breakOn " :: " f of
    (_, rest)
        | not (T.null rest) ->
            Just (T.strip (T.takeWhile (/= '—') (T.drop 4 rest)))
    _ -> Nothing

answerFacts :: Value -> [Text]
answerFacts v = case textsAt "answer" v of
    [] -> holeProbeFacts (textAt "diagnostic" v)
    facts -> facts

noProducerFact :: Text -> Text
noProducerFact ty =
    "no producer of `" <> ty <> "` found in scope (" <> holeProbeProvenance <> ")"

resolveCandidate ::
    ProbeDispatch -> Maybe Text -> [Text] -> IO ([Text], Maybe Text)
resolveCandidate dispatch mDraft = go synthesisRoundCap
  where
    go k facts = case openGaps facts of
        [] -> verify facts
        gaps
            | k <= 0 -> verify facts
            | otherwise -> do
                probed <- concat <$> mapM (probeTargetType dispatch) gaps
                let facts' = foldFacts probed facts
                if facts' == facts then verify facts else go (k - 1) facts'
    openGaps facts = [g | g <- candidateGaps facts, not (probeAnswered facts g)]
    verify facts = case candidateCellFrom mDraft facts of
        Nothing -> pure (facts, Nothing)
        Just src -> do
            ok <- compiles dispatch src
            pure (facts, if ok then Just src else Nothing)

compiles :: ProbeDispatch -> Text -> IO Bool
compiles dispatch src = do
    r <- dispatch (ToolCall "try" (object ["code" .= src]))
    let v = payloadOf r
    pure (isOk r && textAt "verdict" v == "ok" && textAt "outcome" v /= "hole_fits")
  where
    isOk (Right (ToolOk _)) = True
    isOk _ = False

payloadOf :: Either Text ToolOutcome -> Value
payloadOf (Right (ToolOk v)) = v
payloadOf (Right (ToolErr v)) = v
payloadOf _ = object []

textAt :: Text -> Value -> Text
textAt k (Object o) = case KM.lookup (K.fromText k) o of
    Just (String s) -> s
    _ -> ""
textAt _ _ = ""

textsAt :: Text -> Value -> [Text]
textsAt k (Object o) = case KM.lookup (K.fromText k) o of
    Just (Array a) -> [s | String s <- toList a]
    _ -> []
textsAt _ _ = []
