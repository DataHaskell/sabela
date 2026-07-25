{-# LANGUAGE OverloadedStrings #-}

{- | G3: the harness asks the compiler its own questions. When guidance holds
a target type it has no producer for, this runs the hole-fit query itself —
through @try@'s typecheck-only route, so nothing is evaluated and nothing is
committed — and folds the conclusions into the fact ledger as plain
statements ("Sabela.AI.HoleProbe" renders them, with @via: hole-probe@).

'resolveCandidate' is the bounded synthesis loop: probe the open gaps, fill
them, re-check, for at most 'synthesisRoundCap' rounds. Only a candidate the
compiler accepted whole is ever returned; partial states stay in here.
-}
module Siza.Agent.Discover.HoleProbe (
    ProbeDispatch,
    probeTargetType,
    resolveCandidate,
    synthesisRoundCap,
) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.Text (Text)

import Sabela.AI.HoleProbe (
    holeProbeFacts,
    holeProbeProvenance,
    probeAnswered,
 )
import Sabela.AI.Types (ToolOutcome (..))
import Sabela.LLM.Ollama.Client (ToolCall (..))
import Siza.Agent.Discover.Candidate (candidateCellFrom, candidateGaps)
import Siza.Agent.Discover.Facts (foldFacts)

type ProbeDispatch = ToolCall -> IO (Either Text ToolOutcome)

{- | How many probe-and-refine rounds the harness may spend before it stops
and states what it holds. Bounded exactly like the G6 mitigation loop.
-}
synthesisRoundCap :: Int
synthesisRoundCap = 3

{- | Ask the compiler what produces @ty@ and return the conclusions as facts.
The question is a typed hole submitted to @try@, which admits it as
typecheck-only: compiled, never evaluated, never committed. An unanswerable
probe still yields a fact, so a gap is never left dangling.
-}
probeTargetType :: ProbeDispatch -> Text -> IO [Text]
probeTargetType dispatch ty = do
    r <- dispatch (ToolCall "try" (object ["code" .= ("_ :: " <> ty)]))
    pure $ case answerFacts (payloadOf r) of
        [] -> [noProducerFact ty]
        facts -> facts

-- | The probe's conclusions: the server's own, else parsed from its diagnostic.
answerFacts :: Value -> [Text]
answerFacts v = case textsAt "answer" v of
    [] -> holeProbeFacts (textAt "diagnostic" v)
    facts -> facts

noProducerFact :: Text -> Text
noProducerFact ty =
    "no producer of `" <> ty <> "` found in scope (" <> holeProbeProvenance <> ")"

{- | The bounded synthesis loop. Each round probes every still-open gap of the
ledger's candidate, folds the answers in, and re-derives; the loop ends when
no gap is open, when a probe adds nothing new, or at the round cap. The
returned candidate is 'Just' only when @try@ accepted it whole, so a partial
fill can never reach the model.
-}
resolveCandidate :: ProbeDispatch -> Maybe Text -> [Text] -> IO ([Text], Maybe Text)
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
    -- A gap the ledger has no fill for AND no probe has answered yet; an
    -- answered-but-empty gap is settled, so it never re-enters the loop.
    openGaps facts = [g | g <- candidateGaps facts, not (probeAnswered facts g)]
    verify facts = case candidateCellFrom mDraft facts of
        Nothing -> pure (facts, Nothing)
        Just src -> do
            ok <- compiles dispatch src
            pure (facts, if ok then Just src else Nothing)

-- | Does @try@ accept this candidate whole? Only then may it be surfaced.
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
