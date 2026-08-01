{-# LANGUAGE OverloadedStrings #-}

{- | What a green trial hands back so its caller can commit it: the source the
compiler actually read, and the packages committing it would add. Both are
facts about the candidate, not advice about what to do with it.
-}
module Sabela.AI.Capabilities.Try.Commit (
    withField,
    withPairs,
    discloseCandidate,
    dependenciesAdded,
    routedEnvelope,
) where

import Data.Aeson (Value (..), toJSON)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Pair)
import qualified Data.Set as S
import Data.Text (Text)
import ScriptHs.Parser (CabalMeta (..))

import Sabela.AI.Capabilities.TryPlan (TrialPlan (..))
import Sabela.AI.Types (ToolOutcome (..), toolOutcomeValue)
import Sabela.AI.Verdict (VerdictClass (..), verdictTag)
import Sabela.Session.Materialize (CandidateSpec (..), candidateProjectMeta)
import Sabela.State (App (..))
import Sabela.State.Environment (Environment (..))
import Sabela.State.NotebookStore (readNotebook)

withField :: Text -> Value -> ToolOutcome -> ToolOutcome
withField k v (ToolOk (Object o)) = ToolOk (Object (KM.insert (Key.fromText k) v o))
withField k v (ToolErr (Object o)) = ToolErr (Object (KM.insert (Key.fromText k) v o))
withField _ _ out = out

withPairs :: [Pair] -> ToolOutcome -> ToolOutcome
withPairs pairs out = foldr (\(k, v) -> withField (Key.toText k) v) out pairs

{- | try's envelope over an answer another capability produced: which tool
served it, and the outcome class that answer's own verdict states. A non-ok
verdict is an error outcome here, as a diagnostic is on every other try route.
-}
routedEnvelope :: Text -> Text -> ToolOutcome -> ToolOutcome
routedEnvelope command served out =
    reclassify
        . withField "routedFrom" (String command)
        . withField "route" (String served)
        . withField "outcome" (String outcome)
        $ out
  where
    verdict = case toolOutcomeValue out of
        Object o -> KM.lookup (Key.fromText "verdict") o
        _ -> Nothing
    isOk = verdict == Just (String (verdictTag VerdictOk))
    outcome
        | isOk = "ok"
        | verdict == Just (String (verdictTag VerdictDiagnostic)) = "diagnostic"
        | otherwise = "unavailable"
    reclassify o
        | isOk = o
        | otherwise = ToolErr (toolOutcomeValue o)

{- | The source is the plan's own, so an autofix path discloses what it rewrote
rather than what the caller submitted. The tier is not disclosed here: it is a
fact about the run, and the run's own payload carries it.
-}
discloseCandidate :: App -> TrialPlan -> ToolOutcome -> IO ToolOutcome
discloseCandidate app plan out = do
    added <- dependenciesAdded app plan
    pure
        . withField "source" (String (trialSource plan))
        . withField "dependenciesAdded" (toJSON added)
        $ out

{- | The candidate's own packages, less the closure the notebook already
resolves: what committing it would install, not what it compiled against.
-}
dependenciesAdded :: App -> TrialPlan -> IO [Text]
dependenciesAdded app plan = do
    nb <- readNotebook (appNotebook app)
    let depsOf src =
            S.fromList
                ( metaDeps
                    (candidateProjectMeta (envGlobalDeps (appEnv app)) nb (metaSpec src))
                )
    pure (S.toAscList (depsOf (trialSource plan) `S.difference` depsOf ""))
  where
    metaSpec src =
        CandidateSpec
            { candidateMetadataSource = src
            , candidateSetup = ""
            , candidateExpression = Nothing
            , candidateReplacesCellId = Nothing
            , candidateDeliberate = False
            }
