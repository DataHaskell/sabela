{-# LANGUAGE OverloadedStrings #-}

{- | What the caller asked the disposable session to try, and the facts derived
from it before any session exists: the cabal metadata, the build budget, and
whether the notebook's own plan already rules the attempt out.
-}
module Sabela.Session.Materialize.Candidate (
    CandidateSpec (..),
    expressionCandidate,
    buildBudgetFor,
    disposableRouteName,
    prefixFor,
    candidateProjectMeta,
    materializationPlanFailure,
    candidateSafetyPrelude,
    candidateTimeoutUs,
    unrestrictedIOError,
    partitionReplayCells,
    diagnosticText,
) where

import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Health (healthOfCellError, isClean)
import Sabela.Compiled (CompilePlan (..))
import Sabela.Deps (collectMetadata, mergedMeta, repairDeps)
import Sabela.Model (Cell (..), CellError (..), Notebook (..))
import Sabela.Reactivity (ExecutionPlan (..))
import Sabela.Session.MaterializeStage (
    MaterializeFailure (..),
    MaterializeStage (..),
    SkippedCell (..),
    stageFailure,
 )
import Sabela.Session.Timeout (TimeoutConfig (..))
import ScriptHs.Parser (
    CabalMeta (..),
    ScriptFile (..),
    mergeMetas,
    parseScript,
 )

data CandidateSpec = CandidateSpec
    { candidateMetadataSource :: Text
    , candidateSetup :: Text
    , candidateExpression :: Maybe Text
    , candidateReplacesCellId :: Maybe Int
    , candidateDeliberate :: Bool
    }
    deriving (Eq, Show)

expressionCandidate :: Text -> CandidateSpec
expressionCandidate source =
    CandidateSpec
        { candidateMetadataSource = source
        , candidateSetup = ""
        , candidateExpression = Just source
        , candidateReplacesCellId = Nothing
        , candidateDeliberate = False
        }

buildBudgetFor :: CandidateSpec -> TimeoutConfig -> Int
buildBudgetFor spec tc
    | candidateDeliberate spec = tcBuildUs tc
    | otherwise = tcTryBuildUs tc

disposableRouteName :: Text
disposableRouteName = "disposable_scratch"

{- | The cells a candidate is compiled against: the ones above the cell it
replaces. The cells below consume what that cell defines, so replaying them
without the candidate refuses edits that would compile; whether they still
compile is settled when they re-run after the commit.
-}
prefixFor :: CandidateSpec -> Notebook -> Notebook
prefixFor spec nb = case candidateReplacesCellId spec of
    Nothing -> nb
    Just cid -> nb{nbCells = takeWhile ((/= cid) . cellId) (nbCells nb)}

candidateProjectMeta :: Set Text -> Notebook -> CandidateSpec -> CabalMeta
candidateProjectMeta globalDeps nb spec =
    mergedMeta globalDeps (mergeMetas [collectMetadata nb, candidateMeta])
  where
    parsed = scriptMeta (parseScript (candidateMetadataSource spec))
    candidateMeta = parsed{metaDeps = repairDeps (metaDeps parsed)}

materializationPlanFailure :: ExecutionPlan -> Maybe MaterializeFailure
materializationPlanFailure plan
    | Just cid <- S.lookupMin (epCycleIds plan) =
        Just (planFailure cid "notebook contains a dependency cycle")
    | Just (cid, names) <- M.lookupMin (epRedefErrors plan) =
        Just
            ( planFailure
                cid
                ("duplicate notebook definitions: " <> T.intercalate ", " names)
            )
    | Just (cid, errs) <- M.lookupMin (cpViolations (epCompilePlan plan)) =
        Just (planFailure cid (diagnosticText errs))
    | otherwise = Nothing
  where
    planFailure cid = stageFailure StagePlan (Just cid)

{- | Withdraws unchecked IO from the candidate's scope, and asks GHC to say
when it resolved an ambiguous type by defaulting, so the gate can tell a cell
that computes something from one that only type-checks.
-}
candidateSafetyPrelude :: CandidateSpec -> Text
candidateSafetyPrelude _ =
    ":module -System.IO.Unsafe\n:set -Wtype-defaults\n"

candidateTimeoutUs :: Int
candidateTimeoutUs = 30 * 1000000

unrestrictedIOError :: Text -> Bool
unrestrictedIOError raw =
    let lower = T.toLower raw
     in "sabela_unrestricted_io" `T.isInfixOf` lower
            || "scratch candidate is io" `T.isInfixOf` lower
            || "candidate is io" `T.isInfixOf` lower

partitionReplayCells :: [Cell] -> ([SkippedCell], [Cell])
partitionReplayCells = foldr step ([], [])
  where
    step cell (skips, keep)
        | isClean (healthOfCellError (cellError cell)) = (skips, cell : keep)
        | otherwise = (SkippedCell (cellId cell) (skipReason cell) : skips, keep)
    skipReason cell =
        maybe
            "cell has an unresolved compile error"
            (compact . T.strip)
            (cellError cell)
    compact = T.unwords . T.words

diagnosticText :: [CellError] -> Text
diagnosticText errs =
    case filter (not . T.null) (map (T.strip . ceMessage) errs) of
        [] -> "notebook plan is not materializable"
        messages -> T.intercalate "\n" messages
