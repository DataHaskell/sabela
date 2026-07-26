{-# LANGUAGE OverloadedStrings #-}

{- | G2's one commit chokepoint: every repair candidate the cascade's tiers
propose passes through 'verifyAndRevert' or 'applyFresh', and both classify
via 'classifyCandidate' BEFORE touching the notebook. Isolated so the claim
"every self_heal application checks the gate first" is one small module to
audit, not scattered across the tier generators.
-}
module Sabela.AI.Capabilities.Edit.Cascade.Commit (
    classify,
    applyFresh,
    proposeDependency,
    verifyAndRevert,
    restoreIfStale,
) where

import Data.Aeson (Value)
import Data.IORef (IORef, modifyIORef', readIORef, writeIORef)
import Data.Text (Text)
import System.Environment (lookupEnv)

import Sabela.AI.Capabilities.Edit.Exec (executeCell)
import Sabela.AI.Capabilities.Edit.RepairGate (
    CandidateVerdict (..),
    classifyCandidate,
 )
import Sabela.AI.Health (healthOfResult, improvesHealthFor)
import Sabela.AI.Types (ExecutionResult (..))
import Sabela.Anthropic.Types (CancelToken)
import Sabela.Handlers (ReactiveNotebook, updateCellSource)
import Sabela.Model
import Sabela.Parse (cellNames)
import Sabela.Parse.Declared (preservesDeclarations)
import Sabela.SessionTypes (CellLang (..))
import Sabela.State

-- | Gate-check + dependency-classify @cand@ against the cell's live lang/type.
classify :: App -> Int -> Text -> Text -> IO CandidateVerdict
classify app cid priorSrc cand = do
    mc <- lookupCell cid <$> readNotebook (appNotebook app)
    let (lang, ty) = maybe (Haskell, CodeCell) (\c -> (cellLang c, cellType c)) mc
    classifyCandidate app cid lang ty priorSrc cand

{- | Gate-check a single proposal; push it as a disclosed suggestion when it
verifies as one, drop it silently otherwise (rejected or a no-op).
-}
proposeDependency ::
    App -> Int -> IORef [Value] -> Text -> Maybe (Text, Text) -> IO ()
proposeDependency _ _ _ _ Nothing = pure ()
proposeDependency app cid sugRef priorSrc (Just (_, cand)) = do
    verdict <- classify app cid priorSrc cand
    case verdict of
        CandidateSuggested note -> modifyIORef' sugRef (note :)
        _ -> pure ()

{- | Commit path for a single deterministic (never dependency-adding)
candidate: gate-checked first, then committed unconditionally — no health
comparison, matching the pre-G2 "obvious fixer" trust level.
-}
applyFresh ::
    App ->
    ReactiveNotebook ->
    CancelToken ->
    Int ->
    IORef Bool ->
    Text ->
    Text ->
    IO (Maybe (Either Text ExecutionResult))
applyFresh app rn cancelTok cid staleRef priorSrc newSrc = do
    verdict <- classify app cid priorSrc newSrc
    case verdict of
        CandidateApplyable cand -> do
            modifyNotebook (appNotebook app) (updateCellSource cid cand)
            broadcastNotebook app
            writeIORef staleRef False
            Just <$> executeCell app rn cid cancelTok
        _ -> pure Nothing

{- | Classify (gate + dependency check), then APPLY → RE-RUN → KEEP-IFF-IMPROVES
for the first candidate that is 'CandidateApplyable', else REVERT to
@priorSrc@ and try the next. A 'CandidateRejected' or 'CandidateSuggested'
candidate never touches the notebook at all — the hard prerequisite this
cascade used to discover only AFTER a live commit+run. 'Just' the kept run
for the first applyable candidate whose diagnostics are a genuine improvement
('improvesHealthFor'); 'Nothing' when none improve. Comparing diagnostic SETS
(not counts) rejects a candidate that trades one error for another; knock-on
scope errors for the cell's own defined names are excluded. A revert marks
the notebook stale for 'restoreIfStale'.
-}
verifyAndRevert ::
    App ->
    ReactiveNotebook ->
    CancelToken ->
    Int ->
    IORef [Value] ->
    IORef Bool ->
    Either Text ExecutionResult ->
    Text ->
    [Text] ->
    IO (Maybe (Either Text ExecutionResult))
verifyAndRevert _ _ _ _ _ _ _ _ [] = pure Nothing
verifyAndRevert app rn cancelTok cid sugRef staleRef res priorSrc (cand : rest)
    -- A candidate that drops a declared name substituted the deliverable
    -- (the signature-rename class); never apply it, however green.
    | not (preservesDeclarations priorSrc cand) =
        verifyAndRevert app rn cancelTok cid sugRef staleRef res priorSrc rest
verifyAndRevert app rn cancelTok cid sugRef staleRef res priorSrc (cand : rest) = do
    verdict <- classify app cid priorSrc cand
    case verdict of
        CandidateRejected -> next
        CandidateSuggested note -> modifyIORef' sugRef (note :) >> next
        CandidateApplyable _ -> do
            modifyNotebook (appNotebook app) (updateCellSource cid cand)
            broadcastNotebook app
            newRes <- executeCell app rn cid cancelTok
            let defined = fst (cellNames priorSrc)
                kept = improvesHealthFor defined (healthOfResult res) (healthOfResult newRes)
            debugDumpVerify res newRes kept
            if kept
                then writeIORef staleRef False >> pure (Just newRes)
                else do
                    modifyNotebook (appNotebook app) (updateCellSource cid priorSrc)
                    broadcastNotebook app
                    writeIORef staleRef True
                    next
  where
    next = verifyAndRevert app rn cancelTok cid sugRef staleRef res priorSrc rest

{- A reverted candidate leaves the notebook holding the FAILED candidate's
result against the restored source — the model then reads an error it never
caused. One re-run of the reverted source restores the truthful pair; costs a
single execution, only on the all-declined path. -}
restoreIfStale ::
    App ->
    ReactiveNotebook ->
    CancelToken ->
    Int ->
    IORef Bool ->
    Either Text ExecutionResult ->
    IO (Either Text ExecutionResult)
restoreIfStale app rn cancelTok cid staleRef res = do
    stale <- readIORef staleRef
    if stale then executeCell app rn cid cancelTok else pure res

-- | Verify verdicts appended to the file SABELA_DEBUG_VERIFY names.
debugDumpVerify ::
    Either Text ExecutionResult -> Either Text ExecutionResult -> Bool -> IO ()
debugDumpVerify oldRes newRes kept = do
    mp <- lookupEnv "SABELA_DEBUG_VERIFY"
    case mp of
        Just p
            | not (null p)
            , p /= "0" ->
                appendFile p $
                    "verify kept="
                        <> show kept
                        <> "\n  old: "
                        <> show (healthOfResult oldRes)
                        <> "\n  new: "
                        <> show (healthOfResult newRes)
                        <> "\n"
        _ -> pure ()
