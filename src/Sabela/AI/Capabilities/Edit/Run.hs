{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Synchronous cell-execution helpers split out of "Sabela.AI.Capabilities.Edit".

These are the pieces every mutating tool (@replace_cell_source@,
@insert_cell@) calls so the tool response carries the freshly-computed
execution summary, plus the @execute_cell@ tool itself. Kept as a
separate module because the listener-and-timeout pattern in 'executeCell'
is also a natural reuse point for a REST blocking-run endpoint.
-}
module Sabela.AI.Capabilities.Edit.Run (
    autoExecuteAfterMutation,
    execExecuteCell,
    executeCell,
    missingCellError,
    abortCancelled,
    abortSuperseded,
    abortTimedOut,
) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically, readTChan)
import Data.Aeson (Value, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import System.Timeout (timeout)

import Sabela.AI.Capabilities.Edit.HoleSearch (
    holeFitCandidates,
    holeSearchCandidates,
    selectByTypeCheck,
 )
import Sabela.AI.Capabilities.Edit.Repair (
    ambiguousResolveCandidates,
    firstFix,
    hoogleCandidates,
    importResolveCandidates,
    moduleDepStep,
    moduleResolveCandidates,
 )
import Sabela.AI.Capabilities.Util (fieldInt)
import Sabela.AI.CellResult (mergeToolOk, toCellResult)
import Sabela.AI.Health (healthOfResult, improvesHealth)
import Sabela.AI.Store
import Sabela.AI.Types
import Sabela.Anthropic.Types (CancelToken, isCancelled)
import Sabela.Api (errorJson)
import Sabela.Diagnose (
    cellResultWithGuidance,
    guidanceForCell,
    guidancePairs,
 )
import Sabela.Handlers (ReactiveNotebook (..), updateCellSource)
import Sabela.Model
import qualified Sabela.SessionTypes as ST
import Sabela.State

{- | Run a single cell via the reactive notebook and return the typed
'CellResult' JSON for embedding as the mutation-tool @execution@ summary.
The outcome sum (Succeeded/Raised/Rejected/Aborted) and the @ok@ boolean
ride on the same value the @execute_cell@ tool emits.

@_store@ is the (currently unused) carrier for a staged Output chokepoint;
@crOutputs@ inline raw here. The in-browser chat is bounded by
'Sabela.AI.Orchestrator.Compact'; the REST bridge ('aiToolH') is deliberately
un-stashed on this path.
-}
autoExecuteAfterMutation ::
    App -> AIStore -> ReactiveNotebook -> CancelToken -> Int -> IO Value
autoExecuteAfterMutation app _store rn cancelTok cid = do
    res <- executeWithRepair app rn cid cancelTok
    pure (cellResultWithGuidance (toCellResult res (resultOutputs res)))

{- | @execute_cell@. @_store@ is the staged Output-chokepoint carrier — see
'autoExecuteAfterMutation'; @crOutputs@ inline raw on this path.
-}
execExecuteCell ::
    App -> AIStore -> ReactiveNotebook -> CancelToken -> Value -> IO ToolOutcome
execExecuteCell app _store rn cancelTok input =
    case fieldInt "cell_id" input of
        Nothing -> pure (errOutcome (errorJson "cell_id required"))
        Just cid -> do
            nb <- readNotebook (appNotebook app)
            case missingCellError (nbCells nb) cid of
                Just msg -> pure (errOutcome (errorJson msg))
                Nothing -> do
                    result <- executeWithRepair app rn cid cancelTok
                    let cr = toCellResult result (resultOutputs result)
                    pure (mergeToolOk cr (["cellId" .= cid] <> guidancePairs (guidanceForCell cr)))

{- | The @execute_cell@ pre-check: a target id absent from the notebook fails
fast with a clear, id-naming message. Without it the cell is never dispatched,
so no @EvCellResult@ ever broadcasts and 'executeCell' waits out its full
130s timeout before reporting a misleading abort.
-}
missingCellError :: [Cell] -> Int -> Maybe Text
missingCellError cells cid
    | any ((== cid) . cellId) cells = Nothing
    | otherwise = Just ("No cell with id " <> T.pack (show cid))

-- | Outputs an @executeCell@ result carried; @[]@ for an abort 'Left'.
resultOutputs :: Either Text ExecutionResult -> [OutputItem]
resultOutputs (Left _) = []
resultOutputs (Right er) = erOutputs er

executeCell ::
    App ->
    ReactiveNotebook ->
    Int ->
    CancelToken ->
    IO (Either Text ExecutionResult)
executeCell app rn cid cancelTok = do
    reqTime <- getCurrentTime
    resultVar <- newEmptyMVar
    listenerThread <- forkIO $ do
        chan <- subscribeBroadcast (appEvents app)
        let loop = do
                ev <- atomically $ readTChan chan
                case ev of
                    EvCellResult rid outputs err errs warns
                        | rid == cid ->
                            putMVar resultVar (ExecutionResult outputs err errs warns)
                    _ -> loop
        loop
    rnRunCellForced rn cid
    mResult <- timeout 130000000 (takeMVar resultVar)
    killThread listenerThread
    cancelled <- isCancelled cancelTok
    stale <- requestStale app reqTime
    if
        | cancelled -> pure (Left abortCancelled)
        | stale -> pure (Left abortSuperseded)
        | otherwise -> case mResult of
            Nothing -> pure (Left abortTimedOut)
            Just r -> pure (Right r)

{- | Run a cell, and if it fails with an error an obvious fixer can repair — a
missing package (add the @-- cabal: build-depends:@ line) or a missing LANGUAGE
extension (add the pragma) — apply the fix and re-run. Bounded by 'repairCap';
each fixer no-ops once its fix is already present, so a fix that does not help
cannot loop. These are the deterministic repairs GHC's own error names, so the
model never sees the noisy error for them.
-}
executeWithRepair ::
    App ->
    ReactiveNotebook ->
    Int ->
    CancelToken ->
    IO (Either Text ExecutionResult)
executeWithRepair app rn cid cancelTok = do
    res0 <- executeCell app rn cid cancelTok
    go repairCap res0
  where
    go n res
        | n <= 0 = pure res
        | otherwise = do
            nb <- readNotebook (appNotebook app)
            case lookupCell cid nb >>= (firstFix res . cellSource) of
                Just newSrc -> applyAndLoop n newSrc
                Nothing -> case lookupCell cid nb of
                    Nothing -> pure res
                    Just cell -> do
                        -- Phase 1 of B1: declare a near-miss module's package via
                        -- the commit path (a dep change restarts the kernel, which
                        -- verify-and-revert cannot survive); phase 2 renames below.
                        mDep <- moduleDepStep res (cellSource cell)
                        case mDep of
                            Just depSrc -> applyAndLoop n depSrc
                            Nothing -> repairCandidates n res cell
    repairCandidates n res cell = do
        let src = cellSource cell
        modCands <- moduleResolveCandidates app res src
        impCands <- importResolveCandidates app res src
        ambigCands <- ambiguousResolveCandidates res src
        kept <- verifyAndRevert n res src (modCands ++ impCands ++ ambigCands)
        case kept of
            Just newRes -> go (n - 1) newRes
            Nothing -> speculativeRepair n res cell
    {- Compile-only speculative tier: pick by a non-executing :type check, then
    commit + run only the winner, so a round never executes many candidates. -}
    speculativeRepair n res cell = do
        let src = cellSource cell
        holeSearchCands <- holeSearchCandidates app res src
        holeFitCands <- holeFitCandidates app res src
        mWin <- selectByTypeCheck app (holeSearchCands ++ holeFitCands)
        case mWin of
            Just winSrc -> applyAndLoop n winSrc
            Nothing -> restartRepair n res src
    {- Restart-causing tier: a dep add restarts the kernel, which reverting the
    source cannot undo, so commit the best candidate via the loop path instead of
    verify-and-revert (which would leave earlier cells' bindings wiped). -}
    restartRepair n res src = do
        hoogCands <- hoogleCandidates res src
        case hoogCands of
            (h : _) -> applyAndLoop n h
            [] -> pure res
    applyAndLoop n newSrc = do
        modifyNotebook (appNotebook app) (updateCellSource cid newSrc)
        broadcastNotebook app
        newRes <- executeCell app rn cid cancelTok
        go (n - 1) newRes

    {- Try each candidate with APPLY → RE-RUN → KEEP-IFF-IMPROVES, else REVERT
    to priorSrc. 'Just' the kept run for the first candidate whose diagnostics
    are a genuine improvement ('improvesHealth': no NEW diagnostic and at least
    one removed, or compiles clean); 'Nothing' when none improve. Comparing
    diagnostic SETS (not counts) rejects a candidate that trades one error for
    another. -}
    verifyAndRevert _ _ _ [] = pure Nothing
    verifyAndRevert n res priorSrc (cand : rest) = do
        modifyNotebook (appNotebook app) (updateCellSource cid cand)
        broadcastNotebook app
        newRes <- executeCell app rn cid cancelTok
        if improvesHealth (healthOfResult res) (healthOfResult newRes)
            then pure (Just newRes)
            else do
                modifyNotebook (appNotebook app) (updateCellSource cid priorSrc)
                broadcastNotebook app
                verifyAndRevert n res priorSrc rest

-- | Most automatic deterministic repairs a single run will attempt.
repairCap :: Int
repairCap = 3

{- | The three @executeCell@ @Left@ strings, named so 'Sabela.AI.CellResult'
and its tests map the real producer output, not a re-typed literal.
-}
abortCancelled, abortSuperseded, abortTimedOut :: Text
abortCancelled = "Cancelled"
abortSuperseded = "Request superseded by a kernel interrupt"
abortTimedOut = "Cell execution timed out (>120s)"

-- | Did the Haskell kernel interrupt after this request was stamped?
requestStale :: App -> UTCTime -> IO Bool
requestStale app reqTime =
    getHaskellSession (appSessions app)
        >>= maybe (pure False) (`ST.sbRequestStale` reqTime)
