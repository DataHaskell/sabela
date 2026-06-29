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
    countErrorsInResult,
    shouldKeep,
    abortCancelled,
    abortSuperseded,
    abortTimedOut,
) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically, readTChan)
import Data.Aeson (Value, (.=))
import Data.Maybe (isJust, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import System.Timeout (timeout)
import Text.Read (readMaybe)

import Sabela.AI.Capabilities.Util (fieldInt)
import Sabela.AI.CellResult (mergeToolOk, toCellResult)
import Sabela.AI.DepRepair (addBuildDepend, depFromResult)
import Sabela.AI.ExtRepair (addExtension, extFromResult)
import Sabela.AI.HoogleResolve (hoogleResolveTopK)
import Sabela.AI.ImportRepair (addScopedImport, moduleRenameFix)
import Sabela.AI.Store
import Sabela.AI.Types
import Sabela.Anthropic.Types (CancelToken, isCancelled)
import Sabela.Api (errorJson)
import Sabela.Diagnose (
    cellResultWithGuidance,
    guidanceForCell,
    guidancePairs,
    notInScopeName,
 )
import Sabela.Handlers (ReactiveNotebook (..), updateCellSource)
import Sabela.Model
import qualified Sabela.SessionTypes as ST
import Sabela.State
import System.Environment (lookupEnv)

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
                        cands <- hoogleCandidates res (cellSource cell)
                        kept <- verifyAndRevert n res (cellSource cell) cands
                        case kept of
                            Nothing -> pure res
                            Just newRes -> go (n - 1) newRes
    applyAndLoop n newSrc = do
        modifyNotebook (appNotebook app) (updateCellSource cid newSrc)
        broadcastNotebook app
        newRes <- executeCell app rn cid cancelTok
        go (n - 1) newRes

    {- Try each candidate with APPLY → RE-RUN → KEEP-IFF-IMPROVES, else REVERT
    to priorSrc. 'Just' the kept run for the first candidate that strictly
    reduces the error count (or compiles clean); 'Nothing' when none improve. -}
    verifyAndRevert _ _ _ [] = pure Nothing
    verifyAndRevert n res priorSrc (cand : rest) = do
        let oldCount = countErrorsInResult res
        modifyNotebook (appNotebook app) (updateCellSource cid cand)
        broadcastNotebook app
        newRes <- executeCell app rn cid cancelTok
        if shouldKeep oldCount (countErrorsInResult newRes)
            then pure (Just newRes)
            else do
                modifyNotebook (appNotebook app) (updateCellSource cid priorSrc)
                broadcastNotebook app
                verifyAndRevert n res priorSrc rest

{- | The error count of a run, used to decide whether a candidate improved. A
@Left@ (abort/timeout/superseded) is WORSE than any @Right@ — a large sentinel —
so a candidate that turns a compile error into an abort is always reverted. A
@Right@ counts its holistic error (0/1) plus its structured error list.
-}
countErrorsInResult :: Either Text ExecutionResult -> Int
countErrorsInResult (Left _) = 1000000
countErrorsInResult (Right er) =
    (if isJust (erError er) then 1 else 0) + length (erErrors er)

-- | Keep a candidate iff it strictly improves the count or compiles clean.
shouldKeep :: Int -> Int -> Bool
shouldKeep oldCount newCount = newCount < oldCount || newCount == 0

{- | The flag-gated local-Hoogle candidate sources, tried only after every pure
fixer declines. With @SABELA_HOOGLE_RESOLVE@ unset this is the empty list, so the
default repair path is byte-identical to before. When set, resolve the
not-in-scope name to up to K (package, module) candidates via the LOCAL hoogle
CLI and, for each, add the @-- cabal:@ build-depend plus a SCOPED @import M
(name)@. K defaults to 'defaultTopK', override via @SABELA_HOOGLE_TOP_K@. Each
candidate is a kernel-restart+install, so K is the cost knob.
-}
hoogleCandidates :: Either Text ExecutionResult -> Text -> IO [Text]
hoogleCandidates res src = do
    enabled <- lookupEnv "SABELA_HOOGLE_RESOLVE"
    case enabled >> notInScopeFromResult res of
        Nothing -> pure []
        Just name -> do
            k <- topKFromEnv
            resolved <- hoogleResolveTopK k name
            pure
                [ src'
                | (pkg, modul) <- resolved
                , let src' = addScopedImport modul name (addBuildDepend pkg src)
                , src' /= src
                ]

-- | Default top-K candidates the hoogle resolver tries per repair attempt.
defaultTopK :: Int
defaultTopK = 3

-- | The candidate budget K, from @SABELA_HOOGLE_TOP_K@ or 'defaultTopK'.
topKFromEnv :: IO Int
topKFromEnv = do
    mk <- lookupEnv "SABELA_HOOGLE_TOP_K"
    pure (maybe defaultTopK (max 1) (mk >>= readMaybe))

-- | The first not-in-scope name across a failed run's error and error list.
notInScopeFromResult :: Either Text ExecutionResult -> Maybe Text
notInScopeFromResult (Left _) = Nothing
notInScopeFromResult (Right er) =
    listToMaybe (concatMap (mapMaybe notInScopeName . T.lines) errorTexts)
  where
    errorTexts = maybe [] pure (erError er) ++ map ceMessage (erErrors er)

{- | The deterministic source fixers, tried in order: the first whose rewrite
actually changes the source wins this round. Each maps a failed run plus the cell
source to a repaired source, or Nothing when it does not apply.
-}
repairFixers :: [Either Text ExecutionResult -> Text -> Maybe Text]
repairFixers =
    [ \res src -> (`addBuildDepend` src) <$> depFromResult res
    , \res src -> (`addExtension` src) <$> extFromResult res
    , moduleRenameFix
    ]

-- | The first fixer's rewrite that actually changes the source.
firstFix :: Either Text ExecutionResult -> Text -> Maybe Text
firstFix res src =
    listToMaybe [s | fixer <- repairFixers, Just s <- [fixer res src], s /= src]

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
