{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Running a candidate in a throwaway project: snapshot the live notebook,
build a disposable cabal root, replay the notebook there, then run the
candidate. The stages live in the submodules; this owns the cache entry.
-}
module Sabela.Session.Materialize (
    CandidateSpec (..),
    expressionCandidate,
    DisposableVerdict (..),
    MaterializeStage (..),
    MaterializeFailure (..),
    SkippedCell (..),
    DisposableResult (..),
    runDisposableTry,
    MaterializeSnapshot,
    captureMaterializeSnapshot,
    snapshotStillCurrent,
    buildBudgetFor,
    candidateSafetyPrelude,
    candidateProjectMeta,
    materializationPlanFailure,
    partitionReplayCells,
    disposableRouteName,
    materializeStageText,
    unrestrictedIOError,
    evalCandidate,
    emptyResult,
) where

import Control.Exception (
    SomeAsyncException (..),
    SomeException,
    bracket,
    displayException,
    fromException,
    throwIO,
    try,
 )
import Control.Monad (void)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import System.Timeout (timeout)

import Sabela.Reactivity (
    ExecutionPlan,
    computeFullExecutionPlan,
    haskellCodeCells,
 )
import Sabela.Session (SessionConfig (..), mkSessionConfig)
import Sabela.Session.EnvKey (resolveLocalPackages)
import Sabela.Session.Materialize.Candidate (
    CandidateSpec (..),
    buildBudgetFor,
    candidateProjectMeta,
    candidateSafetyPrelude,
    disposableRouteName,
    expressionCandidate,
    materializationPlanFailure,
    partitionReplayCells,
    prefixFor,
    unrestrictedIOError,
 )
import Sabela.Session.Materialize.Pipeline (evalCandidate, runMaterialized)
import Sabela.Session.Materialize.Result (emptyResult, failed, snapshotFailure)
import Sabela.Session.MaterializeSnapshot (
    MaterializeSnapshot (..),
    captureMaterializeSnapshot,
    snapshotStillCurrent,
 )
import Sabela.Session.MaterializeStage (
    DisposableResult (..),
    DisposableVerdict (..),
    MaterializeFailure (..),
    MaterializeStage (..),
    SkippedCell (..),
    materializeStageText,
 )
import Sabela.Session.Process (ghciBackend, newSession)
import Sabela.Session.Project (ReplSupport (..), setupReplProject)
import Sabela.Session.Query (captureBindingsBaseline)
import Sabela.Session.Timeout (readTimeoutConfig, tryBuildTimedOutMessage)
import Sabela.Session.TryCache (
    CacheEntry (..),
    acquireCacheEntry,
    cacheKeyRaw,
    cacheKeyText,
    commitCacheEntry,
    discardCacheEntry,
    resolvedGhcVersion,
    shelveCacheEntry,
    tryCacheMaxEntries,
    tryCacheRoot,
 )
import Sabela.Session.TryCache.Lease (Lease, withBucketLease)
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..))
import Sabela.State.Environment (Environment (..))
import ScriptHs.Parser (CabalMeta (..))

runDisposableTry :: App -> CandidateSpec -> IO DisposableResult
runDisposableTry app spec = do
    captured <- captureMaterializeSnapshot app
    case captured of
        Left message -> pure (snapshotFailure (emptyResult []) [] message)
        Right snapshot -> do
            let nb = prefixFor spec (msNotebook snapshot)
                meta = candidateProjectMeta (envGlobalDeps env) nb spec
                deps = S.toAscList (S.fromList (metaDeps meta))
                plan = computeFullExecutionPlan (haskellCodeCells nb) nb
                base = emptyResult deps
            case materializationPlanFailure plan of
                Just failure ->
                    pure
                        base
                            { disposableVerdict = DisposableCompileError
                            , disposableFailure = Just failure
                            }
                Nothing -> do
                    ghcVersion <- resolvedGhcVersion
                    tryBuildBudget <- buildBudgetFor spec <$> readTimeoutConfig
                    let cacheRoot = tryCacheRoot (envTmpDir env)
                        localPackages =
                            resolveLocalPackages
                                (envWorkDir env)
                                (envLocalPackages env)
                                meta
                        key = cacheKeyText localPackages meta ghcVersion
                    withBucketLease cacheRoot (cacheKeyRaw key) tryBuildBudget $ \case
                        Nothing ->
                            pure
                                base
                                    { disposableVerdict = DisposableUnavailable
                                    , disposableFailure =
                                        Just
                                            ( MaterializeFailure
                                                StageSession
                                                Nothing
                                                envBusyMessage
                                            )
                                    }
                        Just lease -> do
                            entry <- acquireCacheEntry lease
                            outcome <-
                                try
                                    ( runInDisposableRoot
                                        app
                                        snapshot
                                        plan
                                        meta
                                        spec
                                        lease
                                        entry
                                        localPackages
                                        tryBuildBudget
                                        deps
                                    ) ::
                                    IO (Either SomeException DisposableResult)
                            case outcome of
                                Right result -> pure result
                                Left e -> do
                                    discardCacheEntry lease
                                    pure
                                        base
                                            { disposableVerdict = DisposableUnavailable
                                            , disposableFailure =
                                                Just
                                                    ( MaterializeFailure
                                                        StageProject
                                                        Nothing
                                                        (T.pack (displayException e))
                                                    )
                                            }
  where
    env = appEnv app

-- | Like 'try', but an asynchronous exception (cancellation) propagates.
trySync :: IO a -> IO (Either SomeException a)
trySync act = do
    r <- try act
    case r of
        Left e
            | Just (SomeAsyncException _) <- fromException e -> throwIO e
        _ -> pure r

envBusyMessage :: Text
envBusyMessage =
    T.pack "an identical environment trial is already in progress; retry shortly"

runInDisposableRoot ::
    App ->
    MaterializeSnapshot ->
    ExecutionPlan ->
    CabalMeta ->
    CandidateSpec ->
    Lease ->
    CacheEntry ->
    [FilePath] ->
    Int ->
    [Text] ->
    IO DisposableResult
runInDisposableRoot app snapshot plan meta spec lease entry localPackages tryBuildBudget deps = do
    let env = appEnv app
        projectDir = ceProjectDir entry
        base = emptyResult deps
    projectResult <-
        try (setupReplProject WithNotebookSupport localPackages projectDir meta)
    case projectResult of
        Left (e :: SomeException) -> do
            discardCacheEntry lease
            pure (failed base StageProject Nothing (T.pack (displayException e)))
        Right () -> do
            cfg0 <- mkSessionConfig projectDir (envWorkDir env)
            let cfg = cfg0{scJsonDiagnostics = False}
            spawned <- timeout tryBuildBudget (newSession cfg)
            case spawned of
                Nothing -> do
                    shelveCacheEntry lease
                    pure
                        base
                            { disposableVerdict = DisposableTimedOut
                            , disposableFailure =
                                Just
                                    ( MaterializeFailure
                                        StageSession
                                        Nothing
                                        (tryBuildTimedOutMessage deps tryBuildBudget)
                                    )
                            }
                Just sess ->
                    bracket
                        (pure (ghciBackend sess))
                        (closeQuietly . ST.sbClose)
                        ( \backend -> do
                            commitCacheEntry lease tryCacheMaxEntries
                            runMaterialized
                                app
                                snapshot
                                projectDir
                                plan
                                spec
                                base
                                (captureBindingsBaseline sess)
                                backend
                        )

closeQuietly :: IO () -> IO ()
closeQuietly action = void (try action :: IO (Either SomeException ()))
