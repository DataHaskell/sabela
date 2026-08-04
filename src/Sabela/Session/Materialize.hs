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
    SomeException,
    bracket,
    displayException,
    try,
 )
import Control.Monad (void)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (isAbsolute, (</>))
import System.Timeout (timeout)

import Sabela.Reactivity (
    ExecutionPlan,
    computeFullExecutionPlan,
    haskellCodeCells,
 )
import Sabela.Session (SessionConfig (..), mkSessionConfig)
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
    cacheKeyText,
    commitCacheEntry,
    discardCacheEntry,
    resolvedGhcVersion,
    shelveCacheEntry,
    tryCacheMaxEntries,
    tryCacheRoot,
 )
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
                    let cacheRoot = tryCacheRoot (envTmpDir env)
                        key = cacheKeyText meta ghcVersion
                    entry <- acquireCacheEntry cacheRoot key
                    outcome <-
                        try
                            ( runInDisposableRoot
                                app
                                snapshot
                                plan
                                meta
                                spec
                                entry
                                cacheRoot
                                deps
                            ) ::
                            IO (Either SomeException DisposableResult)
                    case outcome of
                        Right result -> pure result
                        Left e -> do
                            discardCacheEntry (ceBucketDir entry)
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

runInDisposableRoot ::
    App ->
    MaterializeSnapshot ->
    ExecutionPlan ->
    CabalMeta ->
    CandidateSpec ->
    CacheEntry ->
    FilePath ->
    [Text] ->
    IO DisposableResult
runInDisposableRoot app snapshot plan meta spec entry cacheRoot deps = do
    let env = appEnv app
        projectDir = ceProjectDir entry
        localPackages = resolveLocalPackages env meta
        base = emptyResult deps
    projectResult <-
        try (setupReplProject WithNotebookSupport localPackages projectDir meta)
    case projectResult of
        Left (e :: SomeException) -> do
            discardCacheEntry (ceBucketDir entry)
            pure (failed base StageProject Nothing (T.pack (displayException e)))
        Right () -> do
            cfg0 <- mkSessionConfig projectDir (envWorkDir env)
            let cfg =
                    cfg0
                        { scJsonDiagnostics = False
                        , scCabalStoreDir = Just (ceStoreDir entry)
                        }
            tryBuildBudget <- buildBudgetFor spec <$> readTimeoutConfig
            spawned <- timeout tryBuildBudget (newSession cfg)
            case spawned of
                Nothing -> do
                    shelveCacheEntry (ceBucketDir entry)
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
                Just sess -> do
                    commitCacheEntry cacheRoot (ceBucketDir entry) tryCacheMaxEntries
                    bracket
                        (pure (ghciBackend sess))
                        (closeQuietly . ST.sbClose)
                        ( runMaterialized
                            app
                            snapshot
                            projectDir
                            plan
                            spec
                            base
                            (captureBindingsBaseline sess)
                        )

resolveLocalPackages :: Environment -> CabalMeta -> [FilePath]
resolveLocalPackages env meta =
    stableNub (envLocalPackages env <> map resolve (metaPackages meta))
  where
    resolve raw =
        let path = T.unpack raw
         in if isAbsolute path then path else envWorkDir env </> path

stableNub :: (Ord a) => [a] -> [a]
stableNub = go S.empty
  where
    go _ [] = []
    go seen (x : xs)
        | x `S.member` seen = go seen xs
        | otherwise = x : go (S.insert x seen) xs

closeQuietly :: IO () -> IO ()
closeQuietly action = void (try action :: IO (Either SomeException ()))
