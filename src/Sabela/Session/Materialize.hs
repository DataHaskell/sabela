{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Disposable, contextful notebook materialization.

This module reconstructs a notebook in a fresh GHCi process and a fresh Cabal
project, then optionally evaluates one compiler-admitted pure expression.  It
never installs into, queries, or mutates the managed live kernel.  Every call
owns its project, process, and build directory and tears all three down before
returning.

The caller splits a trial into:

* metadata source, used only to extend the disposable Cabal environment;
* setup, containing already-rendered non-executing imports/declarations; and
* an optional final expression.

The final expression goes through 'sbEvalPureLive', which owns the
@_sabelaPureWitness@ admission, timeout/recovery, @it@ restoration, and binding
fingerprint checks.  An unrestricted @IO@ expression is therefore never run.
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

    -- * Snapshot fence
    MaterializeSnapshot,
    captureMaterializeSnapshot,
    snapshotStillCurrent,

    -- * Pure pieces used by focused tests
    candidateProjectMeta,
    materializationPlanFailure,
    partitionReplayCells,
    disposableRouteName,
    materializeStageText,
) where

import Control.Exception (
    SomeException,
    bracket,
    displayException,
    try,
 )
import Control.Monad (forM_, void)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing)
import System.FilePath (isAbsolute, takeDirectory, (</>))
import System.Timeout (timeout)

import Sabela.AI.Health (healthOfCellError, isClean)
import Sabela.Bridge (bridgePreamble, widgetPreamble)
import Sabela.Compiled (CompilePlan (..), moduleFilePath)
import Sabela.Deps (collectMetadata, mergedMeta, repairDeps)
import Sabela.Handlers.Shared (partitionExports)
import Sabela.Model (Cell (..), CellError (..), Notebook (..))
import Sabela.Output (displayPrelude, parseMimeOutputs)
import Sabela.Reactivity (
    ExecutionPlan (..),
    computeFullExecutionPlan,
    haskellCodeCells,
 )
import Sabela.Session (SessionConfig (..), mkSessionConfig)
import Sabela.Session.Materialize.Run (
    runChecked,
    runLoadChecked,
    runOptional,
 )
import Sabela.Session.MaterializeSnapshot (
    MaterializeSnapshot (..),
    captureMaterializeSnapshot,
    snapshotStillCurrent,
    withCurrentSnapshot,
 )
import Sabela.Session.Process (ghciBackend, newSession)
import Sabela.Session.Project (ReplSupport (..), setupReplProject)
import Sabela.Session.Query (captureBindingsBaseline)
import Sabela.Session.Timeout (
    readTimeoutConfig,
    tcTryBuildUs,
    tryBuildTimedOutMessage,
 )
import Sabela.Session.TryCache (
    CacheEntry (..),
    acquireCacheEntry,
    cacheKeyText,
    commitCacheEntry,
    discardCacheEntry,
    resolvedGhcVersion,
    tryCacheMaxEntries,
    tryCacheRoot,
 )
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..))
import Sabela.State.Environment (Environment (..))
import ScriptHs.Parser (
    CabalMeta (..),
    ScriptFile (..),
    mergeMetas,
    parseScript,
    parseScriptNumbered,
 )
import ScriptHs.Render (toGhciScript)

-- | The candidate pieces supplied by the internal @try@ router.
data CandidateSpec = CandidateSpec
    { candidateMetadataSource :: Text
    {- ^ Full source used to collect Cabal directives.  It is never executed
    by this field alone.
    -}
    , candidateSetup :: Text
    {- ^ Already-rendered, non-executing imports and declarations.  The public
    router is responsible for declining GHCi commands and executable setup.
    -}
    , candidateExpression :: Maybe Text
    {- ^ A final expression to admit and evaluate.  'Nothing' means the setup
    is a declarations-only compile trial.
    -}
    , candidateReplacesCellId :: Maybe Int
    {- ^ The cell id a replace candidate supersedes, dropped from the
    reconstructed prefix so the candidate replays against the notebook AFTER
    the edit, not alongside its own stale predecessor. 'Nothing' for an insert.
    -}
    }
    deriving (Eq, Show)

-- | A one-expression trial with no separate setup.
expressionCandidate :: Text -> CandidateSpec
expressionCandidate source =
    CandidateSpec
        { candidateMetadataSource = source
        , candidateSetup = ""
        , candidateExpression = Just source
        , candidateReplacesCellId = Nothing
        }

data DisposableVerdict
    = DisposableOk
    | DisposableCompileError
    | DisposableRuntimeError
    | DisposableTimedOut
    | DisposableUnavailable
    deriving (Eq, Show)

data MaterializeStage
    = StagePlan
    | StageSnapshot
    | StageProject
    | StageSession
    | StageCompiled
    | StagePrelude
    | StageCellReplay
    | StageSafety
    | StageCandidateSetup
    | StageCandidateTypecheck
    | StageCandidateRun
    deriving (Eq, Show)

data MaterializeFailure = MaterializeFailure
    { failureStage :: MaterializeStage
    , failureCellId :: Maybe Int
    , failureMessage :: Text
    }
    deriving (Eq, Show)

{- | A prefix cell the disposable replay declined to materialize because it does
not compile in the notebook (its stored 'cellError'). Skipping it keeps a known-
red cell from failing the whole replay and misattributing to the candidate.
-}
data SkippedCell = SkippedCell
    { skippedCellId :: Int
    , skippedReason :: Text
    }
    deriving (Eq, Show)

data DisposableResult = DisposableResult
    { disposableRoute :: Text
    , disposableVerdict :: DisposableVerdict
    , disposableType :: Maybe Text
    , disposableStdout :: Text
    , disposableStderr :: Text
    , disposableFailure :: Maybe MaterializeFailure
    , disposableReplayedCells :: [Int]
    , disposableSkippedCells :: [SkippedCell]
    , disposableDependencies :: [Text]
    }
    deriving (Eq, Show)

disposableRouteName :: Text
disposableRouteName = "disposable_scratch"

-- | The wire text for a 'MaterializeStage', shared by every disposable-route caller.
materializeStageText :: MaterializeStage -> Text
materializeStageText stage = case stage of
    StagePlan -> "plan"
    StageProject -> "project"
    StageSession -> "session"
    StageCompiled -> "compiled"
    StagePrelude -> "prelude"
    StageCellReplay -> "cell_replay"
    StageSnapshot -> "snapshot"
    StageSafety -> "safety"
    StageCandidateSetup -> "candidate_setup"
    StageCandidateTypecheck -> "candidate_typecheck"
    StageCandidateRun -> "candidate_run"

{- | Reconstruct the supplied notebook snapshot and run a disposable trial.
The snapshot is read exactly once; the built package environment is served
from 'Sabela.Session.TryCache' (fresh scratch process every call regardless).
-}
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

{- | The notebook a candidate replays against: unchanged for an insert, or
with 'candidateReplacesCellId' dropped for a replace, so the stale copy
never also compiles (or double-defines) alongside the candidate.
-}
prefixFor :: CandidateSpec -> Notebook -> Notebook
prefixFor spec nb = case candidateReplacesCellId spec of
    Nothing -> nb
    Just cid -> nb{nbCells = filter ((/= cid) . cellId) (nbCells nb)}

{- | Merge the complete candidate Cabal metadata with the notebook and global
environment.  Unlike the legacy scratch path, this preserves extensions,
options, local package declarations, source repositories, and include/lib
directories instead of retaining only package names.
-}
candidateProjectMeta :: Set Text -> Notebook -> CandidateSpec -> CabalMeta
candidateProjectMeta globalDeps nb spec =
    mergedMeta globalDeps (mergeMetas [collectMetadata nb, candidateMeta])
  where
    parsed = scriptMeta (parseScript (candidateMetadataSource spec))
    candidateMeta = parsed{metaDeps = repairDeps (metaDeps parsed)}

{- | Reject notebook plans that the normal full-run planner would skip.  A
disposable reconstruction must not silently present a partial context as a
faithful notebook.
-}
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
    planFailure cid = MaterializeFailure StagePlan (Just cid)

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
            -- Text diagnostics keep replay failures stable across supported GHC
            -- versions and avoid leaking NDJSON into the try result.
            let cfg =
                    cfg0
                        { scJsonDiagnostics = False
                        , scCabalStoreDir = Just (ceStoreDir entry)
                        }
            tryBuildBudget <- tcTryBuildUs <$> readTimeoutConfig
            spawned <- timeout tryBuildBudget (newSession cfg)
            case spawned of
                Nothing -> do
                    discardCacheEntry (ceBucketDir entry)
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

runMaterialized ::
    App ->
    MaterializeSnapshot ->
    FilePath ->
    ExecutionPlan ->
    CandidateSpec ->
    DisposableResult ->
    IO () ->
    ST.SessionBackend ->
    IO DisposableResult
runMaterialized app snapshot projectDir plan spec base0 captureBaseline backend = do
    let context = snapshotRenderContext snapshot
        (skipped, toReplay) = partitionReplayCells (epCellsToRun plan)
        base = base0{disposableSkippedCells = skipped}
    compiled <- loadCompiled projectDir backend (epCompilePlan plan)
    case compiled of
        Left msg -> pure (failed base StageCompiled Nothing msg)
        Right () -> do
            preludeResult <- runChecked backend displayPrelude
            case preludeResult of
                Left msg -> pure (failed base StagePrelude Nothing msg)
                Right _ -> do
                    captured <- try captureBaseline
                    case captured of
                        Left (e :: SomeException) ->
                            pure
                                ( failed
                                    base
                                    StagePrelude
                                    Nothing
                                    (T.pack (displayException e))
                                )
                        Right () -> do
                            replayed <- replayCells backend context toReplay
                            case replayed of
                                Left (done, cid, stage, msg) ->
                                    pure
                                        (failed base stage (Just cid) msg)
                                            { disposableReplayedCells = compiledIds <> done
                                            }
                                Right (done, scratchBridge) -> do
                                    finalPrelude <- runChecked backend displayPrelude
                                    case finalPrelude of
                                        Left msg ->
                                            pure
                                                (failed base StagePrelude Nothing msg)
                                                    { disposableReplayedCells = compiledIds <> done
                                                    }
                                        Right _ -> do
                                            finalBridge <-
                                                runOptional
                                                    backend
                                                    (bridgePreamble scratchBridge)
                                            case finalBridge of
                                                Left msg ->
                                                    pure
                                                        (failed base StageCellReplay Nothing msg)
                                                            { disposableReplayedCells = compiledIds <> done
                                                            }
                                                Right _ -> do
                                                    safety <-
                                                        runChecked backend candidateSafetyPrelude
                                                    case safety of
                                                        Left msg ->
                                                            pure
                                                                (failed base StageSafety Nothing msg)
                                                                    { disposableReplayedCells =
                                                                        compiledIds <> done
                                                                    }
                                                        Right _ -> do
                                                            let candidateBase =
                                                                    base
                                                                        { disposableReplayedCells =
                                                                            compiledIds <> done
                                                                        }
                                                            candidate <-
                                                                withCurrentSnapshot
                                                                    app
                                                                    snapshot
                                                                    ( runCandidate
                                                                        backend
                                                                        spec
                                                                        candidateBase
                                                                    )
                                                            case candidate of
                                                                Left message ->
                                                                    pure
                                                                        ( snapshotFailure
                                                                            candidateBase
                                                                            (compiledIds <> done)
                                                                            message
                                                                        )
                                                                Right result -> pure result
  where
    compiledIds = map cellId (epCompileCells plan)

{- | Safe Haskell is defense in depth for candidate-owned setup.  Existing
notebook bindings may have unsafe provenance, so this establishes TypeOnly,
not SafeProvenance.  The disposable process means no flag restoration is
needed.
-}
candidateSafetyPrelude :: Text
candidateSafetyPrelude =
    T.unlines
        [ ":module -System.IO.Unsafe"
        , ":set -XSafe"
        , ":seti -XSafe"
        ]

runCandidate ::
    ST.SessionBackend ->
    CandidateSpec ->
    DisposableResult ->
    IO DisposableResult
runCandidate backend spec base = do
    setupResult <-
        if T.null (T.strip (candidateSetup spec))
            then pure (Right ("", ""))
            else runChecked backend (candidateSetup spec)
    case setupResult of
        Left msg -> pure (failed base StageCandidateSetup Nothing msg)
        Right (setupOut, setupErr) -> case candidateExpression spec of
            Nothing ->
                pure
                    base
                        { disposableVerdict = DisposableOk
                        , disposableStdout = setupOut
                        , disposableStderr = setupErr
                        }
            Just expression -> evalCandidate backend expression base

evalCandidate ::
    ST.SessionBackend -> Text -> DisposableResult -> IO DisposableResult
evalCandidate backend expression base = do
    generation <- ST.sbSessionGen backend
    result <-
        ST.sbEvalPureLive
            backend
            ST.PureEvalRequest
                { ST.pureEvalExpectedGeneration = generation
                , ST.pureEvalTimeoutUs = candidateTimeoutUs
                , ST.pureEvalExpression = expression
                }
    let out = ST.pureEvalOutput result
        err = ST.pureEvalError result
        inferredText = T.strip (ST.pureEvalInferredType result)
        inferred =
            if T.null inferredText
                then Nothing
                else Just inferredText
        finish verdict stage =
            base
                { disposableVerdict = verdict
                , disposableType = inferred
                , disposableStdout = out
                , disposableStderr = err
                , disposableFailure =
                    if T.null (T.strip err)
                        then Nothing
                        else Just (MaterializeFailure stage Nothing err)
                }
    case ST.pureEvalVerdict result of
        ST.PureEvalSucceeded -> pure (finish DisposableOk StageCandidateRun)
        ST.PureEvalRejected
            | unrestrictedIOError err -> runIOCandidate backend expression inferred base
            | otherwise -> pure (finish DisposableCompileError StageCandidateTypecheck)
        ST.PureEvalRuntimeError -> pure (finish DisposableRuntimeError StageCandidateRun)
        ST.PureEvalTimedOut -> pure (finish DisposableTimedOut StageCandidateRun)
        ST.PureEvalStale -> pure (finish DisposableUnavailable StageCandidateTypecheck)
        ST.PureEvalInvariantFailed -> pure (finish DisposableUnavailable StageCandidateRun)
        ST.PureEvalUnavailable -> pure (finish DisposableUnavailable StageCandidateRun)

{- | The disposable session is thrown away, so an IO candidate runs here as a
committed cell would. The live route keeps its purity guard
('Sabela.Session.Query.evalPureLive'), which backs @semantic_read_only@.
-}
runIOCandidate ::
    ST.SessionBackend ->
    Text ->
    Maybe Text ->
    DisposableResult ->
    IO DisposableResult
runIOCandidate backend expression inferred base = do
    outcome <- runChecked backend expression
    pure $ case outcome of
        Left message -> failed base StageCandidateRun Nothing message
        Right (out, err) ->
            base
                { disposableVerdict = DisposableOk
                , disposableType = inferred
                , disposableStdout = out
                , disposableStderr = err
                }

candidateTimeoutUs :: Int
candidateTimeoutUs = 30 * 1000000

-- The compiler-owned type family emits a stable token for the IO branch.  Keep
-- a compatibility fallback for older prelude wording during rolling upgrades.
unrestrictedIOError :: Text -> Bool
unrestrictedIOError raw =
    let lower = T.toLower raw
     in "sabela_unrestricted_io" `T.isInfixOf` lower
            || "scratch candidate is io" `T.isInfixOf` lower
            || "candidate is io" `T.isInfixOf` lower

data RenderContext = RenderContext
    { rcBridgeValues :: M.Map Text Text
    , rcWidgetValues :: M.Map Int (M.Map Text Text)
    }

snapshotRenderContext :: MaterializeSnapshot -> RenderContext
snapshotRenderContext snapshot =
    RenderContext
        { rcBridgeValues = msBridgeValues snapshot
        , rcWidgetValues = msWidgetValues snapshot
        }

renderCell :: RenderContext -> M.Map Text Text -> Cell -> Text
renderCell context bridge cell =
    widgetPreamble
        (cellId cell)
        (M.findWithDefault M.empty (cellId cell) (rcWidgetValues context))
        <> bridgePreamble bridge
        <> toGhciScript (scriptLines (fst (parseScriptNumbered (cellSource cell))))

replayCells ::
    ST.SessionBackend ->
    RenderContext ->
    [Cell] ->
    IO
        ( Either
            ([Int], Int, MaterializeStage, Text)
            ([Int], M.Map Text Text)
        )
replayCells backend context = go True [] (rcBridgeValues context)
  where
    -- The caller injected the first prelude immediately before capturing the
    -- baseline. Every later cell gets a fresh copy here, matching the normal
    -- engine's load-before-cell contract even if an earlier cell shadowed it.
    go _ done bridge [] = pure (Right (reverse done, bridge))
    go preludeReady done bridge (cell : rest) = do
        preludeResult <-
            if preludeReady
                then pure (Right ("", ""))
                else runChecked backend displayPrelude
        case preludeResult of
            Left msg ->
                pure (Left (reverse done, cellId cell, StagePrelude, msg))
            Right _ -> do
                result <- runChecked backend (renderCell context bridge cell)
                case result of
                    Left msg ->
                        pure
                            ( Left
                                (reverse done, cellId cell, StageCellReplay, msg)
                            )
                    Right (out, _) ->
                        go
                            False
                            (cellId cell : done)
                            (applyBridgeExports bridge out)
                            rest

applyBridgeExports :: M.Map Text Text -> Text -> M.Map Text Text
applyBridgeExports bridge rawOut =
    M.union
        (M.fromList [(name, T.strip value) | (name, value) <- exports])
        bridge
  where
    (exports, _) = partitionExports (parseMimeOutputs rawOut)

loadCompiled ::
    FilePath ->
    ST.SessionBackend ->
    CompilePlan ->
    IO (Either Text ())
loadCompiled projectDir backend cplan
    | M.null (cpModules cplan) = pure (Right ())
    | otherwise = do
        forM_ (M.toList (cpModules cplan)) $ \(name, source) -> do
            let path = projectDir </> moduleFilePath name
            createDirectoryIfMissing True (takeDirectory path)
            TIO.writeFile path source
        let moduleNames = M.keys (cpModules cplan)
            loadCommand =
                T.unwords
                    ( ":load"
                        : [T.pack (show (projectDir </> moduleFilePath name)) | name <- moduleNames]
                    )
        loaded <- runLoadChecked backend loadCommand
        case loaded of
            Left msg -> pure (Left msg)
            Right _ -> do
                imported <-
                    runChecked
                        backend
                        (T.unlines ["import " <> name | name <- moduleNames])
                pure (void imported)

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

diagnosticText :: [CellError] -> Text
diagnosticText errs =
    case filter (not . T.null) (map (T.strip . ceMessage) errs) of
        [] -> "notebook plan is not materializable"
        messages -> T.intercalate "\n" messages

emptyResult :: [Text] -> DisposableResult
emptyResult deps =
    DisposableResult
        { disposableRoute = disposableRouteName
        , disposableVerdict = DisposableUnavailable
        , disposableType = Nothing
        , disposableStdout = ""
        , disposableStderr = ""
        , disposableFailure = Nothing
        , disposableReplayedCells = []
        , disposableSkippedCells = []
        , disposableDependencies = deps
        }

{- | Split the interpreted replay prefix into the cells that do not compile
(skipped, each with its own error as the reason) and the cells to replay.
\"Does not compile\" reuses 'Sabela.AI.Health' — a cell whose stored 'cellError'
is not clean — so the try route judges red exactly as the rest of the system.
-}
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

snapshotFailure :: DisposableResult -> [Int] -> Text -> DisposableResult
snapshotFailure base replayed message =
    base
        { disposableVerdict = DisposableUnavailable
        , disposableStderr = message
        , disposableFailure =
            Just (MaterializeFailure StageSnapshot Nothing message)
        , disposableReplayedCells = replayed
        }

failed ::
    DisposableResult ->
    MaterializeStage ->
    Maybe Int ->
    Text ->
    DisposableResult
failed base stage cid message =
    base
        { disposableVerdict = verdictForFailure message
        , disposableStderr = message
        , disposableFailure = Just (MaterializeFailure stage cid message)
        }

verdictForFailure :: Text -> DisposableVerdict
verdictForFailure message
    | "timed out" `T.isInfixOf` T.toLower message = DisposableTimedOut
    | otherwise = DisposableCompileError

closeQuietly :: IO () -> IO ()
closeQuietly action = void (try action :: IO (Either SomeException ()))
