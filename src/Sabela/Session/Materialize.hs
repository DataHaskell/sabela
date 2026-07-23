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
    DisposableResult (..),
    runDisposableTry,

    -- * Snapshot fence
    MaterializeSnapshot,
    captureMaterializeSnapshot,
    snapshotStillCurrent,

    -- * Pure pieces used by focused tests
    candidateProjectMeta,
    materializationPlanFailure,
    disposableRouteName,
) where

import Control.Concurrent.MVar (withMVar)
import Control.Exception (
    SomeException,
    bracket,
    displayException,
    try,
 )
import Control.Monad (forM_, void)
import Data.IORef (readIORef)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing)
import System.FilePath (isAbsolute, takeDirectory, (</>))
import System.IO.Temp (withTempDirectory)
import System.Timeout (timeout)

import Sabela.Bridge (bridgePreamble, isTemplateHaskellOutput, widgetPreamble)
import Sabela.Compiled (CompilePlan (..), moduleFilePath)
import Sabela.Deps (collectMetadata, mergedMeta, repairDeps)
import Sabela.Errors (parseErrors)
import Sabela.Handlers.Shared (partitionExports)
import Sabela.Model (Cell (..), CellError (..), Notebook)
import Sabela.Output (displayPrelude, parseMimeOutputs)
import Sabela.Reactivity (
    ExecutionPlan (..),
    computeFullExecutionPlan,
    haskellCodeCells,
 )
import Sabela.Session (SessionConfig (..), mkSessionConfig)
import Sabela.Session.Process (ghciBackend, newSession)
import Sabela.Session.Project (ReplSupport (..), setupReplProject)
import Sabela.Session.Query (captureBindingsBaseline)
import Sabela.Session.Timeout (readTimeoutConfig, tcBuildUs)
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..))
import qualified Sabela.State.BridgeStore as BridgeStore
import Sabela.State.Environment (Environment (..))
import qualified Sabela.State.EventBus as EventBus
import qualified Sabela.State.NotebookStore as NotebookStore
import qualified Sabela.State.WidgetStore as WidgetStore
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
    }
    deriving (Eq, Show)

-- | A one-expression trial with no separate setup.
expressionCandidate :: Text -> CandidateSpec
expressionCandidate source =
    CandidateSpec
        { candidateMetadataSource = source
        , candidateSetup = ""
        , candidateExpression = Just source
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

data DisposableResult = DisposableResult
    { disposableRoute :: Text
    , disposableVerdict :: DisposableVerdict
    , disposableType :: Maybe Text
    , disposableStdout :: Text
    , disposableStderr :: Text
    , disposableFailure :: Maybe MaterializeFailure
    , disposableReplayedCells :: [Int]
    , disposableDependencies :: [Text]
    }
    deriving (Eq, Show)

disposableRouteName :: Text
disposableRouteName = "disposable_scratch"

{- | One immutable cut across every App value used to reconstruct a notebook.
The scratch process never consults the mutable stores again; it only checks
this cut for staleness immediately before admitting candidate code.
-}
data MaterializeSnapshot = MaterializeSnapshot
    { msNotebook :: Notebook
    , msEventGeneration :: Int
    , msBridgeValues :: M.Map Text Text
    , msWidgetValues :: M.Map Int (M.Map Text Text)
    }
    deriving (Eq)

snapshotCaptureRetries :: Int
snapshotCaptureRetries = 3

captureMaterializeSnapshot :: App -> IO (Either Text MaterializeSnapshot)
captureMaterializeSnapshot app = attempt (snapshotCaptureRetries + 1)
  where
    attempt remaining = do
        generationBefore <- readGeneration app
        snapshot <- copyMaterializeStores app generationBefore
        generationAfter <- readGeneration app
        if generationBefore == generationAfter
            then pure (Right snapshot)
            else
                if remaining > 1
                    then attempt (remaining - 1)
                    else
                        pure
                            ( Left
                                "notebook changed while capturing disposable context; no candidate code ran"
                            )

copyMaterializeStores :: App -> Int -> IO MaterializeSnapshot
copyMaterializeStores app generation =
    withMVar (NotebookStore.nsNotebook (appNotebook app)) $ \notebook ->
        withMVar (BridgeStore.bsValues (appBridge app)) $ \bridge ->
            withMVar (WidgetStore.wsValues (appWidgets app)) $ \widgets ->
                pure
                    MaterializeSnapshot
                        { msNotebook = notebook
                        , msEventGeneration = generation
                        , msBridgeValues = bridge
                        , msWidgetValues = widgets
                        }

readGeneration :: App -> IO Int
readGeneration = readIORef . EventBus.ebGeneration . appEvents

snapshotStillCurrent :: App -> MaterializeSnapshot -> IO (Either Text ())
snapshotStillCurrent app expected = do
    current <- captureMaterializeSnapshot app
    pure $ case current of
        Left message -> Left message
        Right actual
            | actual == expected -> Right ()
            | otherwise ->
                Left
                    "notebook or render context changed during disposable materialization; no candidate code ran"

{- | Compare and run while every mutable context store remains locked. This
closes the check/use gap at the candidate boundary: edits and widget/bridge
updates cannot land between the equality check and the bounded candidate.
-}
withCurrentSnapshot ::
    App ->
    MaterializeSnapshot ->
    IO a ->
    IO (Either Text a)
withCurrentSnapshot app expected action =
    withMVar (NotebookStore.nsNotebook (appNotebook app)) $ \notebook ->
        withMVar (BridgeStore.bsValues (appBridge app)) $ \bridge ->
            withMVar (WidgetStore.wsValues (appWidgets app)) $ \widgets -> do
                generationBefore <- readGeneration app
                if notebook /= msNotebook expected
                    || bridge /= msBridgeValues expected
                    || widgets /= msWidgetValues expected
                    || generationBefore /= msEventGeneration expected
                    then
                        pure
                            ( Left
                                "notebook or render context changed during disposable materialization; no candidate code ran"
                            )
                    else do
                        result <- action
                        generationAfter <- readGeneration app
                        pure $
                            if generationAfter == msEventGeneration expected
                                then Right result
                                else
                                    Left
                                        "notebook generation changed while the isolated candidate ran; its result was discarded"

{- | Reconstruct the supplied notebook snapshot and run a disposable trial.
The snapshot is read exactly once; the live session manager and dependency
tracker are intentionally absent from this module's write surface.
-}
runDisposableTry :: App -> CandidateSpec -> IO DisposableResult
runDisposableTry app spec = do
    captured <- captureMaterializeSnapshot app
    case captured of
        Left message -> pure (snapshotFailure (emptyResult []) [] message)
        Right snapshot -> do
            let nb = msNotebook snapshot
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
                    outcome <-
                        try
                            ( withTempDirectory (envTmpDir env) "sabela-try" $ \root ->
                                runInDisposableRoot app snapshot plan meta spec root
                            ) ::
                            IO (Either SomeException DisposableResult)
                    pure $ case outcome of
                        Right result -> result
                        Left e ->
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
    FilePath ->
    IO DisposableResult
runInDisposableRoot app snapshot plan meta spec root = do
    let env = appEnv app
        projectDir = root </> "project"
        localPackages = resolveLocalPackages env meta
        deps = S.toAscList (S.fromList (metaDeps meta))
        base = emptyResult deps
    projectResult <-
        try (setupReplProject WithNotebookSupport localPackages projectDir meta)
    case projectResult of
        Left (e :: SomeException) ->
            pure (failed base StageProject Nothing (T.pack (displayException e)))
        Right () -> do
            cfg0 <- mkSessionConfig projectDir (envWorkDir env)
            -- Text diagnostics keep replay failures stable across supported GHC
            -- versions and avoid leaking NDJSON into the try result.
            let cfg =
                    cfg0
                        { scJsonDiagnostics = False
                        , scCabalStoreDir = Just (root </> "cabal-store")
                        }
            buildBudget <- tcBuildUs <$> readTimeoutConfig
            spawned <- timeout buildBudget (newSession cfg)
            case spawned of
                Nothing ->
                    pure
                        base
                            { disposableVerdict = DisposableTimedOut
                            , disposableFailure =
                                Just
                                    ( MaterializeFailure
                                        StageSession
                                        Nothing
                                        "disposable GHCi startup/package build timed out"
                                    )
                            }
                Just sess ->
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
runMaterialized app snapshot projectDir plan spec base captureBaseline backend = do
    let context = snapshotRenderContext snapshot
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
                            replayed <- replayCells backend context (epCellsToRun plan)
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
    pure $ case ST.pureEvalVerdict result of
        ST.PureEvalSucceeded -> finish DisposableOk StageCandidateRun
        ST.PureEvalRejected
            | unrestrictedIOError err ->
                finish DisposableUnavailable StageCandidateTypecheck
            | otherwise -> finish DisposableCompileError StageCandidateTypecheck
        ST.PureEvalRuntimeError -> finish DisposableRuntimeError StageCandidateRun
        ST.PureEvalTimedOut -> finish DisposableTimedOut StageCandidateRun
        ST.PureEvalStale -> finish DisposableUnavailable StageCandidateTypecheck
        ST.PureEvalInvariantFailed -> finish DisposableUnavailable StageCandidateRun
        ST.PureEvalUnavailable -> finish DisposableUnavailable StageCandidateRun

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

runOptional :: ST.SessionBackend -> Text -> IO (Either Text (Text, Text))
runOptional backend source
    | T.null (T.strip source) = pure (Right ("", ""))
    | otherwise = runChecked backend source

runChecked :: ST.SessionBackend -> Text -> IO (Either Text (Text, Text))
runChecked = runCheckedWith False

runLoadChecked :: ST.SessionBackend -> Text -> IO (Either Text (Text, Text))
runLoadChecked = runCheckedWith True

runCheckedWith ::
    Bool ->
    ST.SessionBackend ->
    Text ->
    IO (Either Text (Text, Text))
runCheckedWith checkLoadOutput backend source = do
    outcome <-
        try (ST.sbRunBlock backend source) ::
            IO (Either SomeException (Text, Text))
    pure $ case outcome of
        Left e -> Left (T.pack (displayException e))
        Right pair@(out, err)
            | Just message <- textualStderrFailure err -> Left message
            | checkLoadOutput, loadFailed out -> Left (T.strip out)
            | otherwise -> Right pair

-- Keep the textual scratch path aligned with the normal non-JSON cell engine:
-- compiler diagnostics and runtime exceptions live on stderr; Template
-- Haskell chatter and linker warnings are harmless.  Stdout is user output,
-- except for GHCi's structural @Failed,@ line from a @:load@ command.
textualStderrFailure :: Text -> Maybe Text
textualStderrFailure rawErr
    | T.null cleaned = Nothing
    | isTemplateHaskellOutput rawErr = Nothing
    | not (null errs) = Just cleaned
    | any (`T.isInfixOf` T.toLower cleaned) runtimeFailureSignals = Just cleaned
    | otherwise = Nothing
  where
    errs = parseErrors rawErr
    cleaned =
        T.strip . T.unlines . filter (not . isLinkerNoise) . T.lines $ rawErr
    isLinkerNoise line = "ld: warning:" `T.isPrefixOf` T.strip line
    runtimeFailureSignals =
        [ "*** exception"
        , "execution timed out"
        , "interrupted"
        , "repl failed"
        , "kernel was killed"
        ]

loadFailed :: Text -> Bool
loadFailed =
    any (("failed," `T.isPrefixOf`) . T.toLower . T.stripStart) . T.lines

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
        , disposableDependencies = deps
        }

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
