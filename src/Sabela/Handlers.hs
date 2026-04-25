{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sabela.Handlers (
    -- * Reactive notebook interface
    ReactiveNotebook (..),
    setupReactive,

    -- * Initialization
    initGlobalEnv,
    initPreinstalledPackages,

    -- * Haskell session management (also used by tests)
    installAndRestart,
    setupReplProject,
    updateCellSource,
    killAllSessions,

    -- * Re-exports from submodules
    module Sabela.Handlers.Shared,
) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, void, when)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Sabela.AI.Store as AI
import qualified Sabela.AI.Types as AI
import qualified Sabela.Anthropic.Types as AI (cancel)
import Sabela.Api (RunResult (..))
import Sabela.Bridge (bridgePreamble, isTemplateHaskellOutput, widgetPreamble)
import Sabela.Deps (collectMetadata, collectMetadataFromContent, mergedMeta)
import Sabela.Errors (parseErrors)
import Sabela.Handlers.Python (
    executePythonCell,
    executePythonCells,
 )
import Sabela.Handlers.Shared
import Sabela.Model (
    Cell (..),
    CellError (..),
    CellType (..),
    Notebook (..),
    NotebookEvent (..),
    OutputItem (..),
    SessionStatus (..),
    cellLangOf,
 )
import Sabela.Output (displayPrelude, parseMimeOutputs)
import Sabela.Reactivity (
    ExecutionPlan (..),
    computeExecutionPlan,
    computeFullExecutionPlan,
    cycleErrorMsg,
    haskellCodeCells,
    redefinitionErrorMsg,
 )
import Sabela.Session (
    Session,
    SessionConfig (..),
    clearErrCallback,
    closeSession,
    ghciBackend,
    newSessionStreaming,
    readErrorBuffer,
    runBlock,
 )
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..), getAIStore)
import Sabela.State.BridgeStore (getBridgeValues, setBridgeValue)
import Sabela.State.DependencyTracker (
    getHaskellDeps,
    getHaskellExts,
    setHaskellDeps,
    setHaskellExts,
 )
import Sabela.State.Environment (Environment (..))
import Sabela.State.NotebookStore (modifyNotebook, readNotebook)
import Sabela.State.SessionManager (
    forceResetAllSessions,
    getHaskellSession,
    modifyHaskellSession,
    setHaskellSession,
 )
import Sabela.State.WidgetStore (getWidgetValues)
import qualified Sabela.Topo as Topo
import ScriptHs.Parser (CabalMeta (..), ScriptFile (..), parseScript)
import ScriptHs.Render (toGhciScript)
import ScriptHs.Run (renderCabalFile)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))

initGlobalEnv :: FilePath -> IO (Set Text)
initGlobalEnv path = do
    exists <- doesFileExist path
    if not exists
        then pure S.empty
        else do
            content <- TIO.readFile path
            pure (S.fromList (metaDeps (collectMetadataFromContent content)))

initPreinstalledPackages :: FilePath -> [String] -> IO (Set Text)
initPreinstalledPackages _ [] = pure S.empty
initPreinstalledPackages _ pkgs = pure (S.fromList (map T.pack pkgs))

data ReactiveNotebook = ReactiveNotebook
    { rnCellEdit :: Int -> Text -> IO ()
    , rnRunCell :: Int -> IO ()
    , rnRunAll :: IO ()
    , rnReset :: IO ()
    , rnRestartKernel :: IO ()
    , rnWidgetCell :: Int -> IO ()
    }

setupReactive :: App -> IO ReactiveNotebook
setupReactive app =
    pure $
        ReactiveNotebook
            { rnCellEdit = handleCellEdit app
            , rnRunCell = handleRunCell app
            , rnRunAll = handleRunAll app
            , rnReset = handleReset app
            , rnRestartKernel = handleRestartKernel app
            , rnWidgetCell = handleWidgetCell app
            }

handleCellEdit :: App -> Int -> Text -> IO ()
handleCellEdit app cid src = do
    debugLog app $ "[handler] handleCellEdit: cell " <> T.pack (show cid)
    modifyNotebook (appNotebook app) $ updateCellSource cid src
    nb <- readNotebook (appNotebook app)
    gen <- bumpGeneration app
    dispatchByLang app gen cid (cellLangOf cid nb) (executeAffected app gen cid)

updateCellSource :: Int -> Text -> Notebook -> Notebook
updateCellSource cid src nb =
    nb{nbCells = map upd (nbCells nb)}
  where
    upd c
        | cellId c == cid = c{cellSource = src, cellDirty = True}
        | otherwise = c

handleWidgetCell :: App -> Int -> IO ()
handleWidgetCell app cid = do
    debugLog app $ "[handler] handleWidgetCell: cell " <> T.pack (show cid)
    gen <- bumpGeneration app
    void $ forkIO $ executeAffected app gen cid

handleRunCell :: App -> Int -> IO ()
handleRunCell app cid = do
    debugLog app $ "[handler] handleRunCell: cell " <> T.pack (show cid)
    nb <- readNotebook (appNotebook app)
    gen <- bumpGeneration app
    dispatchByLang app gen cid (cellLangOf cid nb) $
        void $
            forkIO $
                executeSingleCell app gen cid

dispatchByLang :: App -> Int -> Int -> ST.CellLang -> IO () -> IO ()
dispatchByLang app gen _cid lang haskellAction =
    case lang of
        ST.Python -> void $ forkIO $ do
            executePythonCell app gen _cid
            whenCurrentGen app gen $ broadcast app EvExecutionDone
        ST.Haskell -> haskellAction

rerunBridgeCells :: App -> Int -> IO ()
rerunBridgeCells app gen = do
    nb <- readNotebook (appNotebook app)
    let hsCells = filter isBridgeDependent (nbCells nb)
    unless (null hsCells) $ do
        debugLog app $
            "[handler] Bridge changed, re-running "
                <> T.pack (show (length hsCells))
                <> " Haskell cells"
        loadSabelaPrelude app
        runCellList app gen hsCells

isBridgeDependent :: Cell -> Bool
isBridgeDependent c =
    cellType c == CodeCell
        && cellLang c == ST.Haskell
        && "_bridge_" `T.isInfixOf` cellSource c

handleRunAll :: App -> IO ()
handleRunAll app = do
    debugLog app "[handler] handleRunAll: fullRestart"
    gen <- bumpGeneration app
    void $ forkIO $ executeFullRestart app gen

handleReset :: App -> IO ()
handleReset app = do
    debugLog app "[handler] handleReset"
    void $ bumpGeneration app
    void $ forkIO $ killAllSessions app
    cleanupAI app True
    modifyNotebook (appNotebook app) clearAllOutputs
    broadcast app (EvSessionStatus SReset)

handleRestartKernel :: App -> IO ()
handleRestartKernel app = do
    debugLog app "[handler] handleRestartKernel"
    gen <- bumpGeneration app
    cleanupAI app False
    broadcast app (EvSessionStatus SReset)
    void $ forkIO $ executeFullRestart app gen

{- | Cleanup AI state on reset/restart.
fullReset clears conversation and reverts edits; partial only kills scratchpad.
-}
cleanupAI :: App -> Bool -> IO ()
cleanupAI app fullReset = do
    mStore <- getAIStore app
    case mStore of
        Nothing -> pure ()
        Just store -> do
            mTurn <- AI.getCurrentTurn store
            case mTurn of
                Just turn -> AI.cancel (AI.turnCancel turn)
                Nothing -> pure ()
            AI.clearScratchpad store
            when fullReset $ do
                AI.clearConversation store
                AI.revertAllPendingEdits store

clearAllOutputs :: Notebook -> Notebook
clearAllOutputs nb = nb{nbCells = map clr (nbCells nb)}
  where
    clr c = c{cellOutputs = [], cellError = Nothing, cellDirty = False}

killAllSessions :: App -> IO ()
killAllSessions app =
    forceResetAllSessions (appSessions app)

executeSingleCell :: App -> Int -> Int -> IO ()
executeSingleCell app gen cid = do
    debugLog app "[handler] executeSingleCell"
    nb <- readNotebook (appNotebook app)
    let allCode = haskellCodeCells nb
        plan = computeFullExecutionPlan allCode nb
    ok <- ensureSessionAlive app gen (collectMetadata nb)
    when ok $ executeSingleCellPlan app gen cid allCode plan
    whenCurrentGen app gen $ broadcast app EvExecutionDone

executeSingleCellPlan :: App -> Int -> Int -> [Cell] -> ExecutionPlan -> IO ()
executeSingleCellPlan app gen cid allCode plan =
    case find (\c -> cellId c == cid) allCode of
        Just cell ->
            whenCurrentGen app gen $
                if cellInSkipSet cid plan
                    then broadcastPlanErrors app plan (Just cid)
                    else runAndBroadcast app gen cell
        Nothing -> pure ()

cellInSkipSet :: Int -> ExecutionPlan -> Bool
cellInSkipSet cid plan =
    S.member cid (epCycleIds plan `S.union` M.keysSet (epRedefErrors plan))

executeFullRestart :: App -> Int -> IO ()
executeFullRestart app gen = do
    debugLog app "[handler] executeFullRestart: killing session, running all"
    whenCurrentGen app gen $ do
        nb <- readNotebook (appNotebook app)
        let allCode = haskellCodeCells nb
        killAllSessions app
        whenCurrentGen app gen $ do
            ok <- installAndRestart app gen (collectMetadata nb)
            when ok $ executeFullPlan app gen allCode nb
        whenCurrentGen app gen $
            executeNonHaskellCells app gen

executeFullPlan :: App -> Int -> [Cell] -> Notebook -> IO ()
executeFullPlan app gen allCode nb = do
    let plan = computeFullExecutionPlan allCode nb
    broadcastPlanErrors app plan Nothing
    runCellList app gen (epCellsToRun plan)

executeNonHaskellCells :: App -> Int -> IO ()
executeNonHaskellCells app gen = do
    debugLog app "[handler] executeNonHaskellCells: starting"
    whenCurrentGen app gen $ do
        debugLog app "[handler] executeNonHaskellCells: running Python cells"
        oldBridge <- getBridgeValues (appBridge app)
        executePythonCells app gen
        newBridge <- getBridgeValues (appBridge app)
        when (oldBridge /= newBridge) $ rerunBridgeCells app gen
    whenCurrentGen app gen $ broadcast app EvExecutionDone

killSession :: App -> IO ()
killSession app =
    modifyHaskellSession (appSessions app) $ \mSess -> do
        forM_ mSess $ \s ->
            void (try (ST.sbClose s) :: IO (Either SomeException ()))
        pure Nothing

ensureSessionAlive :: App -> Int -> CabalMeta -> IO Bool
ensureSessionAlive app gen metas = do
    installed <- getHaskellDeps (appDeps app)
    instExts <- getHaskellExts (appDeps app)
    mSess <- getHaskellSession (appSessions app)
    let metaMatch = depsMatch metas installed instExts (envGlobalDeps (appEnv app))
    case mSess of
        Just _ | metaMatch -> pure True
        _ -> installAndRestart app gen metas

depsMatch :: CabalMeta -> Set Text -> Set Text -> Set Text -> Bool
depsMatch metas installed instExts globalDeps =
    S.fromList (metaDeps metas) `S.isSubsetOf` (installed `S.union` globalDeps)
        && S.fromList (metaExts metas) == instExts

installAndRestart :: App -> Int -> CabalMeta -> IO Bool
installAndRestart app gen metas = do
    current <- isCurrentGen app gen
    if not current
        then pure False
        else installDepsAndStartSession app gen metas

installDepsAndStartSession :: App -> Int -> CabalMeta -> IO Bool
installDepsAndStartSession app _gen metas = do
    broadcastDepsStatus app metas
    setHaskellExts (appDeps app) (S.fromList (metaExts metas))
    let projDir = envTmpDir (appEnv app) </> "repl-project"
    setupReplProject projDir (mergedMeta (envGlobalDeps (appEnv app)) metas)
    broadcast app (EvSessionStatus SStarting)
    killSession app
    startSessionWith app projDir

broadcastDepsStatus :: App -> CabalMeta -> IO ()
broadcastDepsStatus app metas = do
    installedDeps <- getHaskellDeps (appDeps app)
    let globalDeps = envGlobalDeps (appEnv app)
        notebookDeps = S.difference (S.fromList (metaDeps metas)) globalDeps
    unless (notebookDeps `S.isSubsetOf` installedDeps) $ do
        let newDeps = S.difference notebookDeps installedDeps
        broadcast app $
            EvSessionStatus $
                if S.null newDeps then SDepsUpToDate else SUpdateDeps (S.toList newDeps)
        setHaskellDeps (appDeps app) notebookDeps

setupReplProject :: FilePath -> CabalMeta -> IO ()
setupReplProject dir meta = do
    createDirectoryIfMissing True dir
    ensureFile (dir </> "cabal.project") "packages: .\n"
    ensureFile (dir </> "Main.hs") "main :: IO ()\nmain = pure ()\n"
    writeFile (dir </> "sabela-repl.cabal") (renderCabalFile "sabela-repl" meta)

ensureFile :: FilePath -> String -> IO ()
ensureFile path content = do
    exists <- doesFileExist path
    unless exists $ writeFile path content

startSessionWith :: App -> FilePath -> IO Bool
startSessionWith app projDir = do
    debugLog app "[handler] Injecting display prelude"
    let cfg = SessionConfig{scProjectDir = projDir, scWorkDir = envWorkDir (appEnv app)}
        onLine t = unless (T.null t) $ broadcast app (EvInstallLog t)
    sessResult <-
        try (newSessionStreaming cfg onLine) :: IO (Either SomeException Session)
    case sessResult of
        Left e -> reportSessionFailure app "Session startup failed" e
        Right sess -> do
            clearErrCallback sess
            injectPrelude app sess

reportSessionFailure :: App -> Text -> SomeException -> IO Bool
reportSessionFailure app msg e = do
    debugLog app $ "[handler] " <> msg <> ": " <> T.pack (show e)
    broadcast app (EvSessionStatus SReset)
    pure False

broadcastInstallLog :: App -> Session -> IO ()
broadcastInstallLog app sess = do
    startupLog <- readErrorBuffer sess
    mapM_
        (broadcast app . EvInstallLog)
        (filter (not . T.null) (T.lines startupLog))

injectPrelude :: App -> Session -> IO Bool
injectPrelude app sess = do
    result <-
        try (runBlock sess displayPrelude) :: IO (Either SomeException (Text, Text))
    case result of
        Left e -> do
            _ <- reportSessionFailure app "Prelude injection failed" e
            threadDelay 100000
            broadcastInstallLog app sess
            void (try (closeSession sess) :: IO (Either SomeException ()))
            pure False
        Right _ -> do
            setHaskellSession (appSessions app) (Just (ghciBackend sess))
            broadcast app (EvSessionStatus SReady)
            pure True

loadSabelaPrelude :: App -> IO ()
loadSabelaPrelude app = do
    mSess <- getHaskellSession (appSessions app)
    forM_ mSess $ \backend -> do
        result <- try (ST.sbRunBlock backend displayPrelude)
        case result of
            Left (e :: SomeException) ->
                handleKernelCrash app ("Kernel crashed during prelude: " <> T.pack (show e))
            Right _ -> pure ()

runAndBroadcast :: App -> Int -> Cell -> IO ()
runAndBroadcast app gen cell = do
    broadcast app (EvCellUpdating (cellId cell))
    loadSabelaPrelude app
    (result, errs) <- execCell app cell
    whenCurrentGen app gen $
        updateAndBroadcast
            app
            (\nb -> nb{nbCells = map (applyResult result) (nbCells nb)})
            (EvCellResult (rrCellId result) (rrOutputs result) (rrError result) errs)

execCell :: App -> Cell -> IO (RunResult, [CellError])
execCell app cell = do
    mSess <- getHaskellSession (appSessions app)
    case mSess of
        Nothing -> pure (RunResult (cellId cell) [] (Just "No GHCi session"), [])
        Just backend -> execCellWith app cell backend

execCellWith :: App -> Cell -> ST.SessionBackend -> IO (RunResult, [CellError])
execCellWith app cell backend = do
    ghci <- buildGhciScript app cell
    debugLog app $
        T.pack $
            "[handler] Cell " ++ show (cellId cell) ++ ":\n" ++ T.unpack ghci
    onLine <- mkStreamingCallback app (cellId cell)
    result <- try (ST.sbRunBlockStreaming backend ghci onLine)
    case result of
        Left (e :: SomeException) -> do
            handleKernelCrash app ("Kernel crashed: " <> T.pack (show e))
            pure
                (RunResult (cellId cell) [] (Just ("Kernel crashed: " <> T.pack (show e))), [])
        Right (rawOut, rawErr) -> do
            storeBridgeExports app rawOut
            (rr, errs) <- parseCellResult (cellId cell) rawOut rawErr
            when (isReplCrash rawErr) $
                handleKernelCrash app rawErr
            pure (rr, errs)

handleKernelCrash :: App -> Text -> IO ()
handleKernelCrash app msg = do
    debugLog app $ "[handler] Kernel crash detected: " <> msg
    setHaskellSession (appSessions app) Nothing
    broadcast app (EvSessionStatus SCrashed)

isReplCrash :: Text -> Bool
isReplCrash err = "repl failed" `T.isInfixOf` err

buildGhciScript :: App -> Cell -> IO Text
buildGhciScript app cell = do
    cellWidgets <- getWidgetValues (appWidgets app) (cellId cell)
    bridgeVals <- getBridgeValues (appBridge app)
    let preamble = widgetPreamble (cellId cell) cellWidgets <> bridgePreamble bridgeVals
        sf = scriptLines (parseScript (cellSource cell))
    pure (preamble <> toGhciScript sf)

storeBridgeExports :: App -> Text -> IO ()
storeBridgeExports app rawOut = do
    let (exports, _) = partitionExports (parseMimeOutputs rawOut)
    forM_ exports $ \(name, val) ->
        setBridgeValue (appBridge app) name (T.strip val)

parseCellResult :: Int -> Text -> Text -> IO (RunResult, [CellError])
parseCellResult cid rawOut rawErr = do
    let (_, normalItems) = partitionExports (parseMimeOutputs rawOut)
        outputs = [OutputItem m b | (m, b) <- normalItems, not (T.null (T.strip b))]
        errs = parseErrors rawErr
        actualErr = classifyError errs rawErr
    pure (RunResult cid outputs actualErr, errs)

classifyError :: [CellError] -> Text -> Maybe Text
classifyError errs rawErr
    | null errs && isTemplateHaskellOutput rawErr = Nothing
    | T.null rawErr = Nothing
    | otherwise = Just rawErr

executeAffected :: App -> Int -> Int -> IO ()
executeAffected app gen editedCid = do
    debugLog app $
        "[handler] executeAffected: editedCid=" <> T.pack (show editedCid)
    nb <- readNotebook (appNotebook app)
    sessionReady <- isSessionUpToDate app nb
    if sessionReady
        then executeIncrementalPlan app gen editedCid nb
        else executeFullRestartPlan app gen nb

isSessionUpToDate :: App -> Notebook -> IO Bool
isSessionUpToDate app nb = do
    installed <- getHaskellDeps (appDeps app)
    instExts <- getHaskellExts (appDeps app)
    mSess <- getHaskellSession (appSessions app)
    let needed = collectMetadata nb
        match = depsMatch needed installed instExts (envGlobalDeps (appEnv app))
    case mSess of
        Just _ | match -> pure True
        _ -> pure False

executeIncrementalPlan :: App -> Int -> Int -> Notebook -> IO ()
executeIncrementalPlan app gen editedCid nb = do
    let allCode = haskellCodeCells nb
        plan = computeExecutionPlan editedCid allCode nb
    logExecutionPlan app allCode plan
    broadcastPlanErrors app plan Nothing
    runCellList app gen (epCellsToRun plan)
    whenCurrentGen app gen $ broadcast app EvExecutionDone

executeFullRestartPlan :: App -> Int -> Notebook -> IO ()
executeFullRestartPlan app gen nb = do
    debugLog app "[handler] No session or deps changed -> full restart"
    let allCode = haskellCodeCells nb
    ok <- installAndRestart app gen (collectMetadata nb)
    when ok $ executeFullPlan app gen allCode nb
    whenCurrentGen app gen $ broadcast app EvExecutionDone

logExecutionPlan :: App -> [Cell] -> ExecutionPlan -> IO ()
logExecutionPlan app allCode plan = do
    debugLog app $
        T.pack $
            "[handler] All code cells: " ++ show (map cellId allCode)
    debugLog app $
        T.pack $
            "[handler] WILL RUN: " ++ show (map cellId (epCellsToRun plan))
    debugLog app $
        T.pack $
            "[handler] Cycle cells: " ++ show (S.toList (epCycleIds plan))
    debugLog app $
        T.pack $
            "[handler] Redef cells: " ++ show (M.keys (epRedefErrors plan))
    forM_ allCode $ \c -> logCellDeps app c

logCellDeps :: App -> Cell -> IO ()
logCellDeps app c = do
    let (defs, uses) = Topo.cellNames (cellSource c)
        usesPreview = take 10 (S.toList uses) ++ ["..." | S.size uses > 10]
    debugLog app $
        T.pack $
            "[handler]   cell "
                ++ show (cellId c)
                ++ " defines="
                ++ show (S.toList defs)
                ++ " uses="
                ++ show usesPreview

broadcastPlanErrors :: App -> ExecutionPlan -> Maybe Int -> IO ()
broadcastPlanErrors app plan filterCid = do
    broadcastRedefErrors app plan filterCid
    broadcastCycleErrors app plan filterCid

broadcastRedefErrors :: App -> ExecutionPlan -> Maybe Int -> IO ()
broadcastRedefErrors app plan filterCid = do
    let redefMap = filterByCell filterCid (epRedefErrors plan)
    forM_ (M.toList redefMap) $ \(cid, names) ->
        broadcastCellError
            app
            cid
            (redefinitionErrorMsg (epDefMap plan) (epCellPositions plan) cid names)

broadcastCycleErrors :: App -> ExecutionPlan -> Maybe Int -> IO ()
broadcastCycleErrors app plan filterCid = do
    let cycleIds = filterCycleIds filterCid (epCycleIds plan)
    unless (S.null cycleIds) $ do
        nb <- readNotebook (appNotebook app)
        let cells = nbCells nb
            msg =
                cycleErrorMsg
                    (epCellPositions plan)
                    cycleIds
                    cells
                    (epDefMap plan)
        forM_ (S.toList cycleIds) $ \cid -> broadcastCellError app cid msg

filterByCell :: Maybe Int -> M.Map Int a -> M.Map Int a
filterByCell Nothing m = m
filterByCell (Just cid) m = M.filterWithKey (\k _ -> k == cid) m

filterCycleIds :: Maybe Int -> S.Set Int -> S.Set Int
filterCycleIds Nothing s = s
filterCycleIds (Just cid) s = S.intersection s (S.singleton cid)

runCellList :: App -> Int -> [Cell] -> IO ()
runCellList app gen cells =
    forM_ cells $ \cell ->
        whenCurrentGen app gen $ runAndBroadcast app gen cell
