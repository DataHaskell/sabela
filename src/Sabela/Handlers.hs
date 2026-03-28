{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sabela.Handlers where

import Control.Concurrent (forkIO, modifyMVar_, readMVar, threadDelay)
import Control.Concurrent.MVar (withMVar)
import Control.Concurrent.STM (atomically, writeTChan)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, void, when)
import Data.IORef (atomicModifyIORef', readIORef, writeIORef)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Sabela.Api (RunResult (..))
import Sabela.Errors (parseErrors)
import Sabela.Model (
    AppState (..),
    Cell (..),
    CellError (..),
    CellType (..),
    Notebook (nbCells),
    NotebookEvent (..),
    OutputItem (..),
    SessionStatus (..),
 )
import Sabela.Output (displayPrelude, parseMimeOutputs)
import Sabela.Session (
    Session,
    SessionConfig (..),
    closeSession,
    newSession,
    readErrorBuffer,
    runBlock,
 )
import qualified Sabela.Topo as Topo
import ScriptHs.Markdown (Segment (..), parseMarkdown)
import ScriptHs.Parser (
    CabalMeta (..),
    ScriptFile (..),
    mergeMetas,
    parseScript,
 )
import ScriptHs.Render (toGhciScript)
import ScriptHs.Run (renderCabalFile)
import System.Directory (
    createDirectoryIfMissing,
    doesFileExist,
 )
import System.FilePath ((</>))
import System.IO (stderr)

initGlobalEnv :: FilePath -> IO (Set Text)
initGlobalEnv globalMdPath = do
    exists <- doesFileExist globalMdPath
    if not exists
        then pure S.empty
        else do
            currentContent <- TIO.readFile globalMdPath
            let meta = collectMetadataFromContent currentContent
            pure (S.fromList (metaDeps meta))

initPreinstalledPackages :: FilePath -> [String] -> IO (Set Text)
initPreinstalledPackages _ [] = pure S.empty
initPreinstalledPackages _ pkgs = pure (S.fromList (map T.pack pkgs))

collectMetadataFromContent :: Text -> CabalMeta
collectMetadataFromContent content =
    let segs = parseMarkdown content
        codeSrcs = [src | CodeBlock _ src _ <- segs]
     in mergeMetas (map (scriptMeta . parseScript) codeSrcs)

setupReplProject :: FilePath -> CabalMeta -> IO ()
setupReplProject dir meta = do
    createDirectoryIfMissing True dir
    cabalProjectExists <- doesFileExist (dir </> "cabal.project")
    unless cabalProjectExists $ writeFile (dir </> "cabal.project") "packages: .\n"
    mainHsExists <- doesFileExist (dir </> "Main.hs")
    unless mainHsExists $
        writeFile (dir </> "Main.hs") "main :: IO ()\nmain = pure ()\n"
    writeFile (dir </> "sabela-repl.cabal") (renderCabalFile "sabela-repl" meta)

mergedMeta :: AppState -> CabalMeta -> CabalMeta
mergedMeta st meta =
    meta{metaDeps = S.toList (S.fromList (metaDeps meta) <> stGlobalDeps st)}

data ReactiveNotebook = ReactiveNotebook
    { rnCellEdit :: Int -> Text -> IO ()
    , rnRunCell :: Int -> IO ()
    , rnRunAll :: IO ()
    , rnReset :: IO ()
    , rnWidgetCell :: Int -> IO ()
    }

setupReactive :: AppState -> IO ReactiveNotebook
setupReactive st =
    pure $
        ReactiveNotebook
            { rnCellEdit = handleCellEdit st
            , rnRunCell = handleRunCell st
            , rnRunAll = handleRunAll st
            , rnReset = handleReset st
            , rnWidgetCell = handleWidgetCell st
            }

handleCellEdit :: AppState -> Int -> Text -> IO ()
handleCellEdit st cid src = do
    TIO.hPutStrLn stderr $ "[handler] handleCellEdit: cell " <> T.pack (show cid)
    modifyMVar_ (stNotebook st) $ \nb ->
        pure nb{nbCells = map upd (nbCells nb)}
    gen <- bumpGeneration st
    executeAffected st gen cid
  where
    upd c
        | cellId c == cid = c{cellSource = src, cellDirty = True}
        | otherwise = c

handleWidgetCell :: AppState -> Int -> IO ()
handleWidgetCell st cid = do
    TIO.hPutStrLn stderr $ "[handler] handleWidgetCell: cell " <> T.pack (show cid)
    gen <- bumpGeneration st
    void $ forkIO $ executeAffected st gen cid

handleRunCell :: AppState -> Int -> IO ()
handleRunCell st cid = do
    TIO.hPutStrLn stderr $ "[handler] handleRunCell: cell " <> T.pack (show cid)
    gen <- bumpGeneration st
    void $ forkIO $ executeSingleCell st gen cid

handleRunAll :: AppState -> IO ()
handleRunAll st = do
    TIO.hPutStrLn stderr "[handler] handleRunAll: fullRestart"
    gen <- bumpGeneration st
    void $ forkIO $ executeFullRestart st gen

handleReset :: AppState -> IO ()
handleReset st = do
    TIO.hPutStrLn stderr "[handler] handleReset"
    void $ bumpGeneration st
    killSession st
    modifyMVar_ (stNotebook st) $ \nb ->
        pure nb{nbCells = map clr (nbCells nb)}
    broadcast st (EvSessionStatus SReset)
  where
    clr c =
        c
            { cellOutputs = []
            , cellError = Nothing
            , cellDirty = False
            }

executeSingleCell :: AppState -> Int -> Int -> IO ()
executeSingleCell st gen cid = do
    TIO.hPutStrLn stderr "[handler] executeSingleCell"
    nb <- readMVar (stNotebook st)
    let metas = collectMetadata nb
        allCode = filter (\c -> cellType c == CodeCell) (nbCells nb)
        codePosMap = M.fromList (zip (map cellId (nbCells nb)) [1 ..])
        (defMap, _) = Topo.buildDefMap allCode
        (topoResult, redefMap) = Topo.computeTopoOrder allCode
        skipIds = Topo.trCycleIds topoResult `S.union` M.keysSet redefMap
    ok <- ensureSessionAlive st gen metas
    when ok $
        case filter (\c -> cellId c == cid) allCode of
            (cell : _) -> do
                still <- isCurrentGen st gen
                when still $
                    if S.member cid skipIds
                        then do
                            broadcastRedefinitionErrors
                                st
                                defMap
                                (M.filterWithKey (\k _ -> k == cid) redefMap)
                                codePosMap
                            broadcastCycleErrors
                                st
                                (S.intersection (Topo.trCycleIds topoResult) (S.singleton cid))
                                codePosMap
                        else runAndBroadcast st gen cell
                still' <- isCurrentGen st gen
                when still' $ broadcast st EvExecutionDone
            [] -> broadcast st EvExecutionDone

executeFullRestart :: AppState -> Int -> IO ()
executeFullRestart st gen = do
    putStrLn "[handler] executeFullRestart: killing session, running all"
    nb <- readMVar (stNotebook st)
    let allCode = filter (\c -> cellType c == CodeCell) (nbCells nb)
        needed = collectMetadata nb
    killSession st
    ok <- installAndRestart st gen needed
    when ok $ do
        let (defMap, _) = Topo.buildDefMap allCode
            codePosMap = M.fromList (zip (map cellId (nbCells nb)) [1 ..])
            (topoResult, redefMap) = Topo.computeTopoOrder allCode
        broadcastRedefinitionErrors st defMap redefMap codePosMap
        broadcastCycleErrors st (Topo.trCycleIds topoResult) codePosMap
        let skipIds = Topo.trCycleIds topoResult `S.union` M.keysSet redefMap
            toRun = filter (\c -> not (S.member (cellId c) skipIds)) (Topo.trOrdered topoResult)
        runCellList st gen toRun

bumpGeneration :: AppState -> IO Int
bumpGeneration st = atomicModifyIORef' (stGeneration st) (\g -> let g' = g + 1 in (g', g'))

broadcast :: AppState -> NotebookEvent -> IO ()
broadcast st ev = atomically $ writeTChan (stBroadcast st) ev

killSession :: AppState -> IO ()
killSession st =
    modifyMVar_ (stSession st) $ \mSess -> do
        case mSess of
            Just s -> void (try (closeSession s) :: IO (Either SomeException ()))
            Nothing -> pure ()
        pure Nothing

collectMetadata :: Notebook -> CabalMeta
collectMetadata nb =
    let
        allCode = filter ((== CodeCell) . cellType) (nbCells nb)
     in
        mergeMetas ([(scriptMeta . parseScript) (cellSource c) | c <- allCode])

ensureSessionAlive :: AppState -> Int -> CabalMeta -> IO Bool
ensureSessionAlive st gen metas = do
    installed <- readIORef (stInstalledDeps st)
    instExts <- readIORef (stInstalledExts st)
    mSess <- readMVar (stSession st)
    let globalDeps = stGlobalDeps st
        allMetaApplicable =
            S.fromList (metaDeps metas) `S.isSubsetOf` (installed `S.union` globalDeps)
                && S.fromList (metaExts metas) == instExts
    case mSess of
        Just _ | allMetaApplicable -> pure True
        _ -> installAndRestart st gen metas

installAndRestart :: AppState -> Int -> CabalMeta -> IO Bool
installAndRestart st gen metas = do
    stillInSameGeneration <- isCurrentGen st gen
    if not stillInSameGeneration
        then pure False
        else do
            installedDeps <- readIORef (stInstalledDeps st)
            let globalDeps = stGlobalDeps st
                requiredDeps = S.fromList (metaDeps metas)
                notebookDeps = S.difference requiredDeps globalDeps
                depsUpToDate = notebookDeps `S.isSubsetOf` installedDeps
            unless depsUpToDate $ do
                let newDeps = S.difference notebookDeps installedDeps
                broadcast st $
                    EvSessionStatus $
                        if S.null newDeps then SDepsUpToDate else SUpdateDeps (S.toList newDeps)
                writeIORef (stInstalledDeps st) notebookDeps
            writeIORef (stInstalledExts st) (S.fromList (metaExts metas))
            let projDir = stTmpDir st </> "repl-project"
            setupReplProject projDir (mergedMeta st metas)
            broadcast st (EvSessionStatus SStarting)
            killSession st
            startSessionWith st projDir

startSessionWith :: AppState -> FilePath -> IO Bool
startSessionWith st projDir = do
    let cfg = SessionConfig{scProjectDir = projDir, scWorkDir = stWorkDir st}
    TIO.hPutStrLn stderr "[handler] Injecting display prelude"
    sessResult <- try (newSession cfg) :: IO (Either SomeException Session)
    case sessResult of
        Left e -> do
            TIO.hPutStrLn stderr $ "[handler] Session startup failed: " <> T.pack (show e)
            broadcast st (EvSessionStatus SReset)
            pure False
        Right sess -> do
            startupLog <- readErrorBuffer sess
            mapM_ (broadcast st . EvInstallLog) (filter (not . T.null) (T.lines startupLog))
            preludeResult <-
                try (runBlock sess displayPrelude) :: IO (Either SomeException (Text, Text))
            case preludeResult of
                Left e -> do
                    TIO.hPutStrLn stderr $ "[handler] Prelude injection failed: " <> T.pack (show e)
                    threadDelay 100000
                    errLog <- readErrorBuffer sess
                    mapM_ (broadcast st . EvInstallLog) (filter (not . T.null) (T.lines errLog))
                    void (try (closeSession sess) :: IO (Either SomeException ()))
                    broadcast st (EvSessionStatus SReset)
                    pure False
                Right _ -> do
                    modifyMVar_ (stSession st) (\_ -> pure (Just sess))
                    broadcast st (EvSessionStatus SReady)
                    pure True

loadSabelaPrelude :: AppState -> IO ()
loadSabelaPrelude st = do
    mSess <- readMVar (stSession st)
    case mSess of
        Just sess -> void (runBlock sess displayPrelude)
        Nothing -> pure ()

isCurrentGen :: AppState -> Int -> IO Bool
isCurrentGen st gen = (== gen) <$> readIORef (stGeneration st)

runAndBroadcast :: AppState -> Int -> Cell -> IO ()
runAndBroadcast st gen cell = do
    broadcast st (EvCellUpdating (cellId cell))
    loadSabelaPrelude st
    (result, errs) <- execCell st cell
    still <- isCurrentGen st gen
    when still $ do
        modifyMVar_ (stNotebook st) $ \nb ->
            pure nb{nbCells = map (applyResult result) (nbCells nb)}
        broadcast
            st
            ( EvCellResult
                (rrCellId result)
                (rrOutputs result)
                (rrError result)
                errs
            )

widgetPreamble :: Int -> M.Map Text Text -> Text
widgetPreamble cid vals =
    let pairs = show [(T.unpack k, T.unpack v) | (k, v) <- M.toList vals]
     in T.unlines
            [ "writeIORef _sabelaWidgetRef " <> T.pack pairs
            , "writeIORef _sabelaCellIdRef " <> T.pack (show (show cid))
            ]

execCell :: AppState -> Cell -> IO (RunResult, [CellError])
execCell st cell = do
    mSess <- readMVar (stSession st)
    case mSess of
        Nothing ->
            pure (RunResult (cellId cell) [] (Just "No GHCi session"), [])
        Just sess -> do
            cellWidgets <-
                withMVar (stWidgetValues st) $
                    pure . M.findWithDefault M.empty (cellId cell)
            let preamble = widgetPreamble (cellId cell) cellWidgets
                sf = scriptLines (parseScript (cellSource cell))
                ghci = preamble <> toGhciScript sf
            TIO.hPutStrLn stderr $
                T.pack $
                    "[handler] Cell " ++ show (cellId cell) ++ ":\n" ++ T.unpack ghci
            (rawOut, rawErr) <- runBlock sess ghci
            let items = parseMimeOutputs rawOut
                outputs = [OutputItem m b | (m, b) <- items, not (T.null (T.strip b))]
                errs = parseErrors rawErr
            TIO.hPutStrLn stderr $
                T.pack $
                    "[handler] → outputs="
                        ++ show (length outputs)
                        ++ " errors="
                        ++ show (length errs)
            pure
                ( RunResult
                    { rrCellId = cellId cell
                    , rrOutputs = outputs
                    , rrError = if T.null rawErr then Nothing else Just rawErr
                    }
                , errs
                )

applyResult :: RunResult -> Cell -> Cell
applyResult r c
    | cellId c == rrCellId r =
        c
            { cellOutputs = rrOutputs r
            , cellError = rrError r
            , cellDirty = False
            }
    | otherwise = c

executeAffected :: AppState -> Int -> Int -> IO ()
executeAffected st gen editedCid = do
    TIO.hPutStrLn stderr $
        T.pack $
            "[handler] executeAffected: editedCid="
                ++ show editedCid
    nb <- readMVar (stNotebook st)
    let allCode = filter (\c -> cellType c == CodeCell) (nbCells nb)
        needed = collectMetadata nb

    installed <- readIORef (stInstalledDeps st)
    instExts <- readIORef (stInstalledExts st)
    mSess <- readMVar (stSession st)

    let neededD = S.fromList (metaDeps needed)
        neededE = S.fromList (metaExts needed)
        globalDeps = stGlobalDeps st
        depsOk = neededD `S.isSubsetOf` (installed `S.union` globalDeps)
        extsOk = neededE == instExts
        (defMap, _) = Topo.buildDefMap allCode
        codePosMap = M.fromList (zip (map cellId (nbCells nb)) [1 ..])

    case (mSess, depsOk && extsOk) of
        (Just _, True) -> do
            let (topoResult, redefMap) = Topo.selectAffectedTopo editedCid allCode
                allIds = map cellId allCode
                runIds = map cellId (Topo.trOrdered topoResult)

            TIO.hPutStrLn stderr $ T.pack $ "[handler] All code cells: " ++ show allIds
            TIO.hPutStrLn stderr $
                T.pack $
                    "[handler] WILL RUN (affected, topo order): " ++ show runIds
            TIO.hPutStrLn stderr $
                T.pack $
                    "[handler] Cycle cells: " ++ show (S.toList (Topo.trCycleIds topoResult))
            TIO.hPutStrLn stderr $
                T.pack $
                    "[handler] Redef cells: " ++ show (M.keys redefMap)

            forM_ allCode $ \c -> do
                let (defs, uses) = Topo.cellNames (cellSource c)
                TIO.hPutStrLn stderr $
                    T.pack $
                        "[handler]   cell "
                            ++ show (cellId c)
                            ++ " defines="
                            ++ show (S.toList defs)
                            ++ " uses="
                            ++ show (take 10 (S.toList uses))
                            ++ (if S.size uses > 10 then "..." else "")

            broadcastRedefinitionErrors st defMap redefMap codePosMap
            broadcastCycleErrors st (Topo.trCycleIds topoResult) codePosMap
            let skipIds = Topo.trCycleIds topoResult `S.union` M.keysSet redefMap
                toRun = filter (\c -> not (S.member (cellId c) skipIds)) (Topo.trOrdered topoResult)
            runCellList st gen toRun

        -- ── Deps changed → full restart ──
        (_, False) -> do
            putStrLn "[handler] Deps/exts changed → full restart"
            ok <- installAndRestart st gen needed
            when ok $ do
                let (topoResult, redefMap) = Topo.computeTopoOrder allCode
                broadcastRedefinitionErrors st defMap redefMap codePosMap
                broadcastCycleErrors st (Topo.trCycleIds topoResult) codePosMap
                let skipIds = Topo.trCycleIds topoResult `S.union` M.keysSet redefMap
                    toRun = filter (\c -> not (S.member (cellId c) skipIds)) (Topo.trOrdered topoResult)
                runCellList st gen toRun

        -- ── No session → create + run all ──
        (Nothing, True) -> do
            putStrLn "[handler] No session → starting fresh, running all"
            ok <- installAndRestart st gen needed
            when ok $ do
                let (topoResult, redefMap) = Topo.computeTopoOrder allCode
                broadcastRedefinitionErrors st defMap redefMap codePosMap
                broadcastCycleErrors st (Topo.trCycleIds topoResult) codePosMap
                let skipIds = Topo.trCycleIds topoResult `S.union` M.keysSet redefMap
                    toRun = filter (\c -> not (S.member (cellId c) skipIds)) (Topo.trOrdered topoResult)
                runCellList st gen toRun

broadcastCellError :: AppState -> Int -> Text -> IO ()
broadcastCellError st cid msg = do
    let err = CellError{ceLine = Nothing, ceCol = Nothing, ceMessage = msg}
    modifyMVar_ (stNotebook st) $ \nb ->
        pure nb{nbCells = map (clearCell cid msg) (nbCells nb)}
    broadcast st (EvCellResult cid [] (Just msg) [err])
  where
    clearCell targetCid errMsg c
        | cellId c == targetCid =
            c{cellOutputs = [], cellError = Just errMsg, cellDirty = False}
        | otherwise = c

broadcastRedefinitionErrors ::
    AppState -> M.Map Text Int -> M.Map Int [Text] -> M.Map Int Int -> IO ()
broadcastRedefinitionErrors st defMap redefMap codePosMap =
    forM_ (M.toList redefMap) $ \(cid, names) -> do
        let msgs =
                [ "'"
                    <> name
                    <> "' is already defined in cell "
                    <> T.pack (show (M.findWithDefault origCid origCid codePosMap))
                    <> " (which takes precedence)"
                | name <- names
                , Just origCid <- [M.lookup name defMap]
                ]
            combined =
                "Duplicate definition"
                    <> (if length names > 1 then "s" else "")
                    <> ": "
                    <> T.intercalate "; " msgs
                    <> ". Remove the duplicate to resolve this conflict."
        broadcastCellError st cid combined

broadcastCycleErrors :: AppState -> S.Set Int -> M.Map Int Int -> IO ()
broadcastCycleErrors st cycleIds codePosMap
    | S.null cycleIds = pure ()
    | otherwise = do
        let cids = S.toList cycleIds
            cycleMsg =
                T.intercalate
                    ", "
                    (map (\c -> T.pack (show (M.findWithDefault c c codePosMap))) cids)
            msg =
                "This cell is part of a circular dependency and cannot be executed."
                    <> " Cells in the cycle: ["
                    <> cycleMsg
                    <> "]."
        forM_ cids $ \cid -> broadcastCellError st cid msg

runCellList :: AppState -> Int -> [Cell] -> IO ()
runCellList st gen cells = do
    forM_ cells $ \cell -> do
        still <- isCurrentGen st gen
        when still $ runAndBroadcast st gen cell
    still <- isCurrentGen st gen
    when still $ broadcast st EvExecutionDone
