{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Sabela.Handlers where

import Control.Concurrent (forkIO, modifyMVar_, readMVar)
import Control.Concurrent.MVar (withMVar)
import Control.Concurrent.STM (atomically, writeTChan)
import Control.Exception (IOException, SomeException, catch, try)
import Control.Monad (forM_, unless, void, when)
import qualified Data.ByteString as BS
import Data.FileEmbed (embedFile)
import Data.IORef (atomicModifyIORef', readIORef, writeIORef)
import Data.List (sort)
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
import Sabela.Output (parseMimeOutputs)
import Sabela.Session (SessionConfig (..), closeSession, newSession, runBlock)
import qualified Sabela.Topo as Topo
import ScriptHs.Markdown (Segment (..), parseMarkdown)
import ScriptHs.Parser (
    CabalMeta (..),
    ScriptFile (..),
    mergeMetas,
    parseScript,
 )
import ScriptHs.Render (toGhciScript)
import ScriptHs.Run (resolveDeps)
import System.Directory (
    createDirectoryIfMissing,
    doesFileExist,
    getTemporaryDirectory,
 )
import System.FilePath (takeDirectory, (</>))
import System.IO (stderr)

initGlobalEnv :: FilePath -> IO (Maybe FilePath, Set Text)
initGlobalEnv globalMdPath = do
    exists <- doesFileExist globalMdPath
    if not exists
        then pure (Nothing, S.empty)
        else do
            let dir = takeDirectory globalMdPath
                envPath = dir </> ".ghc.environment"
                hashPath = dir </> "global.hash"
            currentContent <- TIO.readFile globalMdPath
            storedContent <-
                (Just <$> TIO.readFile hashPath)
                    `catch` (\(_ :: IOException) -> pure Nothing)
            let meta = collectMetadataFromContent currentContent
                deps = metaDeps meta
            when (Just currentContent /= storedContent) $ do
                createDirectoryIfMissing True dir
                TIO.hPutStrLn stderr "[sabela] Installing global dependencies..."
                unless (null deps) $ resolveDeps envPath deps
                TIO.writeFile hashPath currentContent
            envExists <- doesFileExist envPath
            pure
                ( if envExists && not (null deps) then Just envPath else Nothing
                , S.fromList deps
                )

initPreinstalledPackages ::
    FilePath -> [String] -> IO (Maybe FilePath, Set Text)
initPreinstalledPackages _ [] = pure (Nothing, S.empty)
initPreinstalledPackages sabelaDir pkgs = do
    createDirectoryIfMissing True sabelaDir
    let envPath = sabelaDir </> "preinstalled.ghc.environment"
        hashPath = sabelaDir </> "preinstalled.hash"
        hashKey = unlines (sort pkgs)
        pkgsT = map T.pack pkgs
    stored <-
        (Just <$> readFile hashPath)
            `catch` (\(_ :: IOException) -> pure Nothing)
    when (Just hashKey /= stored) $ do
        TIO.hPutStrLn stderr "[sabela] Installing preinstalled packages..."
        resolveDeps envPath pkgsT
        writeFile hashPath hashKey
    envExists <- doesFileExist envPath
    pure (if envExists then Just envPath else Nothing, S.fromList pkgsT)

collectMetadataFromContent :: Text -> CabalMeta
collectMetadataFromContent content =
    let segs = parseMarkdown content
        codeSrcs = [src | CodeBlock _ src _ <- segs]
     in mergeMetas (map (scriptMeta . parseScript) codeSrcs)

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
                            broadcastRedefinitionErrors st (M.filterWithKey (\k _ -> k == cid) redefMap)
                            broadcastCycleErrors
                                st
                                (S.intersection (Topo.trCycleIds topoResult) (S.singleton cid))
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
        let (topoResult, redefMap) = Topo.computeTopoOrder allCode
        broadcastRedefinitionErrors st redefMap
        broadcastCycleErrors st (Topo.trCycleIds topoResult)
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
    -- If we restarted after some changes (not in the notebook generation)
    -- we should see if there are new dependencies and reinstall them.
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
                if S.null notebookDeps
                    then do
                        writeIORef (stEnvFile st) Nothing
                        writeIORef (stInstalledDeps st) S.empty
                    else do
                        let envPath = stTmpDir st </> ".ghc.environment"
                        TIO.hPutStrLn stderr $
                            "[sabela] cabal install --lib " <> T.unwords (S.toList notebookDeps)
                        resolveDeps envPath (S.toList notebookDeps)
                        writeIORef (stEnvFile st) (Just envPath)
                        writeIORef (stInstalledDeps st) notebookDeps
            writeIORef (stInstalledExts st) (S.fromList (metaExts metas))
            broadcast st (EvSessionStatus SStarting)
            killSession st
            startSessionWith st (metaDeps metas) (metaExts metas)
            pure True

startSessionWith :: AppState -> [Text] -> [Text] -> IO ()
startSessionWith st deps exts = do
    perNotebookEnv <- readIORef (stEnvFile st)
    let envFiles = stGlobalEnvFiles st ++ maybeToList perNotebookEnv
        cfg =
            SessionConfig
                { scDeps = deps
                , scExts = exts
                , scGhcOptions = []
                , scEnvFiles = envFiles
                }
    TIO.hPutStrLn stderr "[handler] Injecting display prelude"
    sess <- newSession cfg
    prelude <- makeDisplayPrelude
    _ <- runBlock sess prelude
    modifyMVar_ (stSession st) (\_ -> pure (Just sess))
    broadcast st (EvSessionStatus SReady)

loadSabelaPrelude :: AppState -> IO ()
loadSabelaPrelude st = do
    mSess <- readMVar (stSession st)
    case mSess of
        Just sess -> do
            prelude <- makeDisplayPrelude
            void (runBlock sess prelude)
        Nothing -> pure ()

displayHsContent :: BS.ByteString
displayHsContent = $(embedFile "display/Sabela/Display.hs")

-- | Build the two-line GHCi script that loads and imports 'Sabela.Display'.
makeDisplayPrelude :: IO Text
makeDisplayPrelude = do
    displayDir <- findDisplayDir
    let displayFile = T.pack (displayDir </> "Sabela" </> "Display.hs")
    pure $ ":load " <> displayFile <> "\nimport Sabela.Display\n"

{- | Find the directory containing @Sabela/Display.hs@.
Writes the embedded @Display.hs@ to a temp directory so this always
succeeds regardless of the working directory.
-}
findDisplayDir :: IO FilePath
findDisplayDir = do
    tmpDir <- getTemporaryDirectory
    let displayDir = tmpDir </> "sabela-display"
        displayFile = displayDir </> "Sabela" </> "Display.hs"
    exists <- doesFileExist displayFile
    unless exists $ do
        createDirectoryIfMissing True (displayDir </> "Sabela")
        BS.writeFile displayFile displayHsContent
    return displayDir

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

            broadcastRedefinitionErrors st redefMap
            broadcastCycleErrors st (Topo.trCycleIds topoResult)
            let skipIds = Topo.trCycleIds topoResult `S.union` M.keysSet redefMap
                toRun = filter (\c -> not (S.member (cellId c) skipIds)) (Topo.trOrdered topoResult)
            runCellList st gen toRun

        -- ── Deps changed → full restart ──
        (_, False) -> do
            putStrLn "[handler] Deps/exts changed → full restart"
            ok <- installAndRestart st gen needed
            when ok $ do
                let (topoResult, redefMap) = Topo.computeTopoOrder allCode
                broadcastRedefinitionErrors st redefMap
                broadcastCycleErrors st (Topo.trCycleIds topoResult)
                let skipIds = Topo.trCycleIds topoResult `S.union` M.keysSet redefMap
                    toRun = filter (\c -> not (S.member (cellId c) skipIds)) (Topo.trOrdered topoResult)
                runCellList st gen toRun

        -- ── No session → create + run all ──
        (Nothing, True) -> do
            putStrLn "[handler] No session → starting fresh, running all"
            ok <- installAndRestart st gen needed
            when ok $ do
                let (topoResult, redefMap) = Topo.computeTopoOrder allCode
                broadcastRedefinitionErrors st redefMap
                broadcastCycleErrors st (Topo.trCycleIds topoResult)
                let skipIds = Topo.trCycleIds topoResult `S.union` M.keysSet redefMap
                    toRun = filter (\c -> not (S.member (cellId c) skipIds)) (Topo.trOrdered topoResult)
                runCellList st gen toRun

broadcastCellError :: AppState -> Int -> Text -> IO ()
broadcastCellError st cid msg = do
    let err = CellError{ceLine = Nothing, ceCol = Nothing, ceMessage = msg}
    broadcast st (EvCellResult cid [] (Just msg) [err])

broadcastRedefinitionErrors :: AppState -> M.Map Int [Text] -> IO ()
broadcastRedefinitionErrors st redefMap =
    forM_ (M.toList redefMap) $ \(cid, names) ->
        forM_ names $ \name ->
            broadcastCellError st cid $
                "Cell defines '"
                    <> name
                    <> "', which is already defined by another cell."
                    <> " Remove the duplicate definition to resolve this conflict."

broadcastCycleErrors :: AppState -> S.Set Int -> IO ()
broadcastCycleErrors st cycleIds
    | S.null cycleIds = pure ()
    | otherwise = do
        let cids = S.toList cycleIds
            cycleMsg = T.intercalate ", " (map (T.pack . show) cids)
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
