{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sabela.Handlers.Lean (
    ensureLeanSessionAlive,
    killLeanSession,
    executeLeanCells,
    checkBridgeChanged,
    collectLeanDeps,

    -- * Lean REPL helpers (exported for tests)
    parseLeanExports,
    classifyReplMessages,
    replMsgToError,
) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, void, when)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Sabela.Api (RunResult (..))
import Sabela.LeanRepl (
    LeanSession (..),
    ReplMessage (..),
    ReplPos (..),
    ReplResponse (..),
    closeLeanSession,
    newLeanSession,
    sendCommand,
 )
import Sabela.Model (
    Cell (..),
    CellError (..),
    CellType (..),
    Notebook (..),
    NotebookEvent (..),
    OutputItem (..),
    SessionStatus (..),
 )
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..))
import Sabela.State.BridgeStore (getBridgeValues, setBridgeValue)
import Sabela.State.DependencyTracker (getLeanDeps, setLeanDeps)
import Sabela.State.Environment (Environment (..))
import Sabela.State.NotebookStore (readNotebook)
import Sabela.State.SessionManager (getLeanSession, modifyLeanSession)
import System.Directory (
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    findExecutable,
 )
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import qualified System.IO
import qualified System.Process

import Sabela.Handlers.Shared

killLeanSession :: App -> IO ()
killLeanSession app =
    void $ modifyLeanSession (appSessions app) $ \mSess -> do
        forM_ mSess $ \ls ->
            void (try (closeLeanSession ls) :: IO (Either SomeException ()))
        pure (Nothing, ())

ensureLeanSessionAlive :: App -> IO Bool
ensureLeanSessionAlive app =
    modifyLeanSession (appSessions app) $ \mSess -> do
        nb <- readNotebook (appNotebook app)
        let requiredDeps = collectLeanDeps nb
        installed <- getLeanDeps (appDeps app)
        case (mSess, requiredDeps /= installed) of
            (Just ls, False) -> pure (Just ls, True)
            (Nothing, False)
                | not (S.null installed) ->
                    -- Session dead but deps already built — just restart REPL
                    restartLeanRepl app installed
            _ -> startLeanSession app requiredDeps mSess

{- | Restart just the REPL without rebuilding. Used when deps haven't changed
but the session was killed (e.g., by Run All or notebook load).
Falls back to full rebuild if the project dir doesn't exist.
-}
restartLeanRepl :: App -> S.Set Text -> IO (Maybe LeanSession, Bool)
restartLeanRepl app deps = do
    let projDir = envLeanCache (appEnv app) </> leanProjectName deps
    hasProject <- doesDirectoryExist (projDir </> ".lake")
    if hasProject
        then initializeLeanSession app projDir deps
        else startLeanSession app deps Nothing

startLeanSession ::
    App -> S.Set Text -> Maybe LeanSession -> IO (Maybe LeanSession, Bool)
startLeanSession app deps mOldSess = do
    forM_ mOldSess $ \ls ->
        void (try (closeLeanSession ls) :: IO (Either SomeException ()))
    let projDir = envLeanCache (appEnv app) </> leanProjectName deps
    createDirectoryIfMissing True projDir
    seedFromBase (envLeanBase (appEnv app)) projDir
    writeLeanProject projDir deps
    broadcastLeanDeps app deps
    buildAndStartLean app projDir deps

broadcastLeanDeps :: App -> S.Set Text -> IO ()
broadcastLeanDeps app deps = do
    unless (S.null deps) $
        broadcast app (EvSessionStatus (SUpdateDeps (S.toList deps)))
    broadcast app (EvInstallLog "Building Lean project...")

buildAndStartLean ::
    App -> FilePath -> S.Set Text -> IO (Maybe LeanSession, Bool)
buildAndStartLean app projDir deps = do
    buildResult <-
        try (buildLeanProject app projDir) :: IO (Either SomeException ())
    case buildResult of
        Left e -> leanFailure app "Lean build failed" e
        Right () -> initializeLeanSession app projDir deps

leanFailure :: App -> Text -> SomeException -> IO (Maybe LeanSession, Bool)
leanFailure app prefix e = do
    let msg = T.pack (show e)
    debugLog app $ "[handler] " <> prefix <> ": " <> msg
    broadcast app (EvInstallLog (prefix <> ": " <> msg))
    broadcast app (EvSessionStatus SReset)
    pure (Nothing, False)

initializeLeanSession ::
    App -> FilePath -> S.Set Text -> IO (Maybe LeanSession, Bool)
initializeLeanSession app projDir deps = do
    broadcast app (EvSessionStatus SStarting)
    replBin <- resolveLeanReplBin (appEnv app)
    case replBin of
        Nothing -> do
            broadcast app (EvInstallLog "Lean REPL binary not found. Set SABELA_LEAN_REPL.")
            broadcast app (EvSessionStatus SReset)
            pure (Nothing, False)
        Just bin -> do
            result <-
                try (newLeanSession projDir bin) :: IO (Either SomeException LeanSession)
            case result of
                Left e -> leanFailure app "Lean session failed" e
                Right ls -> do
                    setLeanDeps (appDeps app) deps
                    broadcast app (EvSessionStatus SReady)
                    pure (Just ls, True)

{- | Resolve the Lean REPL binary path.
Priority: SABELA_LEAN_REPL env var > envLeanReplBin config
        > repl/ submodule (auto-build if needed) > PATH lookup.
-}
resolveLeanReplBin :: Environment -> IO (Maybe FilePath)
resolveLeanReplBin env = do
    mEnvVar <- lookupEnv "SABELA_LEAN_REPL"
    case mEnvVar of
        Just p -> pure (Just p)
        Nothing -> case envLeanReplBin env of
            Just p -> pure (Just p)
            Nothing -> do
                let replBin = envWorkDir env </> "repl" </> ".lake" </> "build" </> "bin" </> "repl"
                    replDir = envWorkDir env </> "repl"
                binExists <- doesFileExist replBin
                if binExists
                    then pure (Just replBin)
                    else do
                        dirExists <- doesDirectoryExist replDir
                        if dirExists
                            then buildReplBinary replDir replBin
                            else findExecutable "repl"

-- | Build the REPL binary from source in the given directory.
buildReplBinary :: FilePath -> FilePath -> IO (Maybe FilePath)
buildReplBinary replDir binPath = do
    System.IO.hPutStrLn System.IO.stderr $
        "[sabela] Building Lean REPL in " ++ replDir ++ " ..."
    let cp =
            (System.Process.proc "lake" ["build"])
                { System.Process.cwd = Just replDir
                , System.Process.std_out = System.Process.Inherit
                , System.Process.std_err = System.Process.Inherit
                }
    (_, _, _, ph) <- System.Process.createProcess cp
    _ <- System.Process.waitForProcess ph
    built <- doesFileExist binPath
    if built
        then do
            System.IO.hPutStrLn System.IO.stderr "[sabela] Lean REPL built successfully."
            pure (Just binPath)
        else do
            System.IO.hPutStrLn System.IO.stderr "[sabela] Lean REPL build failed."
            pure Nothing

buildLeanProject :: App -> FilePath -> IO ()
buildLeanProject app dir = do
    let cp =
            (System.Process.proc "lake" ["build"])
                { System.Process.cwd = Just dir
                , System.Process.std_out = System.Process.CreatePipe
                , System.Process.std_err = System.Process.CreatePipe
                }
    (_, mOut, mErr, ph) <- System.Process.createProcess cp
    forM_ mOut $ \h -> void $ forkIO $ broadcastProcessOutput app h
    forM_ mErr $ \h -> void $ forkIO $ broadcastProcessOutput app h
    _ <- System.Process.waitForProcess ph
    pure ()

broadcastProcessOutput :: App -> System.IO.Handle -> IO ()
broadcastProcessOutput app h = do
    result <- try (TIO.hGetContents h) :: IO (Either SomeException Text)
    case result of
        Left _ -> pure ()
        Right content ->
            forM_ (filter (not . T.null) (T.lines content)) $ \line ->
                broadcast app (EvInstallLog line)

collectLeanDeps :: Notebook -> S.Set Text
collectLeanDeps nb =
    let leanCells = filter isLeanCodeCell (nbCells nb)
     in S.fromList (concatMap (parseLakeDeps . cellSource) leanCells)

isLeanCodeCell :: Cell -> Bool
isLeanCodeCell c = cellType c == CodeCell && cellLang c == ST.Lean4

data LakeDep = LakeDep {ldName :: Text, ldScope :: Text}

parseLakeDeps :: Text -> [Text]
parseLakeDeps src =
    [ ldScope dep <> "/" <> ldName dep
    | line <- T.lines src
    , Just dep <- [parseLakeDepLine line]
    ]

parseLakeDepLine :: Text -> Maybe LakeDep
parseLakeDepLine line = do
    rest <- T.stripPrefix "-- lake:" (T.strip line)
    case T.words (T.strip rest) of
        ("require" : name : "from" : scope : _) -> Just (LakeDep name scope)
        ("require" : name : _) -> Just (LakeDep name name)
        _ -> Nothing

writeLeanProject :: FilePath -> S.Set Text -> IO ()
writeLeanProject dir deps = do
    writeFile (dir </> "lakefile.toml") (renderLakefile deps)
    ensureLeanFile (dir </> "lean-toolchain") "leanprover/lean4:v4.29.0\n"

ensureLeanFile :: FilePath -> String -> IO ()
ensureLeanFile path content = do
    exists <- doesFileExist path
    unless exists $ writeFile path content

renderLakefile :: S.Set Text -> String
renderLakefile deps =
    unlines $
        [ "name = \"scratch\""
        , "version = \"0.1.0\""
        , ""
        , "[[lean_lib]]"
        , "name = \"Scratch\""
        ]
            ++ concatMap renderLakeDep (S.toList deps)

renderLakeDep :: Text -> [String]
renderLakeDep key = case T.splitOn "/" key of
    [scope, name] ->
        [ ""
        , "[[require]]"
        , "name = " ++ show (T.unpack name)
        , "scope = " ++ show (T.unpack scope)
        ]
    _ -> []

-- --------------------------------------------------------------------------
-- Execution
-- --------------------------------------------------------------------------

executeLeanCells :: App -> Int -> Int -> IO () -> IO ()
executeLeanCells app gen editedCid onBridgeChanged = do
    nb <- readNotebook (appNotebook app)
    let leanCells = filter isLeanCodeCell (nbCells nb)
        affected = selectAffectedLeanCells editedCid leanCells
    unless (null leanCells) $
        runLeanCells app gen leanCells affected onBridgeChanged

selectAffectedLeanCells :: Int -> [Cell] -> [Cell]
selectAffectedLeanCells editedCid leanCells
    | editedCid < 0 = leanCells
    | otherwise = dropWhile (\c -> cellId c /= editedCid) leanCells

runLeanCells :: App -> Int -> [Cell] -> [Cell] -> IO () -> IO ()
runLeanCells app gen leanCells affected onBridgeChanged = do
    ok <- ensureLeanSessionAlive app
    if not ok
        then forM_ affected $ \c ->
            broadcastCellError
                app
                (cellId c)
                "Lean session not available. Is 'lake' installed and on PATH?"
        else runLeanRepl app gen leanCells affected onBridgeChanged

runLeanRepl :: App -> Int -> [Cell] -> [Cell] -> IO () -> IO ()
runLeanRepl app gen leanCells affected onBridgeChanged = do
    mLean <- getLeanSession (appSessions app)
    case mLean of
        Nothing -> pure ()
        Just ls -> do
            -- Kill and restart the REPL for a fresh environment.
            -- This ensures we start from a clean state each time.
            closeLeanSession ls
            replBin <- resolveLeanReplBin (appEnv app)
            case replBin of
                Nothing -> pure ()
                Just bin -> do
                    newLs <- newLeanSession (lsProjectDir ls) bin
                    -- Store the new session
                    void $ modifyLeanSession (appSessions app) $ \_ ->
                        pure (Just newLs, ())
                    bridgeVals <- getBridgeValues (appBridge app)
                    execResult <-
                        try (executeAllCells app newLs bridgeVals leanCells affected) ::
                            IO (Either SomeException ())
                    case execResult of
                        Left e -> do
                            debugLog app $ "[lean-repl] execution error: " <> T.pack (show e)
                            let affectedIds = S.fromList (map cellId affected)
                            forM_ leanCells $ \c ->
                                when (S.member (cellId c) affectedIds) $
                                    broadcastCellError
                                        app
                                        (cellId c)
                                        ("Lean REPL error: " <> T.pack (show e))
                        Right () -> pure ()
                    whenCurrentGen app gen $
                        checkBridgeChanged app bridgeVals onBridgeChanged

executeAllCells ::
    App -> LeanSession -> M.Map Text Text -> [Cell] -> [Cell] -> IO ()
executeAllCells app ls bridgeVals leanCells affected = do
    let affectedIds = S.fromList (map cellId affected)
        allImports = collectLeanImports leanCells
        bridgeDefs = leanBridgeDefs bridgeVals

    -- Phase 1: Send imports (no env → fresh environment)
    mEnv0 <-
        if null allImports
            then pure Nothing
            else do
                broadcast app (EvInstallLog "Lean: loading imports...")
                let importCmd = T.unlines allImports
                resp <- sendCommand ls importCmd Nothing
                pure (Just (rrEnv resp))

    -- Phase 2: Send bridge definitions
    unless (null bridgeDefs) $
        broadcast app (EvInstallLog "Lean: loading bridge definitions...")
    envAfterBridge <- sendDefs ls bridgeDefs mEnv0

    -- Phase 3: Execute each cell
    broadcast app (EvInstallLog "Lean: type-checking cells...")
    _ <- executeCellsSequentially app ls leanCells affectedIds envAfterBridge
    pure ()

-- | Send a list of definitions one by one, chaining env numbers.
sendDefs :: LeanSession -> [Text] -> Maybe Int -> IO (Maybe Int)
sendDefs _ [] env = pure env
sendDefs ls (d : ds) mEnv = do
    resp <- sendCommand ls d mEnv
    sendDefs ls ds (Just (rrEnv resp))

{- | Execute cells sequentially, chaining env numbers.
Returns the final env number.
-}
executeCellsSequentially ::
    App -> LeanSession -> [Cell] -> S.Set Int -> Maybe Int -> IO (Maybe Int)
executeCellsSequentially _ _ [] _ env = pure env
executeCellsSequentially app ls (c : cs) affectedIds mEnv = do
    let cid = cellId c
        src = cellSource c
        strippedLines = filter (not . isImportLine) (T.lines src)
        strippedSrc = T.unlines strippedLines
    when (S.member cid affectedIds) $
        broadcast app (EvCellUpdating cid)
    if T.null (T.strip strippedSrc)
        then do
            -- Empty cell after stripping imports — broadcast success
            when (S.member cid affectedIds) $
                updateAndBroadcast
                    app
                    (\nb -> nb{nbCells = map (applyResult (RunResult cid [] Nothing)) (nbCells nb)})
                    (EvCellResult cid [] Nothing [])
            executeCellsSequentially app ls cs affectedIds mEnv
        else do
            resp <- sendCommand ls strippedSrc mEnv
            let newEnv = Just (rrEnv resp)
            when (S.member cid affectedIds) $ do
                let (outputs, errText, cellErrors) = classifyReplMessages (rrMessages resp) src
                updateAndBroadcast
                    app
                    ( \nb -> nb{nbCells = map (applyResult (RunResult cid outputs errText)) (nbCells nb)}
                    )
                    (EvCellResult cid outputs errText cellErrors)
                storeLeanExports app c (rrMessages resp)
            executeCellsSequentially app ls cs affectedIds newEnv

checkBridgeChanged :: App -> M.Map Text Text -> IO () -> IO ()
checkBridgeChanged app oldBridge onBridgeChanged = do
    newBridge <- getBridgeValues (appBridge app)
    when (newBridge /= oldBridge) onBridgeChanged

-- --------------------------------------------------------------------------
-- Output classification
-- --------------------------------------------------------------------------

classifyReplMessages ::
    [ReplMessage] -> Text -> ([OutputItem], Maybe Text, [CellError])
classifyReplMessages msgs src =
    let infos =
            [ rmData m
            | m <- msgs
            , rmSeverity m `elem` ["information", "info"]
            ]
        errs =
            [ rmData m
            | m <- msgs
            , rmSeverity m `elem` ["error", "warning"]
            ]
        outputs = leanDiagOutputs infos errs src
        errText = if null errs then Nothing else Just (T.unlines errs)
        cellErrors =
            [ replMsgToError m
            | m <- msgs
            , rmSeverity m `elem` ["error", "warning"]
            ]
     in (outputs, errText, cellErrors)

replMsgToError :: ReplMessage -> CellError
replMsgToError msg =
    CellError
        { ceLine = Just (rpLine (rmPos msg))
        , ceCol = Just (rpColumn (rmPos msg))
        , ceMessage = rmData msg
        }

leanDiagOutputs :: [Text] -> [Text] -> Text -> [OutputItem]
leanDiagOutputs infos errs src
    | not (null infos) = [OutputItem "text/plain" (T.unlines infos)]
    | null errs && isLeanDeclaration src =
        [OutputItem "text/html" (leanSuccessHtml src)]
    | otherwise = []

leanSuccessHtml :: Text -> Text
leanSuccessHtml src =
    let isProof =
            any (`T.isPrefixOf` T.stripStart src) ["theorem ", "lemma ", "example "]
                || ":= by" `T.isInfixOf` src
     in "<span style=\"color:#a6e3a1\">&#10003; "
            <> (if isProof then "No goals" else "Defined")
            <> "</span>"

isLeanDeclaration :: Text -> Bool
isLeanDeclaration src =
    not ("#eval " `T.isInfixOf` src || "#check " `T.isInfixOf` src)
        && any (`T.isPrefixOf` firstRealLine src) leanDeclKeywords

firstRealLine :: Text -> Text
firstRealLine src =
    case filter isRealLine (T.lines src) of
        (l : _) -> T.stripStart l
        [] -> ""
  where
    isRealLine l = not (T.null (T.strip l)) && not ("--" `T.isPrefixOf` T.stripStart l)

leanDeclKeywords :: [Text]
leanDeclKeywords =
    [ "theorem "
    , "lemma "
    , "example "
    , "def "
    , "instance "
    , "class "
    , "structure "
    , "inductive "
    , "axiom "
    , "noncomputable "
    , "private "
    , "protected "
    , "section"
    , "namespace"
    , "open "
    , "import "
    ]

-- --------------------------------------------------------------------------
-- Exports (bridge)
-- --------------------------------------------------------------------------

storeLeanExports :: App -> Cell -> [ReplMessage] -> IO ()
storeLeanExports app c msgs = do
    let infoMsgs =
            [ m
            | m <- msgs
            , rmSeverity m `elem` ["information", "info"]
            ]
    forM_ (parseLeanExports (cellSource c) infoMsgs) $
        uncurry (setBridgeValue (appBridge app))

parseLeanExports :: Text -> [ReplMessage] -> [(Text, Text)]
parseLeanExports src msgs =
    mapMaybe
        (findMsgForExport (length srcLines) msgs annotations)
        annotations
  where
    srcLines = T.lines src
    annotations = extractExportAnnotations srcLines

extractExportAnnotations :: [Text] -> [(Text, Int)]
extractExportAnnotations srcLines =
    [ (T.strip rest, lineIdx)
    | (lineIdx, line) <- zip [0 ..] srcLines
    , Just rest <- [T.stripPrefix "-- export:" (T.strip line)]
    , not (T.null (T.strip rest))
    ]

findMsgForExport ::
    Int -> [ReplMessage] -> [(Text, Int)] -> (Text, Int) -> Maybe (Text, Text)
findMsgForExport numLines msgs allAnnotations (name, lineIdx) =
    let nextLine = case dropWhile ((<= lineIdx) . snd) allAnnotations of
            ((_, nl) : _) -> nl
            [] -> numLines
     in case [ rmData m
             | m <- msgs
             , let ml = rpLine (rmPos m)
             , -- REPL positions are 1-based, annotations are 0-based
             ml > lineIdx + 1 && ml <= nextLine + 1
             ] of
            (msg : _) -> Just (name, msg)
            [] -> Nothing

-- --------------------------------------------------------------------------
-- Base project seeding
-- --------------------------------------------------------------------------

{- | Copy the .lake directory from a pre-built base project into a new
project directory, so that @lake build@ can reuse cached packages
(e.g. Mathlib) instead of cloning them from scratch.
-}
seedFromBase :: Maybe FilePath -> FilePath -> IO ()
seedFromBase Nothing _ = pure ()
seedFromBase (Just baseDir) projDir = do
    let targetLake = projDir </> ".lake"
    alreadySeeded <- doesDirectoryExist targetLake
    unless alreadySeeded $ do
        let baseLake = baseDir </> ".lake"
        hasBase <- doesDirectoryExist baseLake
        when hasBase $ do
            let cp =
                    (System.Process.proc "cp" ["-a", baseLake, targetLake])
                        { System.Process.std_out = System.Process.CreatePipe
                        , System.Process.std_err = System.Process.CreatePipe
                        }
            (_, _, _, ph) <- System.Process.createProcess cp
            _ <- System.Process.waitForProcess ph
            pure ()

-- --------------------------------------------------------------------------
-- Project directory naming
-- --------------------------------------------------------------------------

{- | Generate a stable project directory name based on the dependency set.
Each unique set of deps gets its own cached directory so that switching
between notebooks with different deps doesn't blow away the Lake cache.
-}
leanProjectName :: S.Set Text -> FilePath
leanProjectName deps
    | S.null deps = "lean-project"
    | otherwise = "lean-project-" ++ show (hashDeps deps)
  where
    hashDeps :: S.Set Text -> Int
    hashDeps = S.foldl' (\acc d -> acc * 31 + T.foldl' (\a c -> a * 37 + fromEnum c) 0 d) 0

-- --------------------------------------------------------------------------
-- Helpers
-- --------------------------------------------------------------------------

collectLeanImports :: [Cell] -> [Text]
collectLeanImports cells =
    let imports = concatMap (filter isImportLine . T.lines . cellSource) cells
     in imports

isImportLine :: Text -> Bool
isImportLine l = "import " `T.isPrefixOf` T.stripStart l

leanBridgeDefs :: M.Map Text Text -> [Text]
leanBridgeDefs bridgeVals =
    [ "def _bridge_" <> name <> " : String := " <> T.pack (show (T.unpack val))
    | (name, val) <- M.toList bridgeVals
    ]
