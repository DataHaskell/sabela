{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sabela.Handlers.Lean (
    ensureLeanSessionAlive,
    killLeanSession,
    executeLeanCells,
    checkBridgeChanged,
    collectLeanDeps,

    -- * Lean document assembly
    CellLineMap (..),
    assembleLeanDocument,
    groupDiagsByCell,
    mapDiagToError,
    parseLeanExports,
) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, void, when)
import Data.Either (fromRight)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Sabela.Api (RunResult (..))
import Sabela.LeanLsp (
    Diagnostic (..),
    DiagnosticSeverity (..),
    Position (..),
    Range (..),
 )
import Sabela.LeanSession (
    LeanSession,
    closeLeanSession,
    newLeanSession,
    sendDocAndGetDiags,
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
import System.Directory (createDirectoryIfMissing, doesFileExist)
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
            _ -> startLeanSession app requiredDeps mSess

startLeanSession ::
    App -> S.Set Text -> Maybe LeanSession -> IO (Maybe LeanSession, Bool)
startLeanSession app deps mOldSess = do
    forM_ mOldSess $ \ls ->
        void (try (closeLeanSession ls) :: IO (Either SomeException ()))
    let projDir = envTmpDir (appEnv app) </> "lean-project"
    createDirectoryIfMissing True projDir
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
    result <- try (newLeanSession projDir) :: IO (Either SomeException LeanSession)
    case result of
        Left e -> leanFailure app "Lean session failed" e
        Right ls -> do
            setLeanDeps (appDeps app) deps
            broadcast app (EvSessionStatus SReady)
            pure (Just ls, True)

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
    ensureLeanFile (dir </> "Scratch.lean") ""

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
    forM_ affected $ \c -> broadcast app (EvCellUpdating (cellId c))
    ok <- ensureLeanSessionAlive app
    if not ok
        then forM_ affected $ \c ->
            broadcastCellError
                app
                (cellId c)
                "Lean session not available. Is 'lake' installed and on PATH?"
        else runLeanDiagnostics app gen leanCells affected onBridgeChanged

runLeanDiagnostics :: App -> Int -> [Cell] -> [Cell] -> IO () -> IO ()
runLeanDiagnostics app gen leanCells affected onBridgeChanged = do
    mLean <- getLeanSession (appSessions app)
    case mLean of
        Nothing -> pure ()
        Just ls -> do
            bridgeVals <- getBridgeValues (appBridge app)
            diags <- fetchDiagnostics ls bridgeVals leanCells
            let affectedIds = S.fromList (map cellId affected)
            whenCurrentGen app gen $ do
                distributeLeanResults
                    app
                    (snd (assembleLeanDocument bridgeVals leanCells))
                    diags
                    leanCells
                    affectedIds
                checkBridgeChanged app bridgeVals onBridgeChanged

fetchDiagnostics :: LeanSession -> M.Map Text Text -> [Cell] -> IO [Diagnostic]
fetchDiagnostics ls bridgeVals leanCells = do
    let (doc, _) = assembleLeanDocument bridgeVals leanCells
    diagResult <-
        try (sendDocAndGetDiags ls doc) :: IO (Either SomeException [Diagnostic])
    pure $ fromRight [] diagResult

checkBridgeChanged :: App -> M.Map Text Text -> IO () -> IO ()
checkBridgeChanged app oldBridge onBridgeChanged = do
    newBridge <- getBridgeValues (appBridge app)
    when (newBridge /= oldBridge) onBridgeChanged

distributeLeanResults ::
    App -> [CellLineMap] -> [Diagnostic] -> [Cell] -> S.Set Int -> IO ()
distributeLeanResults app lineMap diags leanCells affectedIds = do
    let diagsByCell = groupDiagsByCell lineMap diags
    forM_ leanCells $ \c ->
        when (S.member (cellId c) affectedIds) $
            processCellDiagnostics
                app
                lineMap
                c
                (M.findWithDefault [] (cellId c) diagsByCell)

processCellDiagnostics :: App -> [CellLineMap] -> Cell -> [Diagnostic] -> IO ()
processCellDiagnostics app lineMap c cellDiags = do
    let cid = cellId c
        (outputs, errText, cellErrors) = classifyLeanDiags lineMap c cellDiags
    updateAndBroadcast
        app
        ( \nb' ->
            nb'{nbCells = map (applyResult (RunResult cid outputs errText)) (nbCells nb')}
        )
        (EvCellResult cid outputs errText cellErrors)
    storeLeanExports app lineMap c cellDiags

storeLeanExports :: App -> [CellLineMap] -> Cell -> [Diagnostic] -> IO ()
storeLeanExports app lineMap c cellDiags = do
    let infoDiags = [d | d <- cellDiags, diagSeverity d `elem` [Just DsInformation, Just DsHint]]
        cellOffset = maybe 0 clmStartLine (find (\m -> clmCellId m == cellId c) lineMap)
    forM_ (parseLeanExports (cellSource c) cellOffset infoDiags) $
        uncurry (setBridgeValue (appBridge app))

classifyLeanDiags ::
    [CellLineMap] -> Cell -> [Diagnostic] -> ([OutputItem], Maybe Text, [CellError])
classifyLeanDiags lineMap c cellDiags =
    let infos =
            [ diagMessage d
            | d <- cellDiags
            , diagSeverity d `elem` [Just DsInformation, Just DsHint]
            ]
        errs =
            [ diagMessage d
            | d <- cellDiags
            , diagSeverity d `elem` [Just DsError, Just DsWarning, Nothing]
            ]
        outputs = leanDiagOutputs infos errs (cellSource c)
        errText = if null errs then Nothing else Just (T.unlines errs)
        cellErrors =
            [ mapDiagToError lineMap (cellId c) d
            | d <- cellDiags
            , diagSeverity d `elem` [Just DsError, Just DsWarning, Nothing]
            ]
     in (outputs, errText, cellErrors)

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

parseLeanExports :: Text -> Int -> [Diagnostic] -> [(Text, Text)]
parseLeanExports src cellOffset diags =
    mapMaybe
        (findDiagForExport cellOffset (length srcLines) diags annotations)
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

findDiagForExport ::
    Int -> Int -> [Diagnostic] -> [(Text, Int)] -> (Text, Int) -> Maybe (Text, Text)
findDiagForExport cellOffset numLines diags allAnnotations (name, lineIdx) =
    let docLine = cellOffset + lineIdx
        nextLine = case dropWhile ((<= lineIdx) . snd) allAnnotations of
            ((_, nl) : _) -> cellOffset + nl
            [] -> cellOffset + numLines
     in case [ diagMessage d
             | d <- diags
             , let dl = posLine (rangeStart (diagRange d))
             , dl > docLine && dl < nextLine
             ] of
            (msg : _) -> Just (name, msg)
            [] -> Nothing

data CellLineMap = CellLineMap
    { clmCellId :: Int
    , clmStartLine :: Int
    , clmEndLine :: Int
    }
    deriving (Show, Eq)

assembleLeanDocument :: M.Map Text Text -> [Cell] -> (Text, [CellLineMap])
assembleLeanDocument bridgeVals cells =
    let importBlock = collectLeanImports cells
        bridgeBlock = leanBridgeBlock bridgeVals
        headerLines = length importBlock + length bridgeBlock
        (bodyLines, lineMap) = foldCellsWithLineMap cells headerLines
     in (T.unlines (importBlock ++ bridgeBlock ++ reverse bodyLines), reverse lineMap)

collectLeanImports :: [Cell] -> [Text]
collectLeanImports cells =
    let imports = concatMap (filter isImportLine . T.lines . cellSource) cells
     in if null imports then [] else imports ++ [""]

leanBridgeBlock :: M.Map Text Text -> [Text]
leanBridgeBlock bridgeVals
    | M.null bridgeVals = []
    | otherwise =
        [ "def _bridge_" <> name <> " : String := " <> T.pack (show (T.unpack val))
        | (name, val) <- M.toList bridgeVals
        ]
            ++ [""]

isImportLine :: Text -> Bool
isImportLine l = "import " `T.isPrefixOf` T.stripStart l

foldCellsWithLineMap :: [Cell] -> Int -> ([Text], [CellLineMap])
foldCellsWithLineMap cells startLine = go cells startLine [] []
  where
    go [] _ docAcc mapAcc = (docAcc, mapAcc)
    go (c : cs) curLine docAcc mapAcc =
        let srcLines = filter (not . isImportLine) (T.lines (cellSource c))
            n = length srcLines
         in go
                cs
                (curLine + n)
                (reverse srcLines ++ docAcc)
                (CellLineMap (cellId c) curLine (curLine + n) : mapAcc)

groupDiagsByCell :: [CellLineMap] -> [Diagnostic] -> M.Map Int [Diagnostic]
groupDiagsByCell lineMap = foldr assignDiag M.empty
  where
    assignDiag d acc =
        let ln = posLine (rangeStart (diagRange d))
         in case find (\m -> ln >= clmStartLine m && ln < clmEndLine m) lineMap of
                Just m -> M.insertWith (++) (clmCellId m) [d] acc
                Nothing -> acc

mapDiagToError :: [CellLineMap] -> Int -> Diagnostic -> CellError
mapDiagToError lineMap cid d =
    let ln = posLine (rangeStart (diagRange d))
        col = posCharacter (rangeStart (diagRange d))
        relLine =
            maybe (ln + 1) (\m -> ln - clmStartLine m + 1) $
                find (\m -> clmCellId m == cid) lineMap
     in CellError
            { ceLine = Just relLine
            , ceCol = Just (col + 1)
            , ceMessage = diagMessage d
            }
