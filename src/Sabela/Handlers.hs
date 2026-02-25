{-# LANGUAGE OverloadedStrings #-}

module Sabela.Handlers where

import Control.Concurrent (forkIO, modifyMVar_, readMVar)
import Control.Concurrent.STM (atomically, writeTChan)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, void, when)
import Data.Char
import Data.IORef (atomicModifyIORef', readIORef, writeIORef)
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
    SessionStatus (..),
 )
import Sabela.Output (displayPrelude, parseMimeOutput)
import Sabela.Session (SessionConfig (..), closeSession, newSession, runBlock)
import ScriptHs.Parser (
    CabalMeta (..),
    ScriptFile (..),
    mergeMetas,
    parseScript,
 )
import ScriptHs.Render (toGhciScript)
import ScriptHs.Run (resolveDeps)
import System.FilePath ((</>))
import System.IO (stderr)

data ReactiveNotebook = ReactiveNotebook
    { rnCellEdit :: Int -> Text -> IO ()
    , rnRunCell :: Int -> IO ()
    , rnRunAll :: IO ()
    , rnReset :: IO ()
    }

setupReactive :: AppState -> IO ReactiveNotebook
setupReactive st =
    pure $
        ReactiveNotebook
            { rnCellEdit = handleCellEdit st
            , rnRunCell = handleRunCell st
            , rnRunAll = handleRunAll st
            , rnReset = handleReset st
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
            { cellOutput = Nothing
            , cellError = Nothing
            , cellMime = "text/plain"
            , cellDirty = False
            }

executeSingleCell :: AppState -> Int -> Int -> IO ()
executeSingleCell st gen cid = do
    TIO.hPutStrLn stderr "[handler] handleReset"
    nb <- readMVar (stNotebook st)
    let metas = collectMetadata nb
        allCode = filter (\c -> cellType c == CodeCell) (nbCells nb)
    ok <- ensureSessionAlive st gen metas
    when ok $
        case filter (\c -> cellId c == cid) allCode of
            (cell : _) -> do
                still <- isCurrentGen st gen
                when still $ runAndBroadcast st gen cell
                still' <- isCurrentGen st gen
                when still' $ broadcast st EvExecutionDone
            [] -> broadcast st EvExecutionDone

executeFullRestart :: AppState -> Int -> IO ()
executeFullRestart st gen = do
  putStrLn "[handler] executeFullRestart: killing session, running all"
  nb <- readMVar (stNotebook st)
  let allCode = filter (\c -> cellType c == CodeCell) (nbCells nb)
      needed  = collectMetadata nb
  killSession st
  ok <- installAndRestart st gen needed
  when ok $ runCellList st gen allCode

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
    let allMetaApplicable =
            S.fromList (metaDeps metas) == installed
                && S.fromList (metaExts metas) == instExts
    case mSess of
        Just _ | allMetaApplicable -> pure True
        _ -> installAndRestart st gen metas

-- TODO: We should instead sort the cells in topological order.
-- That is, the cells that have not dependencies shoudl run first.
-- We also need to think through how we deal with circular dependencies.
-- I'm not sure at what stage we filter them out.
installAndRestart :: AppState -> Int -> CabalMeta -> IO Bool
installAndRestart st gen metas = do
    -- If we restarted after some changes (not in the notebook generation)
    -- we should see if there are new dependencies and reinstall them.
    stillInSameGeneration <- isCurrentGen st gen
    if not stillInSameGeneration
        then pure False
        else do
            installedDeps <- readIORef (stInstalledDeps st)
            let requiredDeps = S.fromList (metaDeps metas)
                depsUpToDate = requiredDeps == installedDeps
            unless depsUpToDate $ do
                let newDeps = S.difference requiredDeps installedDeps
                broadcast st $
                    EvSessionStatus $
                        if S.null newDeps then SDepsUpToDate else SUpdateDeps (S.toList newDeps)
                if null (metaDeps metas)
                    then do
                        writeIORef (stEnvFile st) Nothing
                        writeIORef (stInstalledDeps st) S.empty
                    else do
                        let envPath = stTmpDir st </> ".ghc.environment"
                        TIO.hPutStrLn stderr $
                            "[sabela] cabal install --lib " <> T.unwords (metaDeps metas)
                        resolveDeps envPath (metaDeps metas)
                        writeIORef (stEnvFile st) (Just envPath)
                        writeIORef (stInstalledDeps st) requiredDeps
            writeIORef (stInstalledExts st) (S.fromList (metaExts metas))
            broadcast st (EvSessionStatus SStarting)
            killSession st
            startSessionWith st (metaDeps metas) (metaExts metas)
            pure True

startSessionWith :: AppState -> [Text] -> [Text] -> IO ()
startSessionWith st deps exts = do
    envFile <- readIORef (stEnvFile st)
    let cfg =
            SessionConfig
                { scDeps = deps
                , scExts = exts
                , scGhcOptions = []
                , scEnvFile = envFile
                }
    TIO.hPutStrLn stderr "[handler] Injecting display prelude"
    sess <- newSession cfg
    _ <- runBlock sess displayPrelude
    modifyMVar_ (stSession st) (\_ -> pure (Just sess))
    broadcast st (EvSessionStatus SReady)

isCurrentGen :: AppState -> Int -> IO Bool
isCurrentGen st gen = (== gen) <$> readIORef (stGeneration st)

runAndBroadcast :: AppState -> Int -> Cell -> IO ()
runAndBroadcast st gen cell = do
    broadcast st (EvCellUpdating (cellId cell))
    (result, errs) <- execCell st cell
    still <- isCurrentGen st gen
    when still $ do
        modifyMVar_ (stNotebook st) $ \nb ->
            pure nb{nbCells = map (applyResult result) (nbCells nb)}
        broadcast
            st
            ( EvCellResult
                (rrCellId result)
                (rrOutput result)
                (rrError result)
                (rrMime result)
                errs
            )

execCell :: AppState -> Cell -> IO (RunResult, [CellError])
execCell st cell = do
    mSess <- readMVar (stSession st)
    case mSess of
        Nothing ->
            pure (RunResult (cellId cell) Nothing (Just "No GHCi session") "text/plain", [])
        Just sess -> do
            let sf = scriptLines (parseScript (cellSource cell))
            let ghci = toGhciScript sf
            TIO.hPutStrLn stderr $
                T.pack $
                    "[handler] Cell " ++ show (cellId cell) ++ ":\n" ++ T.unpack ghci
            (rawOut, rawErr) <- runBlock sess ghci
            let (mime, body) = parseMimeOutput rawOut
                errs = parseErrors rawErr
            TIO.hPutStrLn stderr $
                T.pack $
                    "[handler] → mime="
                        ++ T.unpack mime
                        ++ " out="
                        ++ show (T.take 80 body)
                        ++ " errors="
                        ++ show (length errs)
            pure
                ( RunResult
                    { rrCellId = cellId cell
                    , rrOutput = if T.null body then Nothing else Just body
                    , rrError = if T.null rawErr then Nothing else Just rawErr
                    , rrMime = mime
                    }
                , errs
                )

applyResult :: RunResult -> Cell -> Cell
applyResult r c
    | cellId c == rrCellId r =
        c
            { cellOutput = rrOutput r
            , cellError = rrError r
            , cellMime = rrMime r
            , cellDirty = False
            }
    | otherwise = c

executeAffected :: AppState -> Int -> Int -> IO ()
executeAffected st gen editedCids = do
    TIO.hPutStrLn stderr $
        T.pack $
            "[handler] smartExecuteAffected: editedCid="
                ++ show editedCids
    nb <- readMVar (stNotebook st)
    let allCode = filter (\c -> cellType c == CodeCell) (nbCells nb)
        needed = collectMetadata nb

    installed <- readIORef (stInstalledDeps st)
    instExts <- readIORef (stInstalledExts st)
    mSess <- readMVar (stSession st)

    let neededD = S.fromList (metaDeps needed)
        neededE = S.fromList (metaExts needed)
        depsOk = neededD == installed
        extsOk = neededE == instExts

    case (mSess, depsOk && extsOk) of
        (Just _, True) -> do
            let toRun = selectAffected editedCids allCode
                allIds = map cellId allCode
                runIds = map cellId toRun
                skipIds = filter (`notElem` runIds) allIds

            TIO.hPutStrLn stderr $ T.pack $ "[handler] All code cells: " ++ show allIds
            TIO.hPutStrLn stderr $
                T.pack $
                    "[handler] From edited cell onward: " ++ show allIds
            TIO.hPutStrLn stderr $ T.pack $ "[handler] WILL RUN (affected): " ++ show runIds
            TIO.hPutStrLn stderr $
                T.pack $
                    "[handler] WILL SKIP (unaffected): " ++ show skipIds

            forM_ allCode $ \c -> do
                let (defs, uses) = cellNames (cellSource c)
                TIO.hPutStrLn stderr $
                    T.pack $
                        "[handler]   cell "
                            ++ show (cellId c)
                            ++ " defines="
                            ++ show (S.toList defs)
                            ++ " uses="
                            ++ show (take 10 (S.toList uses))
                            ++ (if S.size uses > 10 then "..." else "")

            runCellList st gen toRun

        -- ── Deps changed → full restart ──
        (_, False) -> do
            putStrLn "[handler] Deps/exts changed → full restart"
            ok <- installAndRestart st gen needed
            when ok $ runCellList st gen allCode

        -- ── No session → create + run all ──
        (Nothing, True) -> do
            putStrLn "[handler] No session → starting fresh, running all"
            ok <- installAndRestart st gen needed
            when ok $ runCellList st gen allCode

runCellList :: AppState -> Int -> [Cell] -> IO ()
runCellList st gen cells = do
    forM_ cells $ \cell -> do
        still <- isCurrentGen st gen
        when still $ runAndBroadcast st gen cell
    still <- isCurrentGen st gen
    when still $ broadcast st EvExecutionDone

cellNames :: Text -> (S.Set Text, S.Set Text)
cellNames src = (defs, uses)
  where
    ls = T.lines src
    defs = S.fromList $ concatMap extractDefs ls
    uses = S.fromList $ concatMap extractTokens ls

extractDefs :: Text -> [Text]
extractDefs line
    | T.null s = []
    | T.isPrefixOf "--" s = [] -- comment
    | T.isPrefixOf ":" s = [] -- GHCi command
    | T.isPrefixOf "import " s = []
    | T.isPrefixOf "{-#" s = [] -- pragma
    -- let binding: "let x = ..."
    | Just rest <- stripKW "let" s = firstLowerIdent rest
    -- type-level: "data X", "type X", "newtype X", "class X"
    | Just rest <- stripKW "data" s = firstAnyIdent rest
    | Just rest <- stripKW "type" s = firstAnyIdent rest
    | Just rest <- stripKW "newtype" s = firstAnyIdent rest
    | Just rest <- stripKW "class" s = firstAnyIdent rest
    -- value binding: "name ... =" or monadic bind: "name <- ..."
    | otherwise =
        let toks = T.words s
         in case toks of
                (w : rest)
                    | isLowerIdent w
                    , any (\t -> t == "=" || t == "<-") (take 8 rest) ->
                        [w]
                _ -> []
  where
    s = T.strip line

stripKW :: Text -> Text -> Maybe Text
stripKW kw t = case T.stripPrefix kw t of
    Just rest
        | T.null rest -> Nothing
        | not (isIdentChar (T.head rest)) -> Just (T.stripStart rest)
    _ -> Nothing

firstLowerIdent :: Text -> [Text]
firstLowerIdent t =
    let w = T.takeWhile isIdentChar (T.stripStart t)
     in [w | isLowerIdent w]

firstAnyIdent :: Text -> [Text]
firstAnyIdent t =
    let w = T.takeWhile isIdentChar (T.stripStart t)
     in [w | not (T.null w), isAlpha (T.head w) || T.head w == '_']

isLowerIdent :: Text -> Bool
isLowerIdent t =
    not (T.null t)
        && let c = T.head t in (isAlpha c && not (isAsciiUpper c)) || c == '_'

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_' || c == '\''

extractTokens :: Text -> [Text]
extractTokens = filter isIdent . T.split (not . isIdentChar)
  where
    isIdent t = not (T.null t) && (isAlpha (T.head t) || T.head t == '_')

selectAffected :: Int -> [Cell] -> [Cell]
selectAffected editedSet = go S.empty
  where
    go _ [] = []
    go changed (c : cs) =
        let (defs, uses) = cellNames (cellSource c)
            isEdited = cellId c == editedSet
            isAffected = not (S.null (S.intersection uses changed))
         in if isEdited || isAffected
                then c : go (S.union changed defs) cs
                else go changed cs
