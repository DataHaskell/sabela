{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Sabela.Server (
    mkApp,
    initState,
) where

import Control.Concurrent.MVar (modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Concurrent.STM (
    atomically,
    dupTChan,
    newBroadcastTChanIO,
    readTChan,
 )
import Control.Monad (forM, forever)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Char
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (isPrefixOf, sort)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Network.HTTP.Types (hContentType, status200)
import Network.Wai (responseStream)
import Servant
import System.Directory (
    canonicalizePath,
    createDirectoryIfMissing,
    doesDirectoryExist,
    listDirectory,
    makeAbsolute,
 )
import System.FilePath (
    makeRelative,
    normalise,
    splitDirectories,
    takeDirectory,
    (</>),
 )

import Sabela.Api
import Sabela.Handlers
import Sabela.Model
import Sabela.Output (builtinExamples)
import Sabela.Session (queryComplete, queryDoc, queryInfo, queryType)
import ScriptHs.Markdown (Segment (..), parseMarkdown, reassemble)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)

-- ── API types ────────────────────────────────────────────────────

type JsonAPI =
    "api" :> "notebook" :> Get '[JSON] Notebook
        :<|> "api" :> "load" :> ReqBody '[JSON] LoadRequest :> Post '[JSON] Notebook
        :<|> "api" :> "save" :> ReqBody '[JSON] SaveRequest :> Post '[JSON] Notebook
        :<|> "api"
            :> "cell"
            :> Capture "id" Int
            :> ReqBody '[JSON] UpdateCell
            :> Put '[JSON] Cell
        :<|> "api" :> "cell" :> ReqBody '[JSON] InsertCell :> Post '[JSON] Cell
        :<|> "api"
            :> "cell"
            :> Capture "id" Int
            :> Delete '[JSON] Notebook
        :<|> "api" :> "run" :> Capture "id" Int :> Post '[JSON] RunResult
        :<|> "api" :> "run-all" :> Post '[JSON] RunAllResult
        :<|> "api" :> "reset" :> Post '[JSON] Notebook
        -- File explorer
        :<|> "api"
            :> "files"
            :> QueryParam "path" Text
            :> Get '[JSON] [FileEntry]
        :<|> "api" :> "file" :> QueryParam "path" Text :> Get '[JSON] Text
        :<|> "api"
            :> "file"
            :> "create"
            :> ReqBody '[JSON] CreateFileRequest
            :> Post '[JSON] FileEntry
        :<|> "api"
            :> "file"
            :> "write"
            :> ReqBody '[JSON] WriteFileRequest
            :> Post '[JSON] Text
        -- IDE
        :<|> "api"
            :> "complete"
            :> ReqBody '[JSON] CompleteRequest
            :> Post '[JSON] CompleteResult
        :<|> "api"
            :> "info"
            :> ReqBody '[JSON] InfoRequest
            :> Post '[JSON] InfoResult
        -- Examples
        :<|> "api" :> "examples" :> Get '[JSON] [Example]

type FullAPI =
    JsonAPI
        :<|> "api" :> "events" :> Raw
        :<|> Raw

fullProxy :: Proxy FullAPI
fullProxy = Proxy

mkApp :: AppState -> ReactiveNotebook -> FilePath -> Application
mkApp st rn staticDir = serve fullProxy (server st rn staticDir)

server :: AppState -> ReactiveNotebook -> FilePath -> Server FullAPI
server st rn staticDir =
    ( getNotebookH st
        :<|> loadNotebookH st rn
        :<|> saveNotebookH st
        :<|> updateCellH st rn
        :<|> insertCellH st
        :<|> deleteCellH st
        :<|> runCellH rn
        :<|> runAllH rn
        :<|> resetH rn st
        :<|> listFilesH st
        :<|> readFileH st
        :<|> createFileH st
        :<|> writeFileH st
        :<|> completeH st
        :<|> infoH st
        :<|> examplesH
    )
        :<|> Tagged (sseApp st)
        :<|> serveDirectoryWebApp staticDir

initState :: FilePath -> IO AppState
initState workDir = do
    nb <- newMVar (Notebook "Untitled" [])
    sess <- newMVar Nothing
    tmpBase <- getCanonicalTemporaryDirectory
    tmpDir <- createTempDirectory tmpBase "sabela-server"
    envRef <- newIORef Nothing
    nextId <- newIORef 0
    instDeps <- newIORef Set.empty
    instExts <- newIORef Set.empty
    bcast <- newBroadcastTChanIO
    gen <- newIORef 0
    debounce <- newMVar Nothing
    absWork <- makeAbsolute workDir
    pure
        AppState
            { stNotebook = nb
            , stSession = sess
            , stTmpDir = tmpDir
            , stWorkDir = absWork
            , stEnvFile = envRef
            , stNextId = nextId
            , stInstalledDeps = instDeps
            , stInstalledExts = instExts
            , stBroadcast = bcast
            , stGeneration = gen
            , stDebounceRef = debounce
            }

-- ── SSE ──────────────────────────────────────────────────────────

sseApp :: AppState -> Application
sseApp st _req resp = do
    chan <- atomically $ dupTChan (stBroadcast st)
    resp $ responseStream status200 hdrs $ \write flush -> do
        write (Builder.byteString ": connected\n\n")
        flush
        forever $ do
            ev <- atomically $ readTChan chan
            let json = LBS.toStrict (encode ev)
            write (Builder.byteString $ "data: " <> json <> "\n\n")
            flush
  where
    hdrs =
        [ (hContentType, "text/event-stream")
        , ("Cache-Control", "no-cache")
        , ("Connection", "keep-alive")
        , ("Access-Control-Allow-Origin", "*")
        ]

-- ── Notebook CRUD ────────────────────────────────────────────────

getNotebookH :: AppState -> Handler Notebook
getNotebookH st = liftIO $ readMVar (stNotebook st)

loadNotebookH :: AppState -> ReactiveNotebook -> LoadRequest -> Handler Notebook
loadNotebookH st rn (LoadRequest path) = liftIO $ do
    let absPath = if "/" `isPrefixOf` path then path else stWorkDir st </> path
    raw <- TIO.readFile absPath
    let segs = parseMarkdown raw
    nid <- readIORef (stNextId st)
    let (cells, nid') = foldl go ([], nid) segs
        nb = Notebook (T.pack path) (reverse cells)
    writeIORef (stNextId st) nid'
    modifyMVar_ (stNotebook st) (\_ -> pure nb)
    rnRunAll rn
    pure nb
  where
    go (acc, n) (Prose t) = (Cell n ProseCell t Nothing Nothing "text/plain" False : acc, n + 1)
    go (acc, n) (CodeBlock _ code _) = (Cell n CodeCell code Nothing Nothing "text/plain" False : acc, n + 1)

-- | Save notebook back to markdown file.
saveNotebookH :: AppState -> SaveRequest -> Handler Notebook
saveNotebookH st (SaveRequest mPath) = liftIO $ do
    nb <- readMVar (stNotebook st)
    let path = case mPath of
            Just p -> p
            Nothing -> T.unpack (nbTitle nb)
        absPath = if "/" `isPrefixOf` path then path else stWorkDir st </> path
        segs = map cellToSegment (nbCells nb)
        md = reassemble segs
    createDirectoryIfMissing True (takeDirectory absPath)
    TIO.writeFile absPath md
    let nb' = nb{nbTitle = T.pack path}
    modifyMVar_ (stNotebook st) (\_ -> pure nb')
    putStrLn $ "[sabela] Saved to: " ++ absPath
    pure nb'
  where
    cellToSegment c = case cellType c of
        ProseCell -> Prose (cellSource c)
        CodeCell -> CodeBlock "haskell" (cellSource c) Nothing

updateCellH :: AppState -> ReactiveNotebook -> Int -> UpdateCell -> Handler Cell
updateCellH st rn cid (UpdateCell src) = liftIO $ do
    rnCellEdit rn cid src
    nb <- readMVar (stNotebook st)
    case filter (\c -> cellId c == cid) (nbCells nb) of
        (c : _) -> pure c
        [] -> pure (Cell cid CodeCell src Nothing Nothing "text/plain" True)

insertCellH :: AppState -> InsertCell -> Handler Cell
insertCellH st (InsertCell afterId typ src) = liftIO $ do
    nid <- readIORef (stNextId st)
    writeIORef (stNextId st) (nid + 1)
    let cell = Cell nid typ src Nothing Nothing "text/plain" True
    modifyMVar_ (stNotebook st) $ \nb ->
        pure nb{nbCells = ins afterId cell (nbCells nb)}
    pure cell
  where
    ins (-1) c cs = c : cs
    ins _ c [] = [c]
    ins aid c (x : xs)
        | cellId x == aid = x : c : xs
        | otherwise = x : ins aid c xs

deleteCellH :: AppState -> Int -> Handler Notebook
deleteCellH st cid = liftIO $
    modifyMVar (stNotebook st) $ \nb -> do
        let nb' = nb{nbCells = filter (\c -> cellId c /= cid) (nbCells nb)}
        pure (nb', nb')

runCellH :: ReactiveNotebook -> Int -> Handler RunResult
runCellH rn cid = liftIO $ do
    rnRunCell rn cid
    pure (RunResult cid Nothing Nothing "text/plain")

runAllH :: ReactiveNotebook -> Handler RunAllResult
runAllH rn = liftIO $ rnRunAll rn >> pure (RunAllResult [])

resetH :: ReactiveNotebook -> AppState -> Handler Notebook
resetH rn st = liftIO $ rnReset rn >> readMVar (stNotebook st)

-- ── File explorer ────────────────────────────────────────────────
-- Normalize path for comparison (especially on Windows)
normForCmp :: FilePath -> FilePath
normForCmp = map toLower . normalise

-- True if child is inside (or equal to) parent, by path components
isWithinPath :: FilePath -> FilePath -> Bool
isWithinPath parent child =
    let p = splitDirectories (normForCmp parent)
        c = splitDirectories (normForCmp child)
     in p == take (length p) c

listFilesH :: AppState -> Maybe Text -> Handler [FileEntry]
listFilesH st mPath = liftIO $ do
    let relPath = maybe "." T.unpack mPath
        requested = stWorkDir st </> relPath

    rootCanon <- canonicalizePath (stWorkDir st)
    pathCanon <- canonicalizePath requested

    if not (isWithinPath rootCanon pathCanon)
        then pure []
        else do
            entries <- listDirectory pathCanon
            fes <- forM (sort entries) $ \name -> do
                let full = pathCanon </> name
                isDir <- doesDirectoryExist full
                pure
                    FileEntry
                        { feName = T.pack name
                        , fePath = T.pack (makeRelative rootCanon full)
                        , feIsDir = isDir
                        }

            let (dirs, files) =
                    foldr (\e (ds, fs) -> if feIsDir e then (e : ds, fs) else (ds, e : fs)) ([], []) fes

            pure (dirs ++ files)

readFileH :: AppState -> Maybe Text -> Handler Text
readFileH st mPath = liftIO $ do
    let relPath = maybe "" T.unpack mPath
        absPath = stWorkDir st </> relPath
    canon <- canonicalizePath absPath
    if not (stWorkDir st `isPrefixOfPath` canon)
        then pure "(access denied)"
        else TIO.readFile canon

-- | Create a new file or directory.
createFileH :: AppState -> CreateFileRequest -> Handler FileEntry
createFileH st (CreateFileRequest relPath content isDir) = liftIO $ do
    let absPath = stWorkDir st </> T.unpack relPath
    canon <- canonicalizePath (takeDirectory absPath)
    if not (stWorkDir st `isPrefixOfPath` canon)
        then pure (FileEntry relPath relPath False)
        else do
            if isDir
                then createDirectoryIfMissing True absPath
                else do
                    createDirectoryIfMissing True (takeDirectory absPath)
                    TIO.writeFile absPath content
            putStrLn $ "[sabela] Created: " ++ absPath
            pure
                FileEntry
                    { feName = T.pack (last (splitPath' (T.unpack relPath)))
                    , fePath = relPath
                    , feIsDir = isDir
                    }
  where
    splitPath' p = case break (== '/') p of
        (a, []) -> [a]
        (a, _ : bs) -> a : splitPath' bs

-- | Write content to an existing file.
writeFileH :: AppState -> WriteFileRequest -> Handler Text
writeFileH st (WriteFileRequest relPath content) = liftIO $ do
    let absPath = stWorkDir st </> T.unpack relPath
    canon <- canonicalizePath (takeDirectory absPath)
    if not (stWorkDir st `isPrefixOfPath` canon)
        then pure "access denied"
        else do TIO.writeFile absPath content; pure "ok"

-- ── IDE: Completion & Info ───────────────────────────────────────

completeH :: AppState -> CompleteRequest -> Handler CompleteResult
completeH st (CompleteRequest prefix) = liftIO $ do
    mSess <- readMVar (stSession st)
    case mSess of
        Nothing -> pure (CompleteResult [])
        Just sess -> do
            cs <- queryComplete sess prefix
            pure (CompleteResult cs)

infoH :: AppState -> InfoRequest -> Handler InfoResult
infoH st (InfoRequest name) = liftIO $ do
    mSess <- readMVar (stSession st)
    case mSess of
        Nothing -> pure (InfoResult "No GHCi session")
        Just sess -> do
            -- Try :info first, fall back to :type, then :doc
            info <- queryInfo sess name
            if T.null info || "not in scope" `T.isInfixOf` T.toLower info
                then do
                    ty <- queryType sess name
                    pure (InfoResult ty)
                else do
                    doc <- queryDoc sess name
                    if T.null doc || "not found" `T.isInfixOf` T.toLower doc
                        then pure (InfoResult info)
                        else pure (InfoResult (info <> "\n\n--- Documentation ---\n" <> doc))

-- ── Examples ─────────────────────────────────────────────────────

examplesH :: Handler [Example]
examplesH = pure builtinExamples

-- ── Helpers ──────────────────────────────────────────────────────

isPrefixOfPath :: FilePath -> FilePath -> Bool
isPrefixOfPath prefix path =
    let p = addTrailingSlash prefix
     in p == take (length p) path || prefix == path
  where
    addTrailingSlash s
        | null s = "/"
        | last s == '/' = s
        | otherwise = s ++ "/"
