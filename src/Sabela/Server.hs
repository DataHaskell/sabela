{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Sabela.Server (
    mkApp,
    newApp,

    -- * Exposed for testing
    checkBearer,
    isAiApi,
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (modifyMVar)
import Control.Concurrent.STM (TChan, atomically, readTChan)
import Control.Exception (SomeException, try)
import Control.Monad (forM, forever, void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (..), encode, object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Char
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Foldable (for_)
import Data.List (isPrefixOf, sort)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Network.HTTP.Types (
    HeaderName,
    hAuthorization,
    hContentType,
    status200,
    status401,
 )
import Network.Wai (
    Middleware,
    RequestBodyLength (..),
    pathInfo,
    requestBodyLength,
    requestHeaders,
    responseLBS,
    responseStream,
 )
import Servant
import System.Directory (
    canonicalizePath,
    createDirectoryIfMissing,
    doesDirectoryExist,
    listDirectory,
    removeDirectoryRecursive,
    removeFile,
    renamePath,
 )
import System.FilePath (
    makeRelative,
    normalise,
    splitDirectories,
    takeDirectory,
    (</>),
 )

import Sabela.AI.Capabilities (acceptEdit, chatTools, executeTool, revertEdit)
import Sabela.AI.Doc (defaultDocOpts, renderNotebookDoc)
import Sabela.AI.Orchestrator (
    handleCancelTurn,
    handleChatMessage,
    handleClearChat,
 )
import Sabela.AI.Store (getAIConfig)
import qualified Sabela.AI.Store as AIStore
import Sabela.AI.Types (EditId (..))
import Sabela.Anthropic.Types (
    AnthropicConfig (..),
    ToolDef,
    newCancelToken,
 )
import Sabela.Api
import Sabela.Dashboard (renderStaticDashboard)
import Sabela.Handlers
import Sabela.Model
import Sabela.Output (builtinExamples, parseMimeOutputs)
import qualified Sabela.SessionTypes as ST
import Sabela.State (
    AIConfigUpdate (..),
    App (..),
    broadcastNotebook,
    getAIStore,
    newApp,
    resolveCliHandleStore,
    setAIStore,
    updateAIConfig,
 )
import Sabela.State.Environment (Environment (..))
import Sabela.State.EventBus (subscribeBroadcast)
import Sabela.State.NotebookStore (
    NotebookStore (..),
    freshCellId,
    modifyNotebook,
    readNotebook,
 )
import Sabela.State.SessionManager (getHaskellSession)
import Sabela.State.WidgetStore (setWidget)
import ScriptHs.Markdown (
    CodeOutput (..),
    MimeType (..),
    Segment (..),
    parseMarkdown,
    reassemble,
 )

type JsonAPI =
    "api" :> "notebook" :> Get '[JSON] Notebook
        :<|> "api" :> "load" :> ReqBody '[JSON] LoadRequest :> Post '[JSON] Notebook
        :<|> "api" :> "save" :> ReqBody '[JSON] SaveRequest :> Post '[JSON] Notebook
        :<|> "api"
            :> "cell"
            :> Capture "id" Int
            :> Header "X-Sabela-Session" Text
            :> ReqBody '[JSON] UpdateCell
            :> Put '[JSON] Cell
        :<|> "api"
            :> "cell"
            :> Capture "id" Int
            :> "source"
            :> Header "X-Sabela-Session" Text
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
        :<|> "api" :> "restart-kernel" :> Post '[JSON] NoContent
        :<|> "api" :> "clear" :> Capture "id" Int :> Post '[JSON] NoContent
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
        :<|> "api"
            :> "file"
            :> "delete"
            :> ReqBody '[JSON] DeleteFileRequest
            :> Post '[JSON] NoContent
        :<|> "api"
            :> "file"
            :> "rename"
            :> ReqBody '[JSON] RenameFileRequest
            :> Post '[JSON] NoContent
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
        -- Cell language
        :<|> "api"
            :> "cell"
            :> Capture "id" Int
            :> "lang"
            :> ReqBody '[JSON] ST.CellLang
            :> Put '[JSON] Cell
        -- Widgets
        :<|> "api" :> "widget" :> ReqBody '[JSON] WidgetUpdate :> Post '[JSON] NoContent
        -- AI config
        :<|> "api" :> "config" :> "ai" :> Get '[JSON] Value
        :<|> "api" :> "config" :> "ai" :> ReqBody '[JSON] Value :> Post '[JSON] Value
        -- Chat (AI assistant)
        :<|> "api" :> "chat" :> ReqBody '[JSON] ChatRequest :> Post '[JSON] NoContent
        :<|> "api" :> "chat" :> "cancel" :> Post '[JSON] NoContent
        :<|> "api" :> "chat" :> "clear" :> Post '[JSON] NoContent
        :<|> "api"
            :> "chat"
            :> "edit"
            :> Capture "editId" Int
            :> "accept"
            :> Post '[JSON] (Maybe Cell)
        :<|> "api"
            :> "chat"
            :> "edit"
            :> Capture "editId" Int
            :> "revert"
            :> Post '[JSON] NoContent
        -- AI REST bridge for external CLI clients (e.g., Siza skill).
        :<|> "api" :> "ai" :> "health" :> Get '[JSON] Value
        :<|> "api" :> "ai" :> "tools" :> Get '[JSON] [ToolDef]
        :<|> "api" :> "ai" :> "notebook" :> Get '[JSON] Value
        :<|> "api"
            :> "ai"
            :> "tool"
            :> Header "X-Sabela-Session" Text
            :> ReqBody '[JSON] Value
            :> Post '[JSON] Value

type FullAPI =
    JsonAPI
        :<|> "api" :> "events" :> Raw
        :<|> "api" :> "export" :> "dashboard" :> Raw
        :<|> "api" :> "export" :> "markdown" :> Raw
        :<|> "dashboard" :> Raw
        :<|> Raw

fullProxy :: Proxy FullAPI
fullProxy = Proxy

indexHtml :: BS.ByteString
indexHtml = $(makeRelativeToProject "static/index.html" >>= embedFile)

dashboardHtml :: BS.ByteString
dashboardHtml = $(makeRelativeToProject "static/dashboard.html" >>= embedFile)

staticApp :: Application
staticApp _req resp =
    resp $
        responseLBS
            status200
            [(hContentType, "text/html; charset=utf-8")]
            (LBS.fromStrict indexHtml)

dashboardApp :: Application
dashboardApp _req resp =
    resp $
        responseLBS
            status200
            [(hContentType, "text/html; charset=utf-8")]
            (LBS.fromStrict dashboardHtml)

exportDashboardApp :: App -> Application
exportDashboardApp app _req resp = do
    nb <- readNotebook (appNotebook app)
    let body = renderStaticDashboard dashboardHtml nb
        title = nbTitle nb
        filename = T.takeWhileEnd (/= '/') (T.dropWhileEnd (== '/') title)
        htmlName =
            if T.null filename
                then "dashboard.html"
                else T.replace ".md" ".html" filename
    resp $
        responseLBS
            status200
            [ (hContentType, "text/html; charset=utf-8")
            ,
                ( "Content-Disposition"
                , "attachment; filename=\""
                    <> TE.encodeUtf8 htmlName
                    <> "\""
                )
            ]
            body

exportMarkdownApp :: App -> Application
exportMarkdownApp app _req resp = do
    nb <- readNotebook (appNotebook app)
    let md = reassemble (map cellToSegment (nbCells nb))
        title = nbTitle nb
        filename = T.takeWhileEnd (/= '/') (T.dropWhileEnd (== '/') title)
        mdName =
            if T.null filename
                then "notebook.md"
                else filename
    resp $
        responseLBS
            status200
            [ (hContentType, "text/markdown; charset=utf-8")
            ,
                ( "Content-Disposition"
                , "attachment; filename=\""
                    <> TE.encodeUtf8 mdName
                    <> "\""
                )
            ]
            (LBS.fromStrict (TE.encodeUtf8 md))

{- | Maximum request body size (10 MB). Requests exceeding this are rejected
  with 413 Payload Too Large.
-}
maxBodySize :: Int
maxBodySize = 10 * 1024 * 1024

mkApp :: App -> ReactiveNotebook -> Application
mkApp app rn =
    aiAuthMiddleware (appAiToken app) $
        limitRequestBody maxBodySize $
            serve fullProxy (server app rn)

{- | Gate the @/api/ai/*@ subtree behind bearer auth whenever
@SABELA_AI_TOKEN@ is configured. When unset, all requests pass through
unchanged (matches the local/zero-friction posture described in the plan).
-}
aiAuthMiddleware :: Maybe Text -> Middleware
aiAuthMiddleware Nothing baseApp req sendResp = baseApp req sendResp
aiAuthMiddleware (Just tok) baseApp req sendResp
    | isAiApi (pathInfo req) =
        if checkBearer tok (requestHeaders req)
            then baseApp req sendResp
            else sendResp unauthorized
    | otherwise = baseApp req sendResp
  where
    unauthorized =
        responseLBS
            status401
            [(hContentType, "application/json")]
            (encode (object ["error" .= ("Missing or invalid bearer token" :: Text)]))

isAiApi :: [Text] -> Bool
isAiApi ("api" : "ai" : _) = True
isAiApi _ = False

{- | Pure auth check so it can be unit-tested directly. Compares constant-ish
bytes; not timing-safe, but the token is a local secret, not a password.
-}
checkBearer :: Text -> [(HeaderName, BS.ByteString)] -> Bool
checkBearer tok hdrs = case lookup hAuthorization hdrs of
    Just v -> v == "Bearer " <> TE.encodeUtf8 tok
    Nothing -> False

server :: App -> ReactiveNotebook -> Server FullAPI
server app rn =
    ( getNotebookH app
        :<|> loadNotebookH app rn
        :<|> saveNotebookH app
        :<|> updateCellH app rn
        :<|> saveCellSourceH app
        :<|> insertCellH app
        :<|> deleteCellH app
        :<|> runCellH rn
        :<|> runAllH rn
        :<|> resetH rn app
        :<|> restartKernelH rn
        :<|> clearCellH app
        :<|> listFilesH app
        :<|> readFileH app
        :<|> createFileH app
        :<|> writeFileH app
        :<|> deleteFileH app
        :<|> renameFileH app
        :<|> completeH app
        :<|> infoH app
        :<|> examplesH
        :<|> setCellLangH app
        :<|> setWidgetH app rn
        :<|> getAIConfigH app
        :<|> setAIConfigH app
        :<|> chatMessageH app rn
        :<|> chatCancelH app
        :<|> chatClearH app
        :<|> chatAcceptEditH app rn
        :<|> chatRevertEditH app
        :<|> aiHealthH app
        :<|> aiToolsH
        :<|> aiNotebookH app
        :<|> aiToolH app rn
    )
        :<|> Tagged (sseApp app)
        :<|> Tagged (exportDashboardApp app)
        :<|> Tagged (exportMarkdownApp app)
        :<|> Tagged dashboardApp
        :<|> Tagged staticApp

sseHeaders :: [(HeaderName, BS.ByteString)]
sseHeaders =
    [ (hContentType, "text/event-stream")
    , ("Cache-Control", "no-cache")
    , ("Connection", "keep-alive")
    , ("Access-Control-Allow-Origin", "*")
    ]

streamEvents :: Builder.Builder -> (Builder.Builder -> IO ()) -> IO () -> IO ()
streamEvents firstMsg write flush = do
    write firstMsg
    flush

sseApp :: App -> Application
sseApp app _req resp = do
    chan <- subscribeBroadcast (appEvents app)
    resp $ responseStream status200 sseHeaders $ \write flush -> do
        streamEvents (Builder.byteString ": connected\n\n") write flush
        _ <-
            try (forever $ sendEvent chan write flush) ::
                IO (Either SomeException ())
        pure ()

sendEvent :: TChan NotebookEvent -> (Builder.Builder -> IO ()) -> IO () -> IO ()
sendEvent chan write flush = do
    ev <- atomically $ readTChan chan
    let json = LBS.toStrict (encode ev)
    write (Builder.byteString $ "data: " <> json <> "\n\n")
    flush

getNotebookH :: App -> Handler Notebook
getNotebookH app = liftIO $ readNotebook (appNotebook app)

loadNotebookH :: App -> ReactiveNotebook -> LoadRequest -> Handler Notebook
loadNotebookH app _rn (LoadRequest path) = liftIO $ do
    let absPath = resolveWorkPath (appEnv app) path
    raw <- TIO.readFile absPath
    cells <- mapM (segmentToCell (appNotebook app)) (parseMarkdown raw)
    let nb = Notebook (T.pack path) cells
    -- Cancel any in-flight execution and kill sessions
    void $ bumpGeneration app
    void $ forkIO $ killAllSessions app
    modifyNotebook (appNotebook app) (const nb)
    broadcastNotebook app
    pure nb

resolveWorkPath :: Environment -> FilePath -> FilePath
resolveWorkPath env path
    | "/" `isPrefixOf` path = path
    | otherwise = envWorkDir env </> path

segmentToCell :: NotebookStore -> Segment -> IO Cell
segmentToCell store (Prose t) = do
    nid <- freshCellId store
    pure (Cell nid ProseCell ST.Haskell t [] Nothing False)
segmentToCell store (CodeBlock lang code Nothing) = do
    nid <- freshCellId store
    pure (Cell nid CodeCell (parseLang lang) code [] Nothing False)
segmentToCell store (CodeBlock lang code (Just (CodeOutput m o))) = do
    nid <- freshCellId store
    let items = parseCodeOutputItems m o
    pure (Cell nid CodeCell (parseLang lang) code items Nothing False)

parseCodeOutputItems :: MimeType -> Text -> [OutputItem]
parseCodeOutputItems MimePlain o =
    [ OutputItem mt b
    | (mt, b) <- parseMimeOutputs o
    , not (T.null (T.strip b))
    ]
parseCodeOutputItems m o = [OutputItem (mimeIndicator m) o]

parseLang :: Text -> ST.CellLang
parseLang lang
    | lang `elem` ["python", "python3", "py"] = ST.Python
    | otherwise = ST.Haskell

mimeIndicator :: MimeType -> Text
mimeIndicator m = case m of
    MimeHtml -> "text/html"
    MimeMarkdown -> "text/markdown"
    MimeSvg -> "image/svg+xml"
    MimeLatex -> "text/latex"
    MimeJson -> "application/json"
    MimeImage t -> t <> ";base64"
    MimePlain -> "text/plain"

textToMime :: Text -> MimeType
textToMime m = case m of
    "text/html" -> MimeHtml
    "text/markdown" -> MimeMarkdown
    "image/svg+xml" -> MimeSvg
    "text/latex" -> MimeLatex
    "application/json" -> MimeJson
    -- image isn't covered
    _ -> MimePlain

saveNotebookH :: App -> SaveRequest -> Handler Notebook
saveNotebookH app (SaveRequest mPath) = liftIO $ do
    nb <- readNotebook (appNotebook app)
    let path = fromMaybe (T.unpack (nbTitle nb)) mPath
        absPath = resolveWorkPath (appEnv app) path
        md = reassemble (map cellToSegment (nbCells nb))
    createDirectoryIfMissing True (takeDirectory absPath)
    TIO.writeFile absPath md
    let nb' = nb{nbTitle = T.pack path}
    modifyNotebook (appNotebook app) (const nb')
    putStrLn $ "[sabela] Saved to: " ++ absPath
    pure nb'

cellToSegment :: Cell -> Segment
cellToSegment c = case cellType c of
    ProseCell -> Prose (cellSource c)
    CodeCell -> codeToSegment c

codeToSegment :: Cell -> Segment
codeToSegment c =
    let tag = langTag (cellLang c)
     in case filter (not . T.null . T.strip . oiOutput) (cellOutputs c) of
            [] -> CodeBlock tag (cellSource c) Nothing
            [OutputItem mime o] ->
                CodeBlock tag (cellSource c) (Just (CodeOutput (textToMime mime) o))
            items ->
                CodeBlock
                    tag
                    (cellSource c)
                    (Just (CodeOutput MimePlain (serializeOutputs items)))

langTag :: ST.CellLang -> Text
langTag ST.Haskell = "haskell"
langTag ST.Python = "python"

serializeOutputs :: [OutputItem] -> Text
serializeOutputs items =
    T.concat
        [ "---MIME:" <> mime <> "---\n" <> o
        | OutputItem mime o <- items
        ]

updateCellH ::
    App -> ReactiveNotebook -> Int -> Maybe Text -> UpdateCell -> Handler Cell
updateCellH app rn cid mSession (UpdateCell src) = liftIO $ do
    rnCellEdit rn cid src
    nb <- readNotebook (appNotebook app)
    -- External callers (siza, curl) mark themselves with X-Sabela-Session
    -- so the browser refreshes its editor. Browser keystrokes omit the
    -- header and thus don't echo back as SSE noise.
    for_ mSession (const (broadcastNotebook app))
    case lookupCell cid nb of
        Just c -> pure c
        Nothing -> pure (Cell cid CodeCell ST.Haskell src [] Nothing True)

{- | Save cell source without triggering reactive execution.
Used by runAll to sync editor content before running.
-}
saveCellSourceH :: App -> Int -> Maybe Text -> UpdateCell -> Handler Cell
saveCellSourceH app cid mSession (UpdateCell src) = liftIO $ do
    modifyNotebook (appNotebook app) $ updateCellSource cid src
    nb <- readNotebook (appNotebook app)
    for_ mSession (const (broadcastNotebook app))
    case lookupCell cid nb of
        Just c -> pure c
        Nothing -> pure (Cell cid CodeCell ST.Haskell src [] Nothing True)

insertCellH :: App -> InsertCell -> Handler Cell
insertCellH app (InsertCell afterId typ lang src) = liftIO $ do
    nid <- freshCellId (appNotebook app)
    let cell = Cell nid typ lang src [] Nothing True
    modifyNotebook (appNotebook app) $ \nb ->
        nb{nbCells = ins afterId cell (nbCells nb)}
    broadcastNotebook app
    pure cell
  where
    ins (-1) c cs = c : cs
    ins _ c [] = [c]
    ins aid c (x : xs)
        | cellId x == aid = x : c : xs
        | otherwise = x : ins aid c xs

deleteCellH :: App -> Int -> Handler Notebook
deleteCellH app cid = liftIO $ do
    nb' <- modifyMVar (nsNotebook (appNotebook app)) $ \nb -> do
        let nb'' = nb{nbCells = filter (\c -> cellId c /= cid) (nbCells nb)}
        pure (nb'', nb'')
    broadcastNotebook app
    pure nb'

runCellH :: ReactiveNotebook -> Int -> Handler RunResult
runCellH rn cid = liftIO $ do
    rnRunCell rn cid
    pure (RunResult cid [] Nothing)

runAllH :: ReactiveNotebook -> Handler RunAllResult
runAllH rn = liftIO $ rnRunAll rn >> pure (RunAllResult [])

resetH :: ReactiveNotebook -> App -> Handler Notebook
resetH rn app = liftIO $ rnReset rn >> readNotebook (appNotebook app)

restartKernelH :: ReactiveNotebook -> Handler NoContent
restartKernelH rn = liftIO $ rnRestartKernel rn >> pure NoContent

clearCellH :: App -> Int -> Handler NoContent
clearCellH app cid = liftIO $ do
    modifyNotebook (appNotebook app) $ \nb ->
        nb{nbCells = map clr (nbCells nb)}
    broadcast app (EvCellResult cid [] Nothing [])
    pure NoContent
  where
    clr c
        | cellId c == cid =
            c
                { cellOutputs = []
                , cellError = Nothing
                }
        | otherwise = c

normForCmp :: FilePath -> FilePath
normForCmp = map toLower . normalise

isWithinPath :: FilePath -> FilePath -> Bool
isWithinPath parent child =
    let p = splitDirectories (normForCmp parent)
        c = splitDirectories (normForCmp child)
     in p == take (length p) c

listFilesH :: App -> Maybe Text -> Handler [FileEntry]
listFilesH app mPath = liftIO $ do
    let workDir = envWorkDir (appEnv app)
        requested = workDir </> maybe "." T.unpack mPath
    rootCanon <- canonicalizePath workDir
    pathCanon <- canonicalizePath requested
    if not (isWithinPath rootCanon pathCanon)
        then pure []
        else do
            entries <- listDirectory pathCanon
            fes <- forM (sort entries) (toFileEntry rootCanon pathCanon)
            pure (sortDirsFirst fes)

toFileEntry :: FilePath -> FilePath -> String -> IO FileEntry
toFileEntry rootCanon dirCanon name = do
    let full = dirCanon </> name
    isDir <- doesDirectoryExist full
    pure
        FileEntry
            { feName = T.pack name
            , fePath = T.pack (makeRelative rootCanon full)
            , feIsDir = isDir
            }

sortDirsFirst :: [FileEntry] -> [FileEntry]
sortDirsFirst fes =
    let (dirs, files) =
            foldr
                (\e (ds, fs) -> if feIsDir e then (e : ds, fs) else (ds, e : fs))
                ([], [])
                fes
     in dirs ++ files

readFileH :: App -> Maybe Text -> Handler Text
readFileH app mPath = liftIO $ do
    let workDir = envWorkDir (appEnv app)
        relPath = maybe "" T.unpack mPath
        absPath = workDir </> relPath
    canon <- canonicalizePath absPath
    if not (workDir `isPrefixOfPath` canon)
        then pure "(access denied)"
        else TIO.readFile canon

createFileH :: App -> CreateFileRequest -> Handler FileEntry
createFileH app (CreateFileRequest relPath content isDir) = liftIO $ do
    let workDir = envWorkDir (appEnv app)
        absPath = workDir </> T.unpack relPath
    canon <- canonicalizePath (takeDirectory absPath)
    if not (workDir `isPrefixOfPath` canon)
        then pure (FileEntry relPath relPath False)
        else do
            createFileOrDir absPath content isDir
            pure (mkFileEntry relPath isDir)

createFileOrDir :: FilePath -> Text -> Bool -> IO ()
createFileOrDir absPath content isDir = do
    if isDir
        then createDirectoryIfMissing True absPath
        else do
            createDirectoryIfMissing True (takeDirectory absPath)
            TIO.writeFile absPath content
    putStrLn $ "[sabela] Created: " ++ absPath

mkFileEntry :: Text -> Bool -> FileEntry
mkFileEntry relPath isDir =
    FileEntry
        { feName = T.pack (last (splitPath' (T.unpack relPath)))
        , fePath = relPath
        , feIsDir = isDir
        }
  where
    splitPath' p = case break (== '/') p of
        (a, []) -> [a]
        (a, _ : bs) -> a : splitPath' bs

writeFileH :: App -> WriteFileRequest -> Handler Text
writeFileH app (WriteFileRequest relPath content) = liftIO $ do
    let workDir = envWorkDir (appEnv app)
        absPath = workDir </> T.unpack relPath
    canon <- canonicalizePath (takeDirectory absPath)
    if not (workDir `isPrefixOfPath` canon)
        then pure "access denied"
        else do TIO.writeFile absPath content; pure "ok"

deleteFileH :: App -> DeleteFileRequest -> Handler NoContent
deleteFileH app (DeleteFileRequest relPath) = liftIO $ do
    let workDir = envWorkDir (appEnv app)
        absPath = workDir </> T.unpack relPath
    canon <- canonicalizePath absPath
    if not (workDir `isPrefixOfPath` canon) || canon == workDir
        then pure NoContent
        else do
            isDir <- doesDirectoryExist canon
            if isDir
                then removeDirectoryRecursive canon
                else removeFile canon
            pure NoContent

renameFileH :: App -> RenameFileRequest -> Handler NoContent
renameFileH app (RenameFileRequest oldRelPath newRelPath) = liftIO $ do
    let workDir = envWorkDir (appEnv app)
        oldAbs = workDir </> T.unpack oldRelPath
        newAbs = workDir </> T.unpack newRelPath
    oldCanon <- canonicalizePath oldAbs
    -- For new path, canonicalize the parent (the file doesn't exist yet)
    let newParent = takeDirectory newAbs
    newParentCanon <- canonicalizePath newParent
    if not (workDir `isPrefixOfPath` oldCanon)
        || not (workDir `isPrefixOfPath` newParentCanon)
        then pure NoContent
        else do renamePath oldAbs newAbs; pure NoContent

completeH :: App -> CompleteRequest -> Handler CompleteResult
completeH app (CompleteRequest prefix) = liftIO $ do
    mSess <- getHaskellSession (appSessions app)
    case mSess of
        Nothing -> pure (CompleteResult [])
        Just backend -> do
            cs <- ST.sbQueryComplete backend prefix
            pure (CompleteResult cs)

infoH :: App -> InfoRequest -> Handler InfoResult
infoH app (InfoRequest name) = liftIO $ do
    mSess <- getHaskellSession (appSessions app)
    case mSess of
        Nothing -> pure (InfoResult "No GHCi session")
        Just backend -> do
            info <- ST.sbQueryInfo backend name
            queryWithFallback backend name info

queryWithFallback :: ST.SessionBackend -> Text -> Text -> IO InfoResult
queryWithFallback backend name info
    | T.null info || "not in scope" `T.isInfixOf` T.toLower info = do
        ty <- ST.sbQueryType backend name
        pure (InfoResult ty)
    | otherwise = appendDoc backend name info

appendDoc :: ST.SessionBackend -> Text -> Text -> IO InfoResult
appendDoc backend name info = do
    doc <- ST.sbQueryDoc backend name
    if T.null doc || "not found" `T.isInfixOf` T.toLower doc
        then pure (InfoResult info)
        else pure (InfoResult (info <> "\n\n--- Documentation ---\n" <> doc))

examplesH :: Handler [Example]
examplesH = pure builtinExamples

setCellLangH :: App -> Int -> ST.CellLang -> Handler Cell
setCellLangH app cid lang = liftIO $ do
    modifyNotebook (appNotebook app) $ \nb ->
        nb{nbCells = map upd (nbCells nb)}
    broadcastNotebook app
    nb <- readNotebook (appNotebook app)
    case lookupCell cid nb of
        Just c -> pure c
        Nothing -> pure (Cell cid CodeCell lang "" [] Nothing True)
  where
    upd c
        | cellId c == cid = c{cellLang = lang, cellOutputs = [], cellError = Nothing}
        | otherwise = c

setWidgetH :: App -> ReactiveNotebook -> WidgetUpdate -> Handler NoContent
setWidgetH app rn (WidgetUpdate cid name val) = liftIO $ do
    setWidget (appWidgets app) cid name val
    rnWidgetCell rn cid
    pure NoContent

{- | Check whether @path@ is within @prefix@ using path component comparison.
  This avoids false positives like @/home/alice@ matching @/home/alice-secret@.
-}
isPrefixOfPath :: FilePath -> FilePath -> Bool
isPrefixOfPath prefix path =
    let prefixParts = splitDirectories (normalise prefix)
        pathParts = splitDirectories (normalise path)
     in prefixParts `isPrefixOf` pathParts

limitRequestBody :: Int -> Application -> Application
limitRequestBody sizeLimit innerApp req sendResp = do
    case requestBodyLength req of
        KnownLength len
            | fromIntegral len > sizeLimit ->
                sendResp $
                    responseLBS status413 [(hContentType, "text/plain")] "Request body too large"
        _ -> innerApp req sendResp
  where
    status413 = toEnum 413

------------------------------------------------------------------------
-- AI Config handlers
------------------------------------------------------------------------

getAIConfigH :: App -> Handler Value
getAIConfigH app = liftIO $ do
    mStore <- getAIStore app
    case mStore of
        Nothing ->
            pure $
                object
                    [ "configured" .= False
                    , "model" .= (Nothing :: Maybe Text)
                    , "models" .= knownModels
                    ]
        Just store -> do
            cfg <- getAIConfig store
            pure $
                object
                    [ "configured" .= True
                    , "model" .= acModel cfg
                    , "models" .= knownModels
                    ]

-- | Suggested model IDs shown in the picker. Custom values are also accepted.
knownModels :: [Value]
knownModels =
    [ modelEntry
        "claude-haiku-4-5-20251001"
        "Haiku 4.5"
        "Fast + cheap; best for high-frequency iteration"
    , modelEntry
        "claude-sonnet-4-6"
        "Sonnet 4.6"
        "Recommended balance of speed and capability"
    , modelEntry
        "claude-opus-4-7"
        "Opus 4.7"
        "Most capable; slower; use for hard reasoning"
    , modelEntry "claude-sonnet-4-20250514" "Sonnet 4 (legacy)" "Original default"
    ]
  where
    modelEntry :: Text -> Text -> Text -> Value
    modelEntry mid label desc =
        object ["id" .= mid, "label" .= label, "description" .= desc]

setAIConfigH :: App -> Value -> Handler Value
setAIConfigH app (Object o) = liftIO $ do
    let mKey = case KM.lookup (Key.fromText "apiKey") o of
            Just (String s) | not (T.null s) -> Just s
            _ -> Nothing
        mModel = case KM.lookup (Key.fromText "model") o of
            Just (String s) | not (T.null s) -> Just s
            _ -> Nothing
    if isNothing mKey && isNothing mModel
        then pure $ object ["error" .= ("apiKey or model is required" :: Text)]
        else do
            result <-
                updateAIConfig
                    app
                    AIConfigUpdate{aicuApiKey = mKey, aicuModel = mModel}
            case result of
                Right () -> do
                    mStore <- getAIStore app
                    currentModel <- case mStore of
                        Just store -> Just . acModel <$> getAIConfig store
                        Nothing -> pure Nothing
                    pure $
                        object
                            [ "configured" .= True
                            , "model" .= currentModel
                            ]
                Left err -> pure $ object ["error" .= err]
setAIConfigH _ _ = pure $ object ["error" .= ("Invalid request body" :: Text)]

------------------------------------------------------------------------
-- Chat (AI assistant) handlers
------------------------------------------------------------------------

chatMessageH :: App -> ReactiveNotebook -> ChatRequest -> Handler NoContent
chatMessageH app rn (ChatRequest msg) = liftIO $ do
    mStore <- getAIStore app
    case mStore of
        Nothing ->
            broadcast
                app
                (EvChatError 0 "AI not configured. Open the Chat panel to set your API key.")
        Just store ->
            handleChatMessage app store rn msg
    pure NoContent

chatCancelH :: App -> Handler NoContent
chatCancelH app = liftIO $ do
    mStore <- getAIStore app
    for_ mStore (handleCancelTurn app)
    pure NoContent

chatClearH :: App -> Handler NoContent
chatClearH app = liftIO $ do
    mStore <- getAIStore app
    for_ mStore (handleClearChat app)
    pure NoContent

chatAcceptEditH :: App -> ReactiveNotebook -> Int -> Handler (Maybe Cell)
chatAcceptEditH app rn editIdInt = liftIO $ do
    mStore <- getAIStore app
    case mStore of
        Nothing -> pure Nothing
        Just store -> acceptEdit app store rn (EditId editIdInt)

chatRevertEditH :: App -> Int -> Handler NoContent
chatRevertEditH app editIdInt = liftIO $ do
    mStore <- getAIStore app
    case mStore of
        Nothing -> pure ()
        Just store -> revertEdit app store (EditId editIdInt)
    pure NoContent

------------------------------------------------------------------------
-- AI REST bridge (for external CLI skills — e.g. Siza)
------------------------------------------------------------------------

aiHealthH :: App -> Handler Value
aiHealthH app =
    pure $
        object
            [ "ok" .= True
            , "workDir" .= envWorkDir (appEnv app)
            , "authRequired" .= isJust (appAiToken app)
            ]

aiToolsH :: Handler [ToolDef]
aiToolsH = pure chatTools

aiNotebookH :: App -> Handler Value
aiNotebookH app = liftIO $ do
    nb <- readNotebook (appNotebook app)
    pure (renderNotebookDoc defaultDocOpts nb)

{- | Invoke a single AI tool from an external CLI client. Body: @{ name, input }@.
An optional @X-Sabela-Session@ header isolates @explore_result@ handles
between concurrent clients while they still see the same notebook.
-}
aiToolH ::
    App ->
    ReactiveNotebook ->
    Maybe Text ->
    Value ->
    Handler Value
aiToolH app rn mSession body = liftIO $ do
    let name = fromMaybe "" (stringField "name" body)
        input = fromMaybe (object []) (valueField "input" body)
    mStore <- ensureAIStoreForTools app
    case mStore of
        Nothing ->
            pure $
                object
                    [ "isError" .= True
                    , "result"
                        .= object
                            [ "error"
                                .= ( "Cannot execute tools: no HTTP manager available. Start sabela normally via cabal run." ::
                                        Text
                                   )
                            ]
                    ]
        Just store -> do
            storeForCall <- case mSession of
                Nothing -> pure store
                Just sid -> do
                    hs <- resolveCliHandleStore app sid
                    pure store{AIStore.aiHandles = hs}
            cancelTok <- newCancelToken
            (result, isError) <- executeTool app storeForCall rn cancelTok name input
            pure $ object ["isError" .= isError, "result" .= result]

{- | Ensure an AIStore exists. The browser path requires a real API key for
Anthropic access, but the REST bridge only uses the store as a plumbing
object — handles, scratchpad, pending edits. If no store exists yet, we
build one with a placeholder config so read-only/tool-only flows work even
before the user configures a key.
-}
ensureAIStoreForTools :: App -> IO (Maybe AIStore.AIStore)
ensureAIStoreForTools app = do
    mStore <- getAIStore app
    case mStore of
        Just s -> pure (Just s)
        Nothing -> case appHttpMgr app of
            Nothing -> pure Nothing
            Just mgr -> do
                let cfg =
                        AnthropicConfig
                            { acApiKey = ""
                            , acModel = "placeholder"
                            , acBaseUrl = "https://api.anthropic.com"
                            }
                store <- AIStore.newAIStore cfg mgr
                setAIStore app (Just store)
                pure (Just store)

stringField :: Text -> Value -> Maybe Text
stringField k v = case valueField k v of
    Just (String s) -> Just s
    _ -> Nothing

valueField :: Text -> Value -> Maybe Value
valueField k (Object o) = KM.lookup (Key.fromText k) o
valueField _ _ = Nothing
