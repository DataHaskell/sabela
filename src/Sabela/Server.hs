{-# LANGUAGE OverloadedStrings #-}

{- | Top-level routing + middleware assembly. The Servant types live in
"Sabela.Server.Api"; per-feature handlers live in sibling modules:

* "Sabela.Server.Static" — embedded HTML + assets + uploads.
* "Sabela.Server.Export" — the @\/api\/export\/*@ endpoints.
* "Sabela.Server.Files" — file-explorer endpoints + path safety.
* "Sabela.Server.Notebook" — notebook GET/load/save + cell mutators.
* "Sabela.Server.Run" — cell run/reset/restart + IDE + SSE.
* "Sabela.Server.Ai" — chat lifecycle, AI config, REST bridge.

This module wires those handlers into one 'Application' and owns the
cross-cutting concerns (bearer-token middleware, request-size cap).
-}
module Sabela.Server (
    mkApp,
    newApp,

    -- * Exposed for testing
    checkBearer,
    isAiApi,
    proseMarker,
    cellsToSegments,
    splitProseSegments,
    safeUploadName,
    mimeIndicator,
    textToMime,
) where

import Data.Aeson (encode)
import Data.Bits (xor, (.|.))
import qualified Data.ByteString as BS
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)
import Network.HTTP.Types (
    HeaderName,
    hAuthorization,
    hContentType,
    status401,
 )
import Network.Wai (
    Middleware,
    RequestBodyLength (..),
    pathInfo,
    requestBodyLength,
    requestHeaders,
    responseLBS,
 )
import Servant

import Sabela.Api (errorJson)
import Sabela.Handlers
import Sabela.Server.Ai (
    aiHealthH,
    aiNotebookH,
    aiToolH,
    aiToolsH,
    chatAcceptEditH,
    chatCancelH,
    chatClearH,
    chatMessageH,
    chatRevertEditH,
    getAIConfigH,
    setAIConfigH,
 )
import Sabela.Server.Api (FullAPI, fullProxy)
import Sabela.Server.Export (
    exportDashboardApp,
    exportHaskellApp,
    exportLhsApp,
    exportMarkdownApp,
    exportNotebookApp,
    exportReactiveApp,
    exportSlideshowApp,
 )
import Sabela.Server.Files (
    createFileH,
    deleteFileH,
    listFilesH,
    readFileH,
    readFilePreviewH,
    renameFileH,
    writeFileH,
 )
import Sabela.Server.Import (importUrlApp)
import Sabela.Server.Notebook (
    cellsToSegments,
    deleteCellH,
    getNotebookH,
    insertCellH,
    loadNotebookH,
    mimeIndicator,
    proseMarker,
    saveCellSourceH,
    saveNotebookH,
    splitProseSegments,
    textToMime,
    updateCellH,
 )
import Sabela.Server.Run (
    clearCellH,
    completeH,
    examplesH,
    infoH,
    interruptKernelH,
    resetH,
    restartKernelH,
    runAllH,
    runCellH,
    setCellLangH,
    setWidgetH,
    sseApp,
 )
import Sabela.Server.Static (
    assetApp,
    dashboardApp,
    safeUploadName,
    slideshowApp,
    staticApp,
    uploadApp,
 )
import Sabela.State (App (..), newApp)

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
unchanged (the zero-friction local posture: unauthenticated by default).
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
            (encode (errorJson "Missing or invalid bearer token"))

isAiApi :: [Text] -> Bool
isAiApi ("api" : "ai" : _) = True
isAiApi _ = False

{- | Pure auth check so it can be unit-tested directly. Uses a constant-time
compare so the local AI token can't be recovered via early-mismatch timing.
-}
checkBearer :: Text -> [(HeaderName, BS.ByteString)] -> Bool
checkBearer tok hdrs = case lookup hAuthorization hdrs of
    Just v -> constEqBS v ("Bearer " <> TE.encodeUtf8 tok)
    Nothing -> False

{- | Length-checked, constant-time 'BS.ByteString' equality (no early-mismatch
timing leak once the lengths match).
-}
constEqBS :: BS.ByteString -> BS.ByteString -> Bool
constEqBS a b =
    BS.length a == BS.length b
        && foldl' (.|.) 0 (BS.zipWith xor a b) == (0 :: Word8)

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
        :<|> interruptKernelH app
        :<|> clearCellH app
        :<|> listFilesH app
        :<|> readFileH app
        :<|> readFilePreviewH app
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
        :<|> Tagged (exportSlideshowApp app)
        :<|> Tagged (exportNotebookApp app)
        :<|> Tagged (exportMarkdownApp app)
        :<|> Tagged (exportHaskellApp app)
        :<|> Tagged (exportLhsApp app)
        :<|> Tagged (exportReactiveApp app)
        :<|> Tagged dashboardApp
        :<|> Tagged slideshowApp
        :<|> Tagged (assetApp app)
        :<|> Tagged (uploadApp app)
        :<|> Tagged (importUrlApp app)
        :<|> Tagged staticApp
