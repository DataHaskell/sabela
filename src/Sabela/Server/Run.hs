{-# LANGUAGE OverloadedStrings #-}

{- | Small handlers + the SSE event stream that live alongside the
top-level router: cell run/run-all/reset/restart/clear, IDE complete +
info, cell-language change, widget value update, and the @/api/examples@
catalogue. Kept together because each is a few-line wrapper over an
existing service and they share imports.
-}
module Sabela.Server.Run (
    -- * Cell lifecycle
    runCellH,
    runAllH,
    resetH,
    restartKernelH,
    clearCellH,

    -- * IDE
    completeH,
    infoH,

    -- * Misc
    examplesH,
    setCellLangH,
    setWidgetH,

    -- * SSE event stream
    sseApp,
    sseHeaders,
) where

import Control.Concurrent.STM (TChan, atomically, readTChan)
import Control.Exception (SomeException, try)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types (HeaderName, hContentType, status200)
import Network.Wai (Application, responseStream)
import Servant (Handler, NoContent (..))

import Sabela.Api
import Sabela.Handlers
import Sabela.Model
import Sabela.Output.Examples (builtinExamples)
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..))
import Sabela.State.EventBus (subscribeBroadcast)
import Sabela.State.NotebookStore (modifyNotebook, readNotebook)
import Sabela.State.SessionManager (getHaskellSession)
import Sabela.State.WidgetStore (setWidget)

------------------------------------------------------------------------
-- SSE event stream
------------------------------------------------------------------------

sseHeaders :: [(HeaderName, BS.ByteString)]
sseHeaders =
    [ (hContentType, "text/event-stream")
    , ("Cache-Control", "no-cache")
    , ("Connection", "keep-alive")
    , ("Access-Control-Allow-Origin", "*")
    ]

sseApp :: App -> Application
sseApp app _req resp = do
    chan <- subscribeBroadcast (appEvents app)
    resp $ responseStream status200 sseHeaders $ \write flush -> do
        write (Builder.byteString ": connected\n\n")
        flush
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

------------------------------------------------------------------------
-- Cell lifecycle
------------------------------------------------------------------------

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

------------------------------------------------------------------------
-- IDE
------------------------------------------------------------------------

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

------------------------------------------------------------------------
-- Misc
------------------------------------------------------------------------

examplesH :: Handler [Example]
examplesH = pure builtinExamples

setCellLangH :: App -> Int -> ST.CellLang -> Handler Cell
setCellLangH app cid lang = liftIO $ do
    modifyNotebook (appNotebook app) $ \nb ->
        nb{nbCells = map upd (nbCells nb)}
    broadcast app (EvCellUpdating cid)
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
