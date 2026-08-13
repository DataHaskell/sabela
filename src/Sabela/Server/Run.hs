{-# LANGUAGE OverloadedStrings #-}

module Sabela.Server.Run (
    runCellH,
    runAllH,
    resetH,
    restartKernelH,
    restartRunAllH,
    interruptKernelH,
    kernelStatusH,
    getModeH,
    setModeH,
    clearCellH,
    completeH,
    infoH,
    examplesH,
    setCellLangH,
    setWidgetH,
    sseApp,
    sseHeaders,
) where

import Control.Concurrent.STM (TChan, atomically, readTChan)
import Control.Exception (SomeException, try)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types (HeaderName, hContentType, status200)
import Network.Wai (Application, responseStream)
import Servant (Handler, NoContent (..))

import Sabela.AI.Capabilities.Kernel (kernelStatusValue)
import Sabela.Api
import Sabela.Handlers
import Sabela.Model
import Sabela.Output.Examples (builtinExamples)
import Sabela.Reactivity (RestartMode (..), clearCellResult)
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..), getRunMode)
import Sabela.State.EventBus (subscribeBroadcast)
import Sabela.State.NotebookStore (modifyNotebook, readNotebook)
import Sabela.State.SessionManager (getHaskellSession, getPythonSession)
import Sabela.State.WidgetStore (setWidget)

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

{- | An explicit press of Run is an instruction, not a suggestion, so it forces.
Staleness gating belongs to the reactive path ('rnCellEdit', 'rnRunAll'), which
is what must stay idempotent; skipping here just makes the button do nothing.
-}
runCellH :: ReactiveNotebook -> Int -> Handler RunResult
runCellH rn cid = liftIO $ do
    rnRunCellForced rn cid
    pure (RunResult cid [] Nothing [])

runAllH :: ReactiveNotebook -> Handler RunAllResult
runAllH rn = liftIO $ rnRunAll rn >> pure (RunAllResult [])

resetH :: ReactiveNotebook -> App -> Handler Notebook
resetH rn app =
    liftIO $ rnRestart rn RestartClear >> readNotebook (appNotebook app)

{- | Respawn and run nothing. The previous behaviour re-ran the whole notebook,
so restarting because a cell hung immediately re-ran the hanging cell.
-}
restartKernelH :: ReactiveNotebook -> Handler NoContent
restartKernelH rn = liftIO $ rnRestart rn RestartOnly >> pure NoContent

restartRunAllH :: ReactiveNotebook -> Handler NoContent
restartRunAllH rn = liftIO $ rnRestart rn RestartRunAll >> pure NoContent

{- | Level-triggered resync. @EventSource@ reconnects silently after a sleep, so
a client needs one place to ask what is true now rather than reconstructing it
from events it may have missed.
-}
kernelStatusH :: App -> Handler Value
kernelStatusH = liftIO . kernelStatusValue

getModeH :: App -> Handler RunModeUpdate
getModeH app = liftIO (RunModeUpdate <$> getRunMode app)

setModeH :: App -> ReactiveNotebook -> RunModeUpdate -> Handler RunModeUpdate
setModeH app rn upd = liftIO $ do
    applyRunMode app rn (rmuMode upd)
    pure upd

interruptKernelH :: App -> Handler NoContent
interruptKernelH app = liftIO $ do
    mHs <- getHaskellSession (appSessions app)
    mapM_ ST.sbInterrupt mHs
    mPy <- getPythonSession (appSessions app)
    mapM_ ST.sbInterrupt mPy
    pure NoContent

clearCellH :: App -> Int -> Handler NoContent
clearCellH app cid = liftIO $ do
    modifyNotebook (appNotebook app) $ \nb ->
        nb{nbCells = map clr (nbCells nb)}
    broadcast app (EvCellResult cid [] Nothing [] [])
    pure NoContent
  where
    clr c
        | cellId c == cid = clearCellResult c
        | otherwise = c

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
    broadcast app (EvCellUpdating cid)
    nb <- readNotebook (appNotebook app)
    case lookupCell cid nb of
        Just c -> pure c
        Nothing -> pure (Cell cid CodeCell lang "" [] Nothing True)
  where
    upd c
        | cellId c == cid = (clearCellResult c){cellLang = lang}
        | otherwise = c

setWidgetH :: App -> ReactiveNotebook -> WidgetUpdate -> Handler NoContent
setWidgetH app rn (WidgetUpdate cid name val) = liftIO $ do
    setWidget (appWidgets app) cid name val
    broadcast app (EvWidget cid name val)
    rnWidgetCell rn cid
    pure NoContent
