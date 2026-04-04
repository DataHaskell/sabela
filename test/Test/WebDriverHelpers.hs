{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.WebDriverHelpers (
    startChromeDriver,
    withTestServer,
    driverConfig,
    setCellContent,
    runCell,
    waitForOutput,
    getCellOutput,
    getCellError,
    countIframesInCell,
    waitForIframeCount,
    stampCellIframe,
    getCellIframeStamp,
    countOutputBlocks,
    waitForOutputBlockCount,
    postWidgetMessage,
) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client (
    defaultManagerSettings,
    httpNoBody,
    newManager,
    parseRequest,
    responseStatus,
 )
import Network.HTTP.Types (status200)
import Network.Socket (
    AddrInfo (..),
    SocketType (..),
    close,
    connect,
    defaultHints,
    defaultProtocol,
    getAddrInfo,
    socket,
 )
import Network.Wai.Handler.Warp (run)
import System.IO (hPutStrLn, stderr)
import System.Process (spawnProcess, waitForProcess)
import Test.WebDriver (
    Selector (..),
    WD,
    WDConfig (..),
    click,
    defaultConfig,
    findElem,
    findElems,
    getText,
 )
import Test.WebDriver.Commands (JSArg (..), executeJS)
import Test.WebDriver.Commands.Wait (waitUntil)

import qualified Data.Set as Set
import Sabela.Handlers (setupReactive)
import Sabela.Server (mkApp, newApp)

{- | Start the Sabela server in-process on the given port.
The server runs in a forked thread; this function returns once it is ready.
-}
withTestServer :: Int -> FilePath -> IO () -> IO ()
withTestServer port workDir action = do
    app <- newApp workDir Set.empty Nothing
    rn <- setupReactive app
    _ <- forkIO $ run port (mkApp app rn)
    waitForServer port
    action

-- | Poll until the server responds 200 OK on /api/notebook.
waitForServer :: Int -> IO ()
waitForServer port = go (50 :: Int)
  where
    go 0 = hPutStrLn stderr "Warning: server did not become ready in time"
    go n = do
        result <- try checkServer :: IO (Either SomeException ())
        case result of
            Right () -> return ()
            Left _ -> threadDelay 100_000 >> go (n - 1)
    checkServer = do
        mgr <- newManager defaultManagerSettings
        req <- parseRequest $ "http://localhost:" ++ show port ++ "/api/notebook"
        resp <- httpNoBody req mgr
        if responseStatus resp == status200
            then return ()
            else ioError (userError "not ready")

{- | Spawn ChromeDriver on port 9515 and wait until it accepts connections.
The process is killed when the surrounding bracket exits.
-}
startChromeDriver :: IO ()
startChromeDriver = do
    ph <- spawnProcess "chromedriver" ["--port=9515"]
    waitForChromeDriver
    -- Register cleanup to run at process exit (best-effort for tests)
    _ <- forkIO $ void (waitForProcess ph)
    return ()

-- | Poll until ChromeDriver port 9515 accepts connections.
waitForChromeDriver :: IO ()
waitForChromeDriver = go (50 :: Int)
  where
    go 0 = hPutStrLn stderr "Warning: chromedriver did not become ready in time"
    go n = do
        result <- try checkPort :: IO (Either SomeException ())
        case result of
            Right () -> return ()
            Left _ -> threadDelay 100_000 >> go (n - 1)
    checkPort = do
        let hints = defaultHints{addrSocketType = Stream}
        addrs <- getAddrInfo (Just hints) (Just "127.0.0.1") (Just "9515")
        case addrs of
            [] -> ioError (userError "no addr")
            (addr : _) -> do
                sock <- socket (addrFamily addr) Stream defaultProtocol
                connect sock (addrAddress addr)
                close sock

-- | WebDriver session config pointing to local ChromeDriver on port 9515.
driverConfig :: WDConfig
driverConfig =
    defaultConfig
        { wdHost = "localhost"
        , wdPort = 9515
        }

-- | Set the content of a CodeMirror cell by cell ID using JS execution.
setCellContent :: Int -> Text -> WD ()
setCellContent cid src = do
    let sel = ".cell[data-id='" <> T.pack (show cid) <> "'] .CodeMirror" :: Text
    _ <-
        executeJS
            [JSArg sel, JSArg src]
            "document.querySelector(arguments[0]).CodeMirror.setValue(arguments[1])" ::
            WD Text
    return ()

-- | Click the Run button for the given cell ID.
runCell :: Int -> WD ()
runCell cid = do
    let sel = ByCSS $ ".cell[data-id='" <> T.pack (show cid) <> "'] .run-btn"
    btn <- findElem sel
    click btn

{- | Wait up to @timeoutSecs@ seconds for cell @cid@'s output to contain @expected@.
Throws inside 'waitUntil' on each failed attempt so that polling retries.
-}
waitForOutput :: Int -> Int -> Text -> WD ()
waitForOutput cid timeoutSecs expected =
    waitUntil (fromIntegral timeoutSecs) $ do
        mOut <- getCellOutput cid
        case mOut of
            Nothing -> liftIO $ ioError $ userError "no output yet"
            Just txt
                | T.isInfixOf expected txt -> return ()
                | otherwise ->
                    liftIO $
                        ioError $
                            userError $
                                "output "
                                    ++ T.unpack txt
                                    ++ " does not contain "
                                    ++ T.unpack expected

-- | Get the text content of a cell's output element, or Nothing if absent/empty.
getCellOutput :: Int -> WD (Maybe Text)
getCellOutput cid = do
    let sel = ByCSS $ ".cell[data-id='" <> T.pack (show cid) <> "'] .cell-output"
    elems <- findElems sel
    case elems of
        [] -> return Nothing
        (el : _) -> do
            txt <- getText el
            if T.null (T.strip txt) then return Nothing else return (Just txt)

-- | Get the error text for a cell (checks .cell-output.error), or Nothing.
getCellError :: Int -> WD (Maybe Text)
getCellError cid = do
    let sel = ByCSS $ ".cell[data-id='" <> T.pack (show cid) <> "'] .cell-output.error"
    elems <- findElems sel
    case elems of
        [] -> return Nothing
        (el : _) -> do
            txt <- getText el
            if T.null (T.strip txt) then return Nothing else return (Just txt)

-- | Count the number of iframes rendered inside a cell's output area.
countIframesInCell :: Int -> WD Int
countIframesInCell cid = do
    let sel = ByCSS $ ".cell[data-id='" <> T.pack (show cid) <> "'] .cell-output iframe"
    elems <- findElems sel
    return (length elems)

-- | Wait until a cell has exactly @n@ iframes in its output.
waitForIframeCount :: Int -> Int -> Int -> WD ()
waitForIframeCount cid timeoutSecs n =
    waitUntil (fromIntegral timeoutSecs) $ do
        count <- countIframesInCell cid
        if count == n
            then return ()
            else
                liftIO $
                    ioError $
                        userError $
                            "expected " ++ show n ++ " iframe(s), got " ++ show count

{- | Stamp the first iframe in a cell's output with a test marker.
Returns True if an iframe was found and stamped.
-}
stampCellIframe :: Int -> Text -> WD Bool
stampCellIframe cid stamp = do
    let sel = ".cell[data-id='" <> T.pack (show cid) <> "'] .cell-output iframe" :: Text
    executeJS
        [JSArg sel, JSArg stamp]
        "var el=document.querySelector(arguments[0]); if(!el) return false; el.setAttribute('data-stamp',arguments[1]); return true;"

-- | Read the stamp attribute from the first iframe in a cell's output.
getCellIframeStamp :: Int -> WD (Maybe Text)
getCellIframeStamp cid = do
    let sel = ".cell[data-id='" <> T.pack (show cid) <> "'] .cell-output iframe" :: Text
    executeJS
        [JSArg sel]
        "var el=document.querySelector(arguments[0]); return el ? el.getAttribute('data-stamp') : null;"

-- | Count non-error output blocks in a cell's output area.
countOutputBlocks :: Int -> WD Int
countOutputBlocks cid = do
    let sel =
            ByCSS $
                ".cell[data-id='"
                    <> T.pack (show cid)
                    <> "'] .cell-output > div:not(.output-error-trail)"
    elems <- findElems sel
    return (length elems)

-- | Wait until a cell has exactly @n@ output blocks.
waitForOutputBlockCount :: Int -> Int -> Int -> WD ()
waitForOutputBlockCount cid timeoutSecs n =
    waitUntil (fromIntegral timeoutSecs) $ do
        count <- countOutputBlocks cid
        if count == n
            then return ()
            else
                liftIO $
                    ioError $
                        userError $
                            "expected " ++ show n ++ " output block(s), got " ++ show count

-- | Post a synthetic widget postMessage to the page (simulates an iframe slider).
postWidgetMessage :: Int -> Text -> Text -> WD ()
postWidgetMessage cid name value = do
    _ <-
        executeJS
            [JSArg (T.pack (show cid) :: Text), JSArg name, JSArg value]
            "window.postMessage({type:'widget',cellId:parseInt(arguments[0]),name:arguments[1],value:arguments[2]},'*');" ::
            WD Text
    return ()
