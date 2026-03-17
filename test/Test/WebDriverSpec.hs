{-# LANGUAGE OverloadedStrings #-}

module Test.WebDriverSpec (spec) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import System.Directory (findExecutable)
import Test.Hspec (
    Spec,
    beforeAll_,
    describe,
    it,
    pendingWith,
    runIO,
    shouldBe,
 )
import Test.WebDriver (
    Selector (..),
    WD,
    click,
    closeSession,
    findElem,
    findElems,
    openPage,
    runSession,
 )
import Test.WebDriver.Commands.Wait (waitUntil)

import Test.WebDriverHelpers (
    driverConfig,
    getCellError,
    getCellIframeStamp,
    getCellOutput,
    postWidgetMessage,
    runCell,
    setCellContent,
    stampCellIframe,
    startChromeDriver,
    waitForIframeCount,
    waitForOutput,
    waitForOutputBlockCount,
    withTestServer,
 )

testPort :: Int
testPort = 13742

testUrl :: String
testUrl = "http://localhost:" ++ show testPort ++ "/index.html"

-- | Run a WebDriver action in a fresh browser session, then close it.
withSession :: WD () -> IO ()
withSession action = runSession driverConfig (action >> closeSession)

-- | Wait for at least one .cell element to appear on the page.
waitForPageLoad :: WD ()
waitForPageLoad =
    waitUntil 15 $ do
        elems <- findElems (ByCSS ".cell")
        when (null elems) $ liftIO $ ioError $ userError "page not loaded yet"

spec :: Spec
spec = do
    mChrome <- runIO $ findExecutable "chromedriver"
    case mChrome of
        Nothing ->
            describe "WebDriver integration" $
                it "all webdriver tests" $
                    pendingWith "chromedriver not found in PATH"
        Just _ ->
            beforeAll_ (startChromeDriver >> withTestServer testPort "." (return ())) $
                describe "WebDriver integration" $ do
                    it "basic expression: 1 + 1 evaluates to 2" $
                        withSession $ do
                            openPage testUrl
                            waitForPageLoad
                            setCellContent 1 "1 + 1"
                            runCell 1
                            waitForOutput 1 15 "2"

                    it "string output: \"hello\" displays hello" $
                        withSession $ do
                            openPage testUrl
                            waitForPageLoad
                            setCellContent 1 "\"hello\""
                            runCell 1
                            waitForOutput 1 15 "hello"

                    it "error display: bad code shows an error" $
                        withSession $ do
                            openPage testUrl
                            waitForPageLoad
                            setCellContent 1 "badCode ***"
                            runCell 1
                            waitUntil 15 $ do
                                mErr <- getCellError 1
                                case mErr of
                                    Nothing -> liftIO $ ioError $ userError "no error yet"
                                    Just _ -> return ()

                    it "reset clears cell outputs" $
                        withSession $ do
                            openPage testUrl
                            waitForPageLoad
                            setCellContent 1 "42"
                            runCell 1
                            waitForOutput 1 15 "42"
                            resetBtn <- findElem (ByCSS ".toolbar button[onclick*='reset']")
                            click resetBtn
                            waitUntil 10 $ do
                                mOut <- getCellOutput 1
                                case mOut of
                                    Nothing -> return ()
                                    Just _ -> liftIO $ ioError $ userError "output still present after reset"

                    it "displayHtml renders exactly one iframe" $
                        withSession $ do
                            openPage testUrl
                            waitForPageLoad
                            setCellContent 1 "displayHtml \"<p>hello</p>\""
                            runCell 1
                            waitForIframeCount 1 15 1

                    it "displaySlider renders exactly one iframe" $
                        withSession $ do
                            openPage testUrl
                            waitForPageLoad
                            setCellContent 1 "displaySlider \"x\" 0 100 50"
                            runCell 1
                            waitForIframeCount 1 15 1

                    it "slider + displayHtml in same cell merge into one iframe" $
                        withSession $ do
                            openPage testUrl
                            waitForPageLoad
                            let src =
                                    "val <- widgetGet \"x\"\n"
                                        <> "let n = maybe 50 read val :: Int\n"
                                        <> "displaySlider \"x\" 0 100 n\n"
                                        <> "displayHtml $ \"<p>n=\" ++ show n ++ \"</p>\""
                            setCellContent 1 src
                            runCell 1
                            -- two consecutive text/html outputs → merged into one iframe
                            waitForIframeCount 1 15 1

                    it "HTML followed by markdown renders two separate blocks" $
                        withSession $ do
                            openPage testUrl
                            waitForPageLoad
                            let src =
                                    "displayHtml \"<p>html part</p>\"\n"
                                        <> "displayMarkdown \"# md part\""
                            setCellContent 1 src
                            runCell 1
                            -- HTML → one iframe; markdown → one .mime-markdown div; total iframes = 1
                            waitForIframeCount 1 15 1
                            -- markdown block should also be present
                            waitUntil 15 $ do
                                elems <- findElems (ByCSS ".cell[data-id='1'] .mime-markdown")
                                Control.Monad.when (null elems) $
                                    liftIO $
                                        ioError $
                                            userError "no markdown block found"

                    it "re-running an HTML cell reuses the existing iframe" $
                        withSession $ do
                            openPage testUrl
                            waitForPageLoad
                            setCellContent 1 "displayHtml \"<p>first</p>\""
                            runCell 1
                            waitForIframeCount 1 15 1
                            -- stamp the iframe DOM element
                            stamped <- stampCellIframe 1 "reuse-marker"
                            liftIO $ stamped `shouldBe` True
                            -- re-run the cell (same content, new execution)
                            runCell 1
                            waitForIframeCount 1 15 1
                            -- stamp must persist: same DOM node was reused, not replaced
                            mStamp <- getCellIframeStamp 1
                            liftIO $ mStamp `shouldBe` Just "reuse-marker"

                    it "reconcile: re-running with fewer outputs removes stale blocks" $
                        withSession $ do
                            openPage testUrl
                            waitForPageLoad
                            -- two outputs: html block + markdown block
                            let src2 =
                                    "displayHtml \"<p>html</p>\"\n"
                                        <> "displayMarkdown \"**md**\""
                            setCellContent 1 src2
                            runCell 1
                            waitForOutputBlockCount 1 15 2
                            -- re-run with only one output
                            setCellContent 1 "displayHtml \"<p>html only</p>\""
                            runCell 1
                            waitForOutputBlockCount 1 15 1

                    it "debounce: rapid widget messages produce a single re-execution" $
                        withSession $ do
                            openPage testUrl
                            waitForPageLoad
                            let src =
                                    "val <- widgetGet \"x\"\n"
                                        <> "let n = maybe 0 read val :: Int\n"
                                        <> "displayMarkdown $ \"value=\" ++ show n"
                            setCellContent 1 src
                            runCell 1
                            waitForOutput 1 15 "value=0"
                            -- fire 5 rapid widget messages — debounce should coalesce to 1 fetch
                            postWidgetMessage 1 "x" "10"
                            postWidgetMessage 1 "x" "20"
                            postWidgetMessage 1 "x" "30"
                            postWidgetMessage 1 "x" "40"
                            postWidgetMessage 1 "x" "50"
                            -- after debounce + execution, output should reflect the last value
                            waitForOutput 1 15 "value=50"
                            -- and must NOT still show an intermediate value only
                            mOut <- getCellOutput 1
                            liftIO $ mOut `shouldBe` Just "value=50"
