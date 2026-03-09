{-# LANGUAGE OverloadedStrings #-}

module Test.WebDriverSpec (spec) where

import Control.Monad.IO.Class (liftIO)
import System.Directory (findExecutable)
import Test.Hspec (
    Spec,
    beforeAll_,
    describe,
    it,
    pendingWith,
    runIO,
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
    getCellOutput,
    runCell,
    setCellContent,
    startChromeDriver,
    waitForOutput,
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
        if null elems
            then liftIO $ ioError $ userError "page not loaded yet"
            else return ()

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
