{-# LANGUAGE OverloadedStrings #-}

module Test.TransportFailureSpec (transportFailureSpec) where

import Control.Exception (toException)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client (
    HttpException (..),
    HttpExceptionContent (..),
    parseRequest_,
 )
import Test.Hspec

import Siza.Transport.Failure (
    FailureClass (..),
    ToolFailure (..),
    classifyDecode,
    classifyException,
    classifyStatus,
    classifyTransport,
    renderFailure,
 )

-- | Strings the 2026-07-18 transcripts leaked and must never leak again.
banned :: [Text]
banned = ["HttpExceptionRequest", "Trailing garbage", "Error in $"]

wellFormed :: ToolFailure -> Expectation
wellFormed tf = do
    let r = renderFailure tf
    r `shouldSatisfy` (not . T.isInfixOf "\n")
    T.length r `shouldSatisfy` (<= 400)
    mapM_ (\b -> r `shouldSatisfy` (not . T.isInfixOf b)) banned

httpEx :: HttpExceptionContent -> HttpException
httpEx = HttpExceptionRequest (parseRequest_ "http://localhost:3101/api/ai/tool")

transportFailureSpec :: Spec
transportFailureSpec = describe "Siza.Transport.Failure (R6.3/R6.9)" $ do
    describe "general invariant over injected HTTP failures" $ do
        it "classifies 404 as infra and absolves the caller's request" $ do
            let tf = classifyStatus 404
            tfClass tf `shouldBe` InfraFault
            tfText tf `shouldSatisfy` T.isInfixOf "not the problem"
            wellFormed tf
        it "classifies every server-side status as infra, never payload" $
            mapM_
                ( \s -> do
                    let tf = classifyStatus s
                    tfClass tf `shouldBe` InfraFault
                    tfText tf `shouldSatisfy` T.isInfixOf "not the problem"
                    wellFormed tf
                )
                [401, 403, 404, 500, 502, 503]
        it "classifies only malformed-request statuses as payload" $
            mapM_
                ( \s -> do
                    tfClass (classifyStatus s) `shouldBe` PayloadFault
                    wellFormed (classifyStatus s)
                )
                [400, 422]
        it "distils a connect-refused to one bounded infra line" $ do
            let tf =
                    classifyException
                        60
                        (httpEx (ConnectionFailure (toException (userError "refused"))))
            tfClass tf `shouldBe` InfraFault
            wellFormed tf
        it "reports a timeout as the-server-is-still-working, with the check" $ do
            let tf = classifyException 60 (httpEx ResponseTimeout)
            tfClass tf `shouldBe` InfraFault
            tfText tf `shouldSatisfy` T.isInfixOf "60s"
            tfText tf `shouldSatisfy` T.isInfixOf "may have landed"
            tfText tf `shouldSatisfy` T.isInfixOf "list_cells"
            tfText tf `shouldSatisfy` T.isInfixOf "do NOT resend"
            wellFormed tf
        it "never renders a non-JSON body as the caller's JSON problem (M8)" $ do
            let tf = classifyDecode "404 page not found"
            tfClass tf `shouldBe` InfraFault
            tfText tf `shouldSatisfy` T.isInfixOf "not the problem"
            wellFormed tf
        it "distils any other HttpException to its label, never the record" $ do
            let tf = classifyException 60 (httpEx (TooManyRedirects []))
            tfClass tf `shouldBe` InfraFault
            wellFormed tf
        it "classifies a non-HTTP exception as bounded infra" $ do
            let tf = classifyTransport 60 (toException (userError "boom"))
            tfClass tf `shouldBe` InfraFault
            wellFormed tf

    describe "single error envelope" $
        it "renders every class as one [class] line" $ do
            renderFailure (ToolFailure InfraFault "x")
                `shouldBe` "[infra] x"
            renderFailure (ToolFailure KernelFault "x")
                `shouldBe` "[kernel] x"
            renderFailure (ToolFailure PayloadFault "x")
                `shouldBe` "[payload] x"
