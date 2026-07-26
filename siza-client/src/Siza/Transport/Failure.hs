{-# LANGUAGE OverloadedStrings #-}

module Siza.Transport.Failure (
    FailureClass (..),
    ToolFailure (..),
    renderFailure,
    classifyTransport,
    classifyException,
    classifyStatus,
    classifyDecode,
) where

import Control.Exception (SomeException, fromException)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client (
    HttpException (..),
    HttpExceptionContent (..),
 )

data FailureClass = InfraFault | KernelFault | PayloadFault
    deriving (Eq, Show)

data ToolFailure = ToolFailure
    { tfClass :: FailureClass
    , tfText :: Text
    }
    deriving (Eq, Show)

renderFailure :: ToolFailure -> Text
renderFailure (ToolFailure c t) = "[" <> tag c <> "] " <> bounded t
  where
    tag InfraFault = "infra"
    tag KernelFault = "kernel"
    tag PayloadFault = "payload"

bounded :: Text -> Text
bounded = T.take 380 . T.unwords . T.words

notYourFault :: Text
notYourFault = "Your request was not the problem."

classifyTransport :: Int -> SomeException -> ToolFailure
classifyTransport timeoutSecs e = case fromException e of
    Just he -> classifyException timeoutSecs he
    Nothing ->
        ToolFailure
            InfraFault
            ( "transport failure ("
                <> T.take 100 (tshow e)
                <> "). "
                <> notYourFault
            )

classifyException :: Int -> HttpException -> ToolFailure
classifyException timeoutSecs (HttpExceptionRequest _ content) =
    ToolFailure InfraFault (describe content)
  where
    describe ResponseTimeout =
        "no response within "
            <> tshow timeoutSecs
            <> "s. The server is likely STILL WORKING; a write may have landed. "
            <> "Check with list_cells / kernel_status (or await_idle) before "
            <> "anything else - do NOT resend the same call."
    describe ConnectionTimeout =
        "cannot connect to the server (connect timeout): infrastructure is "
            <> "down or unreachable. "
            <> notYourFault
    describe (ConnectionFailure _) =
        "cannot connect to the server (connection refused/failed): "
            <> "infrastructure is down or unreachable. "
            <> notYourFault
    describe other =
        "HTTP transport failure ("
            <> T.takeWhile (/= ' ') (tshow other)
            <> "). "
            <> notYourFault
classifyException _ (InvalidUrlException _ why) =
    ToolFailure
        InfraFault
        ("invalid tool endpoint URL (" <> T.pack why <> "). " <> notYourFault)

classifyStatus :: Int -> ToolFailure
classifyStatus 404 =
    ToolFailure
        InfraFault
        ( "HTTP 404: this tool endpoint does not exist on the server "
            <> "(server-side routing failure). "
            <> notYourFault
            <> " Use a different tool or report the endpoint as unavailable; "
            <> "do not rework the request body."
        )
classifyStatus s
    | s `elem` [400, 422] =
        ToolFailure
            PayloadFault
            ("HTTP " <> tshow s <> ": the server rejected the request body as malformed.")
    | otherwise =
        ToolFailure
            InfraFault
            ("HTTP " <> tshow s <> ": server-side failure. " <> notYourFault)

classifyDecode :: Text -> ToolFailure
classifyDecode raw =
    ToolFailure
        InfraFault
        ( "the server replied with non-JSON (\""
            <> T.take 60 raw
            <> "\"): a server or proxy fault. "
            <> notYourFault
        )

tshow :: (Show a) => a -> Text
tshow = T.pack . show
