{-# LANGUAGE OverloadedStrings #-}

{- | One bounded failure envelope for the tool transport (R6.3, R6.9): every
transport-layer failure is classified — infra, kernel, or payload — and
rendered as a single @[class] message@ line. Raw exception records and
serialisation-error strings never reach the model; routing failures never
blame the caller's payload.
-}
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

-- | Who is at fault: the infrastructure, the kernel, or the caller's payload.
data FailureClass = InfraFault | KernelFault | PayloadFault
    deriving (Eq, Show)

-- | The single failure envelope every transport error renders through.
data ToolFailure = ToolFailure
    { tfClass :: FailureClass
    , tfText :: Text
    }
    deriving (Eq, Show)

-- | The one-line rendering: @[class] message@, bounded.
renderFailure :: ToolFailure -> Text
renderFailure (ToolFailure c t) = "[" <> tag c <> "] " <> bounded t
  where
    tag InfraFault = "infra"
    tag KernelFault = "kernel"
    tag PayloadFault = "payload"

-- | One line, bounded: newlines collapse and long text truncates.
bounded :: Text -> Text
bounded = T.take 380 . T.unwords . T.words

notYourFault :: Text
notYourFault = "Your request was not the problem."

-- | Classify any exception the HTTP layer threw; never ship its Show record.
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

{- | A non-2xx status is a server-side answer, classified BEFORE any payload
decode: 404 and 5xx are infrastructure; only 400/422 blame the payload.
-}
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

{- | A 2xx body that fails to decode is a server/proxy fault (it spoke
non-JSON) - never rendered as the caller's payload or JSON problem (M8).
-}
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
