{-# LANGUAGE LambdaCase #-}

{- | Serving requests off the wire. One thread per request, because a call that
takes minutes must not hold the next request behind it: @kernel_status@ and
@await_idle@ are lock-free at the server and exist to answer while a build runs.
-}
module Siza.Mcp.Serve (serveRequests, stdinLine) where

import Control.Concurrent (
    forkIO,
    newEmptyMVar,
    newMVar,
    putMVar,
    takeMVar,
    withMVar,
 )
import Control.Exception (SomeException, catch, finally)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import System.IO (hPutStrLn, isEOF, stderr)

stdinLine :: IO (Maybe BS.ByteString)
stdinLine = do
    eof <- isEOF
    if eof then pure Nothing else Just <$> BS8.getLine

{- | Responses go out under a lock, since JSON-RPC pairs them by id rather than
by order, and end of input waits for whatever is still running. A request that
throws is reported and dropped: the rest of the session carries on.
-}
serveRequests ::
    IO (Maybe BS.ByteString) ->
    (resp -> IO ()) ->
    (env -> BS.ByteString -> IO (Maybe resp)) ->
    env ->
    IO ()
serveRequests nextLine write handle env = do
    writing <- newMVar ()
    let respond line = do
            resp <- handle env (stripCR line)
            mapM_ (\v -> withMVar writing (\_ -> write v)) resp
        report e =
            hPutStrLn
                stderr
                ("siza mcp: request failed: " <> show (e :: SomeException))
        go running =
            nextLine >>= \case
                Nothing -> mapM_ takeMVar running
                Just line -> do
                    finished <- newEmptyMVar
                    _ <-
                        forkIO
                            ((respond line `catch` report) `finally` putMVar finished ())
                    go (finished : running)
    go []

stripCR :: BS.ByteString -> BS.ByteString
stripCR b
    | not (BS.null b) && BS.last b == 13 = BS.init b
    | otherwise = b
