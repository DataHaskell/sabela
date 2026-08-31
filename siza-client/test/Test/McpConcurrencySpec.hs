{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- | A slow tool call must not hold the next request behind it. The MCP server
read one line, handled it to completion, then read the next, so a
@kernel_status@ sent during a multi-minute build waited for the build: the
lock-free status query exists precisely to answer while something else runs.
-}
module Test.McpConcurrencySpec (mcpConcurrencySpec) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Monad (unless)
import Data.Aeson (Value (..), encode, object, (.=))
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import System.Timeout (timeout)
import Test.Hspec

import Sabela.AI.Types (ToolOutcome (..))
import Siza.Mcp (McpEnv, handleLine, mcpCatalogue, mcpEnvOver, mcpSession)
import Siza.Mcp.Serve (serveRequests)

callLine :: String -> BS8.ByteString
callLine name =
    LBS8.toStrict . encode $
        object
            [ "jsonrpc" .= ("2.0" :: String)
            , "id" .= name
            , "method" .= ("tools/call" :: String)
            , "params" .= object ["name" .= name, "arguments" .= object []]
            ]

-- | Hands out the queued lines, then end of input.
lineSource :: IORef [BS8.ByteString] -> IO (Maybe BS8.ByteString)
lineSource ref = atomicModifyIORef' ref $ \case
    [] -> ([], Nothing)
    (l : ls) -> (ls, Just l)

-- | Waits for a written response mentioning the given text.
awaitWrite :: IORef [String] -> String -> IO Bool
awaitWrite written needle = fmap (fromMaybe False) (timeout 2_000_000 poll)
  where
    poll = do
        ws <- readIORef written
        if any (needle `isInfixOf`) ws
            then pure True
            else threadDelay 5000 >> poll

mcpConcurrencySpec :: Spec
mcpConcurrencySpec = describe "the MCP server serves requests concurrently" $ do
    it "answers a quick request while a slow one is still running" $ do
        held <- newEmptyMVar
        session <- mcpSession
        written <- newIORef ([] :: [String])
        let dispatch call
                | "kernel_status" `isInfixOf` show call = pure (Right (ToolOk "idle"))
                | otherwise = takeMVar held >> pure (Right (ToolOk "slow done"))
            env = mcpEnvOver session dispatch mcpCatalogue
        queue <- newIORef [callLine "execute_cell", callLine "kernel_status"]
        served <- newEmptyMVar
        _ <-
            forkIO
                ( serveRequests
                    (lineSource queue)
                    (\v -> atomicModifyIORef' written (\ws -> (ws ++ [show v], ())))
                    handleLine
                    env
                    >> putMVar served ()
                )
        quick <- awaitWrite written "kernel_status"
        quick `shouldBe` True
        putMVar held ()
        finished <- timeout 5_000_000 (takeMVar served)
        finished `shouldBe` Just ()

    it "waits at end of input for the calls still in flight" $ do
        session <- mcpSession
        written <- newIORef ([] :: [String])
        let dispatch _ = threadDelay 50_000 >> pure (Right (ToolOk "done"))
            env = mcpEnvOver session dispatch mcpCatalogue
        queue <- newIORef [callLine "execute_cell"]
        _ <-
            serveRequests
                (lineSource queue)
                (\v -> atomicModifyIORef' written (\ws -> (ws ++ [show v], ())))
                handleLine
                env
        ws <- readIORef written
        length ws `shouldBe` 1

    it "keeps serving after a request that throws" $ do
        session <- mcpSession
        written <- newIORef ([] :: [String])
        let dispatch call
                | "execute_cell" `isInfixOf` show call = ioError (userError "boom")
                | otherwise = pure (Right (ToolOk "idle"))
            env = mcpEnvOver session dispatch mcpCatalogue
        queue <- newIORef [callLine "execute_cell", callLine "kernel_status"]
        done <-
            timeout 5_000_000 $
                serveRequests
                    (lineSource queue)
                    (\v -> atomicModifyIORef' written (\ws -> (ws ++ [show v], ())))
                    handleLine
                    env
        done `shouldBe` Just ()
        ws <- readIORef written
        unless (any ("kernel_status" `isInfixOf`) ws) $
            expectationFailure ("the surviving request went unanswered: " ++ show ws)
