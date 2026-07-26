{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Sabela.Session.Timeout (
    TimeoutConfig (..),
    defaultTimeoutConfig,
    readTimeoutConfig,
    timeoutSeconds,
    timedOutMessage,
    timedOutKilledMessage,
    buildTimedOutMessage,
    tryBuildTimedOutMessage,
) where

import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

data TimeoutConfig = TimeoutConfig
    { tcExecutionUs :: Int
    , tcResyncUs :: Int
    , tcBuildUs :: Int
    , tcTryBuildUs :: Int
    }
    deriving (Eq, Show)

defaultTimeoutConfig :: TimeoutConfig
defaultTimeoutConfig =
    TimeoutConfig
        { tcExecutionUs = 1800_000_000
        , tcResyncUs = 5_000_000
        , tcBuildUs = 1800_000_000
        , tcTryBuildUs = 120_000_000
        }

readTimeoutConfig :: IO TimeoutConfig
readTimeoutConfig = do
    mEnv <- lookupEnv "SABELA_CELL_TIMEOUT_SECONDS"
    mBuild <- lookupEnv "SABELA_BUILD_TIMEOUT_SECONDS"
    mTryBuild <- lookupEnv "SABELA_TRY_BUILD_TIMEOUT_SECONDS"
    let withExec cfg = case mEnv >>= readMaybe of
            Just secs | secs > 0 -> cfg{tcExecutionUs = secs * 1_000_000}
            _ -> cfg
        withBuild cfg = case mBuild >>= readMaybe of
            Just secs | secs > 0 -> cfg{tcBuildUs = secs * 1_000_000}
            _ -> cfg
        withTryBuild cfg = case mTryBuild >>= readMaybe of
            Just secs | secs > 0 -> cfg{tcTryBuildUs = secs * 1_000_000}
            _ -> cfg
    pure (withTryBuild (withBuild (withExec defaultTimeoutConfig)))

timeoutSeconds :: Int -> Int
timeoutSeconds executionUs = executionUs `div` 1_000_000

timedOutMessage :: Int -> Text
timedOutMessage executionUs =
    "\n*** Execution timed out after "
        <> T.pack (show (timeoutSeconds executionUs))
        <> " seconds; computation interrupted ***"

timedOutKilledMessage :: Int -> Text
timedOutKilledMessage executionUs =
    "\n*** Execution timed out after "
        <> T.pack (show (timeoutSeconds executionUs))
        <> " seconds and did not respond to interrupt; the kernel was \
           \killed and will respawn on the next run ***"

buildTimedOutMessage :: Int -> Text
buildTimedOutMessage buildUs =
    "\n*** Build (dependency install / cold start) timed out after "
        <> T.pack (show (timeoutSeconds buildUs))
        <> " seconds; the kernel was reset. Check the dependencies compile, or \
           \raise SABELA_BUILD_TIMEOUT_SECONDS ***"

tryBuildTimedOutMessage :: [Text] -> Int -> Text
tryBuildTimedOutMessage deps buildUs =
    "\n*** try build timed out after "
        <> T.pack (show (timeoutSeconds buildUs))
        <> " seconds building "
        <> depsText
        <> "; this looks like a heavy dependency for a disposable trial. \
           \Commit it deliberately with a `-- cabal:` line in a real cell \
           \instead of retrying try ***"
  where
    depsText
        | null deps = "the requested dependencies"
        | otherwise = T.intercalate ", " deps
