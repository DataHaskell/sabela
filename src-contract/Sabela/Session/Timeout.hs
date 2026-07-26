{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Cell-execution timeout policy, shared by every language backend so
the GHCi and Python sessions cannot drift apart. The execution budget is
configurable via @SABELA_CELL_TIMEOUT_SECONDS@; the post-timeout resync
window is fixed.
-}
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
    -- ^ Per-cell execution budget, in microseconds.
    , tcResyncUs :: Int
    -- ^ Post-timeout resync window, in microseconds.
    , tcBuildUs :: Int
    {- ^ Off-lock build budget (dep install / cold start), in microseconds. The
    per-cell 'tcExecutionUs' does not cover this phase, so a wedged @cabal
    install@ or cold start is bounded here instead of hanging indefinitely.
    -}
    , tcTryBuildUs :: Int
    {- ^ Disposable @try@ package-environment build budget, in microseconds.
    Tighter than 'tcBuildUs' so a slow disposable build fails fast with
    'tryBuildTimedOutMessage' guidance instead of hanging toward the live ceiling.
    -}
    }
    deriving (Eq, Show)

{- | 30 minutes for execution and for the live-session build, 5s resync; 120s
disposable-try build, since a speculative trial should fail fast rather than
wait toward the live ceiling.
-}
defaultTimeoutConfig :: TimeoutConfig
defaultTimeoutConfig =
    TimeoutConfig
        { tcExecutionUs = 1800_000_000
        , tcResyncUs = 5_000_000
        , tcBuildUs = 1800_000_000
        , tcTryBuildUs = 120_000_000
        }

{- | Read the execution budget from @SABELA_CELL_TIMEOUT_SECONDS@. A
missing, unparseable, or non-positive value keeps the 30-minute default; the
resync window is not configurable.
-}
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

-- | The execution budget rendered as whole seconds (for UI messages).
timeoutSeconds :: Int -> Int
timeoutSeconds executionUs = executionUs `div` 1_000_000

{- | The post-interrupt timeout notice, with the actual budget filled in
so the message can never claim a duration the session did not apply.
-}
timedOutMessage :: Int -> Text
timedOutMessage executionUs =
    "\n*** Execution timed out after "
        <> T.pack (show (timeoutSeconds executionUs))
        <> " seconds; computation interrupted ***"

{- | The notice for a timeout that the kill ladder had to escalate past
the interrupt: the session was killed and the next run respawns a fresh
kernel automatically, so the user is told recovery is in hand rather than
left with an opaque "session killed".
-}
timedOutKilledMessage :: Int -> Text
timedOutKilledMessage executionUs =
    "\n*** Execution timed out after "
        <> T.pack (show (timeoutSeconds executionUs))
        <> " seconds and did not respond to interrupt; the kernel was \
           \killed and will respawn on the next run ***"

{- | The notice when the off-lock build (dep install / cold start) exceeds
'tcBuildUs': the half-built kernel was reaped, so the user is told recovery is
in hand and how to raise the budget, rather than left staring at a stuck build.
-}
buildTimedOutMessage :: Int -> Text
buildTimedOutMessage buildUs =
    "\n*** Build (dependency install / cold start) timed out after "
        <> T.pack (show (timeoutSeconds buildUs))
        <> " seconds; the kernel was reset. Check the dependencies compile, or \
           \raise SABELA_BUILD_TIMEOUT_SECONDS ***"

{- | The notice when a disposable @try@ build breaches 'tcTryBuildUs': names
the dependencies and points at the deliberate-commit alternative, rather
than leaving a bare timeout.
-}
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
