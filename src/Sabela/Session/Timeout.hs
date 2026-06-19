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
    }
    deriving (Show, Eq)

-- | 120s execution budget, 5s resync window — the historical defaults.
defaultTimeoutConfig :: TimeoutConfig
defaultTimeoutConfig =
    TimeoutConfig
        { tcExecutionUs = 120_000_000
        , tcResyncUs = 5_000_000
        }

{- | Read the execution budget from @SABELA_CELL_TIMEOUT_SECONDS@. A
missing, unparseable, or non-positive value keeps the 120s default; the
resync window is not configurable.
-}
readTimeoutConfig :: IO TimeoutConfig
readTimeoutConfig = do
    mEnv <- lookupEnv "SABELA_CELL_TIMEOUT_SECONDS"
    pure $ case mEnv >>= readMaybe of
        Just secs
            | secs > 0 ->
                defaultTimeoutConfig{tcExecutionUs = secs * 1_000_000}
        _ -> defaultTimeoutConfig

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
