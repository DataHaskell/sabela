module Sabela.State.Environment (
    Environment (..),
) where

import Data.Set (Set)
import Data.Text (Text)

data Environment = Environment
    { envWorkDir :: FilePath
    -- ^ Root directory for the file explorer and relative paths.
    , envTmpDir :: FilePath
    -- ^ Temporary directory for REPL project scaffolding.
    , envGlobalDeps :: Set Text
    -- ^ Dependencies from global.md / preinstalled packages (immutable).
    , envDebugLog :: Bool
    -- ^ Whether to emit verbose debug logging to stderr.
    , envAnthropicKey :: Maybe Text
    -- ^ Anthropic API key. From ANTHROPIC_API_KEY env var.
    , envAnthropicModel :: Text
    -- ^ Claude model to use. From ANTHROPIC_MODEL env var (default: claude-sonnet-4-20250514).
    }
