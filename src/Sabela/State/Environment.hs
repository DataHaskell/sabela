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
    , envLeanReplBin :: Maybe FilePath
    -- ^ Path to the Lean REPL binary. Resolved from SABELA_LEAN_REPL env var.
    , envLeanBase :: Maybe FilePath
    -- ^ Path to a pre-built Lean project with Mathlib. Resolved from SABELA_LEAN_BASE.
    , envLeanCache :: FilePath
    -- ^ Persistent cache directory for Lean projects (~/.sabela/lean-cache/).
    , envAnthropicKey :: Maybe Text
    -- ^ Anthropic API key. From ANTHROPIC_API_KEY env var.
    , envAnthropicModel :: Text
    -- ^ Claude model to use. From ANTHROPIC_MODEL env var (default: claude-sonnet-4-20250514).
    }
