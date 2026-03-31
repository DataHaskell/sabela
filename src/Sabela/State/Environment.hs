module Sabela.State.Environment
    ( Environment (..)
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
    }
