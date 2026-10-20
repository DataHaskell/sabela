module Sabela.State.Environment (
    Environment (..),
) where

import Data.Set (Set)
import Data.Text (Text)
import System.IO (Handle)

data Environment = Environment
    { envWorkDir :: FilePath
    , envTmpDir :: FilePath
    , envReplProjectDir :: FilePath
    , envReplProjectLock :: Maybe Handle
    , envGlobalDeps :: Set Text
    , envLocalPackages :: [FilePath]
    , envDebugLog :: Bool
    , envAnthropicKey :: Maybe Text
    , envAnthropicModel :: Text
    }
