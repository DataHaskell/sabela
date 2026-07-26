module Sabela.State.Environment (
    Environment (..),
) where

import Data.Set (Set)
import Data.Text (Text)

data Environment = Environment
    { envWorkDir :: FilePath
    , envTmpDir :: FilePath
    , envGlobalDeps :: Set Text
    , envLocalPackages :: [FilePath]
    , envDebugLog :: Bool
    , envAnthropicKey :: Maybe Text
    , envAnthropicModel :: Text
    }
