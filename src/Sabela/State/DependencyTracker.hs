{- | Python dependency state. The Haskell equivalents recorded intent at
install-attempt start rather than what a kernel holds; that now lives against
the live process as an @EnvSig@ in "Sabela.State.SessionManager".
-}
module Sabela.State.DependencyTracker (
    DependencyTracker (..),
    newDependencyTracker,
    getPythonDeps,
    setPythonDeps,
) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)

newtype DependencyTracker = DependencyTracker
    { dtPythonDeps :: IORef (Set Text)
    }

newDependencyTracker :: IO DependencyTracker
newDependencyTracker = DependencyTracker <$> newIORef S.empty

getPythonDeps :: DependencyTracker -> IO (Set Text)
getPythonDeps = readIORef . dtPythonDeps

setPythonDeps :: DependencyTracker -> Set Text -> IO ()
setPythonDeps dt = writeIORef (dtPythonDeps dt)
