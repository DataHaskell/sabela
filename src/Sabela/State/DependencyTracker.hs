module Sabela.State.DependencyTracker (
    DependencyTracker (..),
    newDependencyTracker,
    getHaskellDeps,
    setHaskellDeps,
    getHaskellExts,
    setHaskellExts,
    getLeanDeps,
    setLeanDeps,
    getPythonDeps,
    setPythonDeps,
) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)

data DependencyTracker = DependencyTracker
    { dtHaskellDeps :: IORef (Set Text)
    , dtHaskellExts :: IORef (Set Text)
    , dtLeanDeps :: IORef (Set Text)
    , dtPythonDeps :: IORef (Set Text)
    }

newDependencyTracker :: IO DependencyTracker
newDependencyTracker =
    DependencyTracker
        <$> newIORef S.empty
        <*> newIORef S.empty
        <*> newIORef S.empty
        <*> newIORef S.empty

getHaskellDeps :: DependencyTracker -> IO (Set Text)
getHaskellDeps = readIORef . dtHaskellDeps

setHaskellDeps :: DependencyTracker -> Set Text -> IO ()
setHaskellDeps dt = writeIORef (dtHaskellDeps dt)

getHaskellExts :: DependencyTracker -> IO (Set Text)
getHaskellExts = readIORef . dtHaskellExts

setHaskellExts :: DependencyTracker -> Set Text -> IO ()
setHaskellExts dt = writeIORef (dtHaskellExts dt)

getLeanDeps :: DependencyTracker -> IO (Set Text)
getLeanDeps = readIORef . dtLeanDeps

setLeanDeps :: DependencyTracker -> Set Text -> IO ()
setLeanDeps dt = writeIORef (dtLeanDeps dt)

getPythonDeps :: DependencyTracker -> IO (Set Text)
getPythonDeps = readIORef . dtPythonDeps

setPythonDeps :: DependencyTracker -> Set Text -> IO ()
setPythonDeps dt = writeIORef (dtPythonDeps dt)
