module Sabela.State.DependencyTracker (
    DependencyTracker (..),
    newDependencyTracker,
    getHaskellDeps,
    setHaskellDeps,
    getHaskellExts,
    setHaskellExts,
    getHaskellProjectSig,
    setHaskellProjectSig,
    getPythonDeps,
    setPythonDeps,
) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Sabela.Deps (ProjectSig, emptyProjectSig)

data DependencyTracker = DependencyTracker
    { dtHaskellDeps :: IORef (Set Text)
    , dtHaskellExts :: IORef (Set Text)
    , dtHaskellProjectSig :: IORef ProjectSig
    , dtPythonDeps :: IORef (Set Text)
    }

newDependencyTracker :: IO DependencyTracker
newDependencyTracker =
    DependencyTracker
        <$> newIORef S.empty
        <*> newIORef S.empty
        <*> newIORef emptyProjectSig
        <*> newIORef S.empty

getHaskellDeps :: DependencyTracker -> IO (Set Text)
getHaskellDeps = readIORef . dtHaskellDeps

setHaskellDeps :: DependencyTracker -> Set Text -> IO ()
setHaskellDeps dt = writeIORef (dtHaskellDeps dt)

getHaskellExts :: DependencyTracker -> IO (Set Text)
getHaskellExts = readIORef . dtHaskellExts

setHaskellExts :: DependencyTracker -> Set Text -> IO ()
setHaskellExts dt = writeIORef (dtHaskellExts dt)

getHaskellProjectSig :: DependencyTracker -> IO ProjectSig
getHaskellProjectSig = readIORef . dtHaskellProjectSig

setHaskellProjectSig :: DependencyTracker -> ProjectSig -> IO ()
setHaskellProjectSig dt = writeIORef (dtHaskellProjectSig dt)

getPythonDeps :: DependencyTracker -> IO (Set Text)
getPythonDeps = readIORef . dtPythonDeps

setPythonDeps :: DependencyTracker -> Set Text -> IO ()
setPythonDeps dt = writeIORef (dtPythonDeps dt)
