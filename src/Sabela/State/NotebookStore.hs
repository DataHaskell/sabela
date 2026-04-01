{-# LANGUAGE OverloadedStrings #-}

module Sabela.State.NotebookStore (
    NotebookStore (..),
    newNotebookStore,
    readNotebook,
    modifyNotebook,
    modifyNotebookIO,
    freshCellId,
) where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Sabela.Model (Notebook (..))

data NotebookStore = NotebookStore
    { nsNotebook :: MVar Notebook
    , nsNextId :: IORef Int
    }

newNotebookStore :: IO NotebookStore
newNotebookStore =
    NotebookStore
        <$> newMVar (Notebook "Untitled.md" [])
        <*> newIORef 0

readNotebook :: NotebookStore -> IO Notebook
readNotebook = readMVar . nsNotebook

modifyNotebook :: NotebookStore -> (Notebook -> Notebook) -> IO ()
modifyNotebook ns f = modifyMVar_ (nsNotebook ns) (pure . f)

modifyNotebookIO :: NotebookStore -> (Notebook -> IO Notebook) -> IO ()
modifyNotebookIO ns = modifyMVar_ (nsNotebook ns)

freshCellId :: NotebookStore -> IO Int
freshCellId ns = atomicModifyIORef' (nsNextId ns) (\n -> (n + 1, n))
