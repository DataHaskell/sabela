{-# LANGUAGE BangPatterns #-}
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

{- | Apply @f@ to the notebook and store the result. The bang on the
result keeps the 'MVar' from trapping a thunk chain across edits.
-}
modifyNotebook :: NotebookStore -> (Notebook -> Notebook) -> IO ()
modifyNotebook ns f = modifyMVar_ (nsNotebook ns) (\nb -> let !nb' = f nb in pure nb')

modifyNotebookIO :: NotebookStore -> (Notebook -> IO Notebook) -> IO ()
modifyNotebookIO ns f =
    modifyMVar_ (nsNotebook ns) (\nb -> do !nb' <- f nb; pure nb')

freshCellId :: NotebookStore -> IO Int
freshCellId ns = atomicModifyIORef' (nsNextId ns) (\n -> (n + 1, n))
