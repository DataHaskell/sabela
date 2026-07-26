{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Sabela.State.NotebookStore (
    NotebookStore (..),
    newNotebookStore,
    readNotebook,
    modifyNotebook,
    modifyNotebookIO,
    atomicEditNotebook,
    freshCellId,
) where

import Control.Concurrent.MVar (
    MVar,
    modifyMVar,
    modifyMVar_,
    newMVar,
    readMVar,
 )
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
modifyNotebook ns f = modifyMVar_ (nsNotebook ns) (\nb -> let !nb' = f nb in pure nb')

modifyNotebookIO :: NotebookStore -> (Notebook -> IO Notebook) -> IO ()
modifyNotebookIO ns f =
    modifyMVar_ (nsNotebook ns) (\nb -> do !nb' <- f nb; pure nb')

atomicEditNotebook :: NotebookStore -> (Notebook -> (Notebook, a)) -> IO a
atomicEditNotebook ns f =
    modifyMVar (nsNotebook ns) (\nb -> let (!nb', a) = f nb in pure (nb', a))

freshCellId :: NotebookStore -> IO Int
freshCellId ns = atomicModifyIORef' (nsNextId ns) (\n -> (n + 1, n))
