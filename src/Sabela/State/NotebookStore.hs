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

{- | Apply @f@ to the notebook and store the result. The bang on the
result keeps the 'MVar' from trapping a thunk chain across edits.
-}
modifyNotebook :: NotebookStore -> (Notebook -> Notebook) -> IO ()
modifyNotebook ns f = modifyMVar_ (nsNotebook ns) (\nb -> let !nb' = f nb in pure nb')

modifyNotebookIO :: NotebookStore -> (Notebook -> IO Notebook) -> IO ()
modifyNotebookIO ns f =
    modifyMVar_ (nsNotebook ns) (\nb -> do !nb' <- f nb; pure nb')

{- | Atomically transform the notebook and return a value, both decided under
the lock. A validate-then-commit (return the new notebook on success, the
unchanged one plus a rejection on conflict) is then a single critical section,
so a checked mutation never leaves a half-applied edit.
-}
atomicEditNotebook :: NotebookStore -> (Notebook -> (Notebook, a)) -> IO a
atomicEditNotebook ns f =
    modifyMVar (nsNotebook ns) (\nb -> let (!nb', a) = f nb in pure (nb', a))

freshCellId :: NotebookStore -> IO Int
freshCellId ns = atomicModifyIORef' (nsNextId ns) (\n -> (n + 1, n))
