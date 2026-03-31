module Sabela.State.EventBus
    ( EventBus (..)
    , newEventBus
    , broadcast
    , bumpGeneration
    , isCurrentGen
    , whenCurrentGen
    , subscribeBroadcast
    , debugLog
    ) where

import Control.Concurrent.STM (TChan, atomically, dupTChan, newBroadcastTChanIO, writeTChan)
import Control.Monad (when)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Sabela.Model (NotebookEvent)
import Sabela.State.Environment (Environment (..))
import System.IO (stderr)

data EventBus = EventBus
    { ebBroadcast :: TChan NotebookEvent
    , ebGeneration :: IORef Int
    }

newEventBus :: IO EventBus
newEventBus = EventBus <$> newBroadcastTChanIO <*> newIORef 0

broadcast :: EventBus -> NotebookEvent -> IO ()
broadcast eb ev = atomically $ writeTChan (ebBroadcast eb) ev

bumpGeneration :: EventBus -> IO Int
bumpGeneration eb =
    atomicModifyIORef' (ebGeneration eb) (\g -> let g' = g + 1 in (g', g'))

isCurrentGen :: EventBus -> Int -> IO Bool
isCurrentGen eb gen = (== gen) <$> readIORef (ebGeneration eb)

whenCurrentGen :: EventBus -> Int -> IO () -> IO ()
whenCurrentGen eb gen action = do
    still <- isCurrentGen eb gen
    when still action

subscribeBroadcast :: EventBus -> IO (TChan NotebookEvent)
subscribeBroadcast eb = atomically $ dupTChan (ebBroadcast eb)

debugLog :: Environment -> Text -> IO ()
debugLog env msg = when (envDebugLog env) $ TIO.hPutStrLn stderr msg
