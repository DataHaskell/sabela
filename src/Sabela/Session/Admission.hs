module Sabela.Session.Admission (
    Admission (..),
    admit,
) where

import Control.Concurrent (MVar, putMVar, tryTakeMVar)
import Control.Exception (finally, mask)
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)

data Admission a
    = Ran a
    | Busy {running :: !Int, runningForMs :: !Int}
    deriving (Eq, Show)

admit ::
    MVar () -> IORef (Maybe (Int, Word64)) -> Int -> IO a -> IO (Admission a)
admit lock holderRef cid act = mask $ \restore -> do
    got <- tryTakeMVar lock
    case got of
        Nothing -> do
            held <- readIORef holderRef
            now <- getMonotonicTimeNSec
            let (hid, ms) = case held of
                    Just (h, t0) -> (h, fromIntegral ((now - t0) `div` 1000000))
                    Nothing -> (cid, 0)
            pure (Busy hid ms)
        Just () -> do
            now <- getMonotonicTimeNSec
            writeIORef holderRef (Just (cid, now))
            let release = writeIORef holderRef Nothing >> putMVar lock ()
            r <- restore act `finally` release
            pure (Ran r)
