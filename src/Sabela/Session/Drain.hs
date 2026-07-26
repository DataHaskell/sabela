{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Sabela.Session.Drain (
    DrainResult (..),
    drainResultText,
    drainUntilMarker,
    discardUntilMarker,
    runAccumCapBytes,
) where

import Control.Concurrent.STM (atomically)
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.Session.Reader (
    OutQueue,
    dequeueLine,
    markerNonceBase,
    markerNumberIn,
 )

data DrainResult = DrainOk !Text | DrainEof !Text
    deriving (Eq, Show)

drainResultText :: DrainResult -> Text
drainResultText (DrainOk t) = t
drainResultText (DrainEof t) = t

runAccumCapBytes :: Int
runAccumCapBytes = 50 * 1024 * 1024

drainUntilMarker :: OutQueue -> Text -> (Text -> IO ()) -> IO DrainResult
drainUntilMarker q mk onLine = go [] 0 False
  where
    target = markerNumberIn mk
    go !acc !sz !truncated = do
        r <- atomically (dequeueLine q)
        case r of
            Nothing -> pure (DrainEof (render acc truncated))
            Just line
                | mk `T.isInfixOf` line -> pure (DrainOk (render acc truncated))
                | isStale line -> go [] 0 truncated
                | sz > runAccumCapBytes -> onLine line >> go acc sz True
                | otherwise -> do
                    onLine line
                    go (line : acc) (sz + T.length line) truncated
    isStale line = case (markerNumberIn line, target) of
        (Just m, Just t) ->
            m `div` markerNonceBase == t `div` markerNonceBase && m < t
        _ -> False
    render acc truncated =
        let body = T.strip (T.unlines (reverse acc))
         in if truncated
                then body <> "\n …[output truncated by sabela]"
                else body

discardUntilMarker :: OutQueue -> Text -> IO Bool
discardUntilMarker q mk = go
  where
    go = do
        r <- atomically (dequeueLine q)
        case r of
            Nothing -> pure False
            Just line
                | mk `T.isInfixOf` line -> pure True
                | otherwise -> go
