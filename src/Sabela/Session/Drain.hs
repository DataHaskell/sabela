{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Marker-delimited drains over an 'OutQueue', shared by the GHCi and
Python backends. EOF is the typed tombstone (never a string), and lines
carrying a stale (lower-numbered) marker discard the output before them.
-}
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
    markerNumberIn,
 )

-- | Output up to the target marker, or up to EOF if the session died.
data DrainResult = DrainOk !Text | DrainEof !Text
    deriving (Eq, Show)

drainResultText :: DrainResult -> Text
drainResultText (DrainOk t) = t
drainResultText (DrainEof t) = t

-- | Cap on output accumulated per run; the rest is dropped with a notice.
runAccumCapBytes :: Int
runAccumCapBytes = 50 * 1024 * 1024

{- | Collect lines until the target marker ('DrainOk'), the EOF tombstone
('DrainEof'), or—on a stale marker—restart the accumulator, dropping the
previous run's leftovers. Streams each kept line to the callback.
-}
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
        (Just m, Just t) -> m < t
        _ -> False
    render acc truncated =
        let body = T.strip (T.unlines (reverse acc))
         in if truncated
                then body <> "\n …[output truncated by sabela]"
                else body

{- | Discard lines until the target marker; True when found, False when
the EOF tombstone is observed first. Used to resync after a timeout.
-}
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
