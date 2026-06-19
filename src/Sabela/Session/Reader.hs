{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Bounded, binary-safe output capture shared by all interpreter
backends: a byte-budgeted queue of decoded lines with an out-of-band
EOF tombstone, fed by a chunked reader that caps line length.
-}
module Sabela.Session.Reader (
    -- * Queue
    SessLine (..),
    OutQueue (..),
    newOutQueue,
    enqueueLine,
    enqueueEof,
    dequeueLine,
    drainToEof,

    -- * Loops
    readLoop,
    errLoop,
    boundedLines,
    scanDiscarded,

    -- * Markers
    markerPrefix,
    markerSuffix,
    mkMarkerText,
    markerNumberIn,
    markerNonceBase,

    -- * Limits
    lineCapBytes,
    errLineCapBytes,
    queueBytesCap,
    queueEntryCap,
    maxErrLines,
) where

import Control.Concurrent.STM (
    STM,
    TBQueue,
    TVar,
    atomically,
    check,
    isFullTBQueue,
    modifyTVar',
    newTBQueueIO,
    newTVarIO,
    peekTBQueue,
    readTBQueue,
    readTVar,
    writeTBQueue,
    writeTVar,
 )
import Control.Exception (SomeException, finally, try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.IORef (IORef, atomicModifyIORef', readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import System.IO (Handle)

-- | One captured output line (with its byte cost) or the EOF tombstone.
data SessLine = SessLine !Int !Text | SessEof

{- | Line queue plus a byte budget. Producers block on either bound;
consumers credit the budget back. 'SessEof' is sticky: it is peeked,
never popped, so every later drain observes it immediately.
-}
data OutQueue = OutQueue
    { oqItems :: TBQueue SessLine
    , oqBytes :: TVar Int
    }

lineCapBytes, errLineCapBytes, queueBytesCap, queueEntryCap, maxErrLines :: Int
lineCapBytes = 10 * 1024 * 1024
errLineCapBytes = 64 * 1024
queueBytesCap = 64 * 1024 * 1024
queueEntryCap = 64
maxErrLines = 500

newOutQueue :: IO OutQueue
newOutQueue =
    OutQueue
        <$> newTBQueueIO (fromIntegral queueEntryCap)
        <*> newTVarIO 0

-- | Producer enqueue: one transaction blocking on both bounds.
enqueueLine :: OutQueue -> Int -> Text -> STM ()
enqueueLine (OutQueue q b) c t = do
    used <- readTVar b
    check (used + c <= queueBytesCap)
    writeTBQueue q (SessLine c t)
    writeTVar b (used + c)

{- | Enqueue the EOF tombstone without ever blocking: drops the oldest
line (crediting its bytes back) when the queue is full.
-}
enqueueEof :: OutQueue -> STM ()
enqueueEof (OutQueue q b) = do
    full <- isFullTBQueue q
    if not full
        then writeTBQueue q SessEof
        else do
            item <- readTBQueue q
            case item of
                SessEof -> writeTBQueue q SessEof
                SessLine c _ -> do
                    modifyTVar' b (subtract c)
                    writeTBQueue q SessEof

{- | Pop the next line, or return Nothing on the EOF tombstone (which
stays in place). Pop and budget credit happen in this one transaction.
-}
dequeueLine :: OutQueue -> STM (Maybe Text)
dequeueLine (OutQueue q b) = do
    item <- peekTBQueue q
    case item of
        SessEof -> pure Nothing
        SessLine c t -> do
            _ <- readTBQueue q
            modifyTVar' b (subtract c)
            pure (Just t)

-- | Pop lines until the tombstone is observed; used by session teardown.
drainToEof :: OutQueue -> IO ()
drainToEof q = do
    r <- atomically (dequeueLine q)
    case r of
        Nothing -> pure ()
        Just _ -> drainToEof q

{- | Capture an interpreter's stdout into the queue; on any exit
(EOF or exception) the tombstone is guaranteed to land.
-}
readLoop :: Handle -> OutQueue -> IO ()
readLoop h q =
    swallow (boundedLines lineCapBytes h emit)
        `finally` atomically (enqueueEof q)
  where
    emit c t = atomically (enqueueLine q c t)

-- | Capture stderr into a bounded ring, echoing each line to a callback.
errLoop :: Handle -> IORef [Text] -> IORef (Text -> IO ()) -> IO ()
errLoop h ref cbRef = swallow (boundedLines errLineCapBytes h emit)
  where
    emit _ t = do
        atomicModifyIORef' ref $ \ls ->
            let ls' = take maxErrLines (t : ls)
             in (length ls' `seq` ls', ())
        cb <- readIORef cbRef
        cb t

swallow :: IO () -> IO ()
swallow act = do
    _ <- try act :: IO (Either SomeException ())
    pure ()

{- | Stream a handle line-by-line in raw chunks, leniently decoded, with
@cap@ bytes per line; the over-cap remainder is discarded without
accumulation but still scanned for markers (emitted as synthetic lines).
-}
boundedLines :: Int -> Handle -> (Int -> Text -> IO ()) -> IO ()
boundedLines cap h emit = go [] 0 False BS.empty
  where
    go !acc !len !discard !carry = do
        chunk <- BS.hGetSome h chunkSize
        if BS.null chunk
            then case acc of
                [] -> pure ()
                _ -> emitLine acc False
            else consume chunk acc len discard carry
    consume chunk acc len discard carry
        | BS.null chunk = go acc len discard carry
        | otherwise = case BS.elemIndex 10 chunk of
            Nothing
                | discard -> do
                    carry' <- scanDiscarded emit (carry <> chunk)
                    go acc len True carry'
                | len + BS.length chunk > cap -> do
                    let (keep, rest) = BS.splitAt (cap - len) chunk
                    emitLine (keep : acc) True
                    carry' <- scanDiscarded emit (capTail keep <> rest)
                    go [] 0 True carry'
                | otherwise ->
                    go (chunk : acc) (len + BS.length chunk) False BS.empty
            Just i -> do
                let (end, rest) = (BS.take i chunk, BS.drop (i + 1) chunk)
                if discard
                    then do
                        _ <- scanDiscarded emit (carry <> end)
                        pure ()
                    else
                        if len + BS.length end > cap
                            then do
                                let (keep, rest') = BS.splitAt (cap - len) end
                                emitLine (keep : acc) True
                                _ <- scanDiscarded emit (capTail keep <> rest')
                                pure ()
                            else emitLine (end : acc) False
                consume rest [] 0 False BS.empty
    emitLine revChunks truncated = do
        let bs = BS.concat (reverse revChunks)
            txt = TE.decodeUtf8With TE.lenientDecode bs
            txt' = if truncated then txt <> truncNotice else txt
        emit (BS.length bs) txt'
    truncNotice = " …[line truncated by sabela]"
    capTail = BS.takeEnd (BS.length markerPrefixBS + 26)

chunkSize :: Int
chunkSize = 32768

{- | Scan discarded bytes for complete markers, emitting each as its own
synthetic line; returns the carry (a possibly-incomplete marker tail).
-}
scanDiscarded :: (Int -> Text -> IO ()) -> BS.ByteString -> IO BS.ByteString
scanDiscarded emit win =
    case BS.breakSubstring markerPrefixBS win of
        (_, rest)
            | BS.null rest -> pure (BS.takeEnd (BS.length markerPrefixBS - 1) win)
            | otherwise -> do
                let afterPfx = BS.drop (BS.length markerPrefixBS) rest
                    (digits, afterD) = BC.span (\c -> c >= '0' && c <= '9') afterPfx
                if BS.null digits
                    then
                        if BS.null afterD
                            then pure rest
                            else scanDiscarded emit afterD
                    else
                        if "---" `BS.isPrefixOf` afterD
                            then do
                                let mk = markerPrefixBS <> digits <> "---"
                                emit
                                    (BS.length mk)
                                    (TE.decodeUtf8With TE.lenientDecode mk)
                                scanDiscarded emit (BS.drop 3 afterD)
                            else
                                if BS.length afterD < 3
                                    then pure (BS.takeEnd maxCarry rest)
                                    else scanDiscarded emit afterD
  where
    maxCarry = BS.length markerPrefixBS + 24

markerPrefix, markerSuffix :: Text
markerPrefix = "---SABELA_MARKER_"
markerSuffix = "---"

markerPrefixBS :: BS.ByteString
markerPrefixBS = TE.encodeUtf8 markerPrefix

mkMarkerText :: Int -> Text
mkMarkerText n = markerPrefix <> T.pack (show n) <> markerSuffix

{- | Multiplier separating a session's per-run counter from its nonce in a
marker number: @nonce * markerNonceBase + run@. The nonce occupies the high
digits, so notebook output cannot forge a live marker without knowing it.
-}
markerNonceBase :: Int
markerNonceBase = 100000

{- | The number of the first well-formed marker in a line; lines with a
marker prefix but no parsable @n---@ tail are ordinary output.
-}
markerNumberIn :: Text -> Maybe Int
markerNumberIn line = do
    let (_, rest) = T.breakOn markerPrefix line
    afterPfx <- T.stripPrefix markerPrefix rest
    let (digits, afterD) = T.span (\c -> c >= '0' && c <= '9') afterPfx
    if not (T.null digits)
        && T.length digits <= 19
        && markerSuffix `T.isPrefixOf` afterD
        then Just (read (T.unpack digits))
        else Nothing
