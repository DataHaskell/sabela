{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Sabela.AI.Handles (
    HandleId (..),
    HandleRef (..),
    Output (..),
    LargeResult (..),
    HandleStore,
    newHandleStore,
    storeLargeResult,
    lookupHandle,
    clearHandles,
    summarizeForLLM,
    headLines,
    tailLines,
    sliceLines,
    grepLines,
    cleanOutput,
    stripAnsi,
) where

import Control.Concurrent.STM (
    TVar,
    atomically,
    modifyTVar',
    newTVarIO,
    readTVarIO,
    writeTVar,
 )
import Data.Aeson (Value, object, (.=))
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.AI.Output (
    HandleId (..),
    HandleRef (..),
    Output (..),
 )
import Sabela.Model (MimeType (..))

data LargeResult = LargeResult
    { lrLines :: [Text]
    , lrTotalLines :: Int
    , lrTotalBytes :: Int
    }

data HandleStore = HandleStore
    { hsMap :: TVar (M.Map HandleId LargeResult)
    , hsCounter :: IORef Int
    }

newHandleStore :: IO HandleStore
newHandleStore = HandleStore <$> newTVarIO M.empty <*> newIORef 0

largeThresholdLines :: Int
largeThresholdLines = 40

largeThresholdBytes :: Int
largeThresholdBytes = 4096

summaryPreviewLines :: Int
summaryPreviewLines = 20

storeLargeResult :: HandleStore -> Text -> IO Output
storeLargeResult store raw = do
    let cleaned = cleanOutput raw
        ls = T.lines cleaned
        nLines = length ls
        nBytes = T.length cleaned
    if nLines <= largeThresholdLines && nBytes <= largeThresholdBytes
        then pure (Inline MimePlain cleaned)
        else do
            n <- atomicModifyIORef' (hsCounter store) (\i -> (i + 1, i))
            let hid = HandleId (T.pack ("lr_" ++ show n))
                previewLines = take summaryPreviewLines ls
                moreCount = nLines - length previewLines
                suffix =
                    if moreCount > 0
                        then "\n...[" <> T.pack (show moreCount) <> " more lines; use explore_result]"
                        else ""
                summary = T.intercalate "\n" previewLines <> suffix
            atomically $
                modifyTVar' (hsMap store) (M.insert hid (LargeResult ls nLines nBytes))
            pure (Stashed (HandleRef hid summary nLines nBytes))

lookupHandle :: HandleStore -> HandleId -> IO (Maybe LargeResult)
lookupHandle store hid = M.lookup hid <$> readTVarIO (hsMap store)

clearHandles :: HandleStore -> IO ()
clearHandles store = atomically $ writeTVar (hsMap store) M.empty

summarizeForLLM :: HandleId -> Text -> Int -> Int -> Value
summarizeForLLM (HandleId hid) summary nLines nBytes =
    object
        [ "handleId" .= hid
        , "summary" .= summary
        , "totalLines" .= nLines
        , "totalBytes" .= nBytes
        , "hint"
            .= ( "Call explore_result with handleId to read head/tail/slice/grep of this payload." ::
                    Text
               )
        ]

headLines :: Int -> LargeResult -> [Text]
headLines n lr = take (max 0 n) (lrLines lr)

tailLines :: Int -> LargeResult -> [Text]
tailLines n lr =
    let keep = max 0 (lrTotalLines lr - max 0 n)
     in drop keep (lrLines lr)

sliceLines :: Int -> Int -> LargeResult -> [Text]
sliceLines from1 to1 lr =
    let from = max 1 from1
        to = max from to1
        size = to - from + 1
     in take size (drop (from - 1) (lrLines lr))

grepLines :: Text -> LargeResult -> [(Int, Text)]
grepLines pat lr =
    take 50 [(i, l) | (i, l) <- zip [1 ..] (lrLines lr), pat `T.isInfixOf` l]

cleanOutput :: Text -> Text
cleanOutput = T.intercalate "\n" . dedupeConsec . map stripAnsi . T.lines
  where
    dedupeConsec [] = []
    dedupeConsec (x : xs) = go x (1 :: Int) xs
    go x !n (y : ys)
        | y == x = go x (n + 1) ys
        | n > 1 = annotate x n : dedupeConsec (y : ys)
        | otherwise = x : dedupeConsec (y : ys)
    go x !n [] =
        if n > 1 then [annotate x n] else [x]
    annotate x n = x <> " [\xd7" <> T.pack (show n) <> "]"

stripAnsi :: Text -> Text
stripAnsi = go
  where
    go t = case T.findIndex (== '\ESC') t of
        Nothing -> t
        Just i ->
            let (pre, rest) = T.splitAt i t
                after = case T.stripPrefix "\ESC[" rest of
                    Just r -> dropUntilFinal r
                    Nothing -> T.drop 1 rest
             in pre <> go after
    dropUntilFinal t =
        let (_, final) = T.break isFinal t
         in T.drop 1 final
    isFinal c = c >= '@' && c <= '~'
