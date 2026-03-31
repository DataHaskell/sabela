{-# LANGUAGE OverloadedStrings #-}

module Sabela.Handlers.Shared
    ( bumpGeneration
    , isCurrentGen
    , whenCurrentGen
    , broadcast
    , updateAndBroadcast
    , broadcastCellError
    , debugLog
    , isMimeLine
    , mkStreamingCallback
    , partitionExports
    , applyResult
    , find
    ) where

import Control.Monad (unless)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.Api (RunResult (..))
import Sabela.Model
    ( Cell (..)
    , CellError (..)
    , Notebook (..)
    , NotebookEvent (..)
    )
import Sabela.State (App (..))
import qualified Sabela.State.EventBus as EB
import Sabela.State.NotebookStore (modifyNotebookIO)

bumpGeneration :: App -> IO Int
bumpGeneration = EB.bumpGeneration . appEvents

broadcast :: App -> NotebookEvent -> IO ()
broadcast = EB.broadcast . appEvents

updateAndBroadcast :: App -> (Notebook -> Notebook) -> NotebookEvent -> IO ()
updateAndBroadcast app f ev = modifyNotebookIO (appNotebook app) $ \nb -> do
    let nb' = f nb
    broadcast app ev
    pure nb'

debugLog :: App -> Text -> IO ()
debugLog app = EB.debugLog (appEnv app)

isCurrentGen :: App -> Int -> IO Bool
isCurrentGen = EB.isCurrentGen . appEvents

whenCurrentGen :: App -> Int -> IO () -> IO ()
whenCurrentGen = EB.whenCurrentGen . appEvents

isMimeLine :: Text -> Bool
isMimeLine line = "---MIME:" `T.isPrefixOf` line

mkStreamingCallback :: App -> Int -> IO (Text -> IO ())
mkStreamingCallback app cid = do
    inMimeBlock <- newIORef False
    pure $ \line ->
        if isMimeLine line
            then writeIORef inMimeBlock ("---MIME:text/plain---" /= line)
            else do
                isMime <- readIORef inMimeBlock
                unless isMime $
                    broadcast app (EvCellPartialOutput cid line)

partitionExports :: [(Text, Text)] -> ([(Text, Text)], [(Text, Text)])
partitionExports = foldr classify ([], [])
  where
    classify (mime, body) (exps, normals) =
        case T.stripPrefix "EXPORT:" mime of
            Just name -> ((name, body) : exps, normals)
            Nothing -> (exps, (mime, body) : normals)

applyResult :: RunResult -> Cell -> Cell
applyResult r c
    | cellId c == rrCellId r =
        c
            { cellOutputs = rrOutputs r
            , cellError = rrError r
            , cellDirty = False
            }
    | otherwise = c

broadcastCellError :: App -> Int -> Text -> IO ()
broadcastCellError app cid msg = do
    let err = CellError{ceLine = Nothing, ceCol = Nothing, ceMessage = msg}
    updateAndBroadcast
        app
        (\nb -> nb{nbCells = map (clearCellOutputs cid msg) (nbCells nb)})
        (EvCellResult cid [] (Just msg) [err])

clearCellOutputs :: Int -> Text -> Cell -> Cell
clearCellOutputs targetCid errMsg c
    | cellId c == targetCid =
        c{cellOutputs = [], cellError = Just errMsg, cellDirty = False}
    | otherwise = c
