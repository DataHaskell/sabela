{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Capabilities.Edit.Cascade.Commit (
    classify,
    applyFresh,
    proposeDependency,
    verifyAndRevert,
    restoreIfStale,
) where

import Data.Aeson (Value)
import Data.IORef (IORef, modifyIORef', readIORef, writeIORef)
import Data.Text (Text)
import System.Environment (lookupEnv)

import Sabela.AI.Capabilities.Edit.Exec (executeCell)
import Sabela.AI.Capabilities.Edit.RepairGate (
    CandidateVerdict (..),
    classifyCandidate,
 )
import Sabela.AI.Health (healthOfResult, improvesHealthFor)
import Sabela.AI.Types (ExecutionResult (..))
import Sabela.Anthropic.Types (CancelToken)
import Sabela.Handlers (ReactiveNotebook, updateCellSource)
import Sabela.Model
import Sabela.Parse (cellNames)
import Sabela.Parse.Declared (preservesDeclarations)
import Sabela.SessionTypes (CellLang (..))
import Sabela.State

classify :: App -> Int -> Text -> Text -> IO CandidateVerdict
classify app cid priorSrc cand = do
    mc <- lookupCell cid <$> readNotebook (appNotebook app)
    let (lang, ty) = maybe (Haskell, CodeCell) (\c -> (cellLang c, cellType c)) mc
    classifyCandidate app cid lang ty priorSrc cand

proposeDependency ::
    App -> Int -> IORef [Value] -> Text -> Maybe (Text, Text) -> IO ()
proposeDependency _ _ _ _ Nothing = pure ()
proposeDependency app cid sugRef priorSrc (Just (_, cand)) = do
    verdict <- classify app cid priorSrc cand
    case verdict of
        CandidateSuggested note -> modifyIORef' sugRef (note :)
        _ -> pure ()

applyFresh ::
    App ->
    ReactiveNotebook ->
    CancelToken ->
    Int ->
    IORef Bool ->
    Text ->
    Text ->
    IO (Maybe (Either Text ExecutionResult))
applyFresh app rn cancelTok cid staleRef priorSrc newSrc = do
    verdict <- classify app cid priorSrc newSrc
    case verdict of
        CandidateApplyable cand -> do
            modifyNotebook (appNotebook app) (updateCellSource cid cand)
            broadcastNotebook app
            writeIORef staleRef False
            Just <$> executeCell app rn cid cancelTok
        _ -> pure Nothing

verifyAndRevert ::
    App ->
    ReactiveNotebook ->
    CancelToken ->
    Int ->
    IORef [Value] ->
    IORef Bool ->
    Either Text ExecutionResult ->
    Text ->
    [Text] ->
    IO (Maybe (Either Text ExecutionResult))
verifyAndRevert _ _ _ _ _ _ _ _ [] = pure Nothing
verifyAndRevert app rn cancelTok cid sugRef staleRef res priorSrc (cand : rest)
    | not (preservesDeclarations priorSrc cand) =
        verifyAndRevert app rn cancelTok cid sugRef staleRef res priorSrc rest
verifyAndRevert app rn cancelTok cid sugRef staleRef res priorSrc (cand : rest) = do
    verdict <- classify app cid priorSrc cand
    case verdict of
        CandidateRejected -> next
        CandidateSuggested note -> modifyIORef' sugRef (note :) >> next
        CandidateApplyable _ -> do
            modifyNotebook (appNotebook app) (updateCellSource cid cand)
            broadcastNotebook app
            newRes <- executeCell app rn cid cancelTok
            let defined = fst (cellNames priorSrc)
                kept = improvesHealthFor defined (healthOfResult res) (healthOfResult newRes)
            debugDumpVerify res newRes kept
            if kept
                then writeIORef staleRef False >> pure (Just newRes)
                else do
                    modifyNotebook (appNotebook app) (updateCellSource cid priorSrc)
                    broadcastNotebook app
                    writeIORef staleRef True
                    next
  where
    next = verifyAndRevert app rn cancelTok cid sugRef staleRef res priorSrc rest

restoreIfStale ::
    App ->
    ReactiveNotebook ->
    CancelToken ->
    Int ->
    IORef Bool ->
    Either Text ExecutionResult ->
    IO (Either Text ExecutionResult)
restoreIfStale app rn cancelTok cid staleRef res = do
    stale <- readIORef staleRef
    if stale then executeCell app rn cid cancelTok else pure res

debugDumpVerify ::
    Either Text ExecutionResult -> Either Text ExecutionResult -> Bool -> IO ()
debugDumpVerify oldRes newRes kept = do
    mp <- lookupEnv "SABELA_DEBUG_VERIFY"
    case mp of
        Just p
            | not (null p)
            , p /= "0" ->
                appendFile p $
                    "verify kept="
                        <> show kept
                        <> "\n  old: "
                        <> show (healthOfResult oldRes)
                        <> "\n  new: "
                        <> show (healthOfResult newRes)
                        <> "\n"
        _ -> pure ()
