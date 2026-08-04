{-# LANGUAGE OverloadedStrings #-}

{- | The four shapes a 'DisposableResult' takes: the empty one every route
starts from, and the three failures (snapshot moved under us, a stage failed,
the verdict that failure implies).
-}
module Sabela.Session.Materialize.Result (
    emptyResult,
    snapshotFailure,
    failed,
    verdictForFailure,
) where

import Data.Text (Text)
import qualified Data.Text as T

import Sabela.Session.Materialize.Candidate (disposableRouteName)
import Sabela.Session.MaterializeStage (
    DisposableResult (..),
    DisposableVerdict (..),
    MaterializeFailure (..),
    MaterializeStage (..),
    stageFailure,
 )

emptyResult :: [Text] -> DisposableResult
emptyResult deps =
    DisposableResult
        { disposableRoute = disposableRouteName
        , disposableVerdict = DisposableUnavailable
        , disposableType = Nothing
        , disposableStdout = ""
        , disposableStderr = ""
        , disposableFailure = Nothing
        , disposableReplayedCells = []
        , disposableSkippedCells = []
        , disposableDependencies = deps
        }

snapshotFailure :: DisposableResult -> [Int] -> Text -> DisposableResult
snapshotFailure base replayed message =
    base
        { disposableVerdict = DisposableUnavailable
        , disposableStderr = message
        , disposableFailure =
            Just (MaterializeFailure StageSnapshot Nothing message)
        , disposableReplayedCells = replayed
        }

failed ::
    DisposableResult ->
    MaterializeStage ->
    Maybe Int ->
    Text ->
    DisposableResult
failed base stage cid message =
    base
        { disposableVerdict = verdictForFailure message
        , disposableStderr = message
        , disposableFailure = Just (stageFailure stage cid message)
        }

verdictForFailure :: Text -> DisposableVerdict
verdictForFailure message
    | "timed out" `T.isInfixOf` T.toLower message = DisposableTimedOut
    | otherwise = DisposableCompileError
