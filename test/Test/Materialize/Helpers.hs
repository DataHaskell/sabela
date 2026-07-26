{-# LANGUAGE OverloadedStrings #-}

-- | Shared fixtures and gating helpers for the disposable-materialization specs.
module Test.Materialize.Helpers (
    requireLiveIntegration,
    scratchDirectories,
    newPackageCandidate,
    requireCompleted,
    nsToSeconds,
    hasCompleteMarker,
    listCacheBuckets,
    requireSnapshot,
) where

import Data.List (isPrefixOf)
import qualified Data.Text as T
import Data.Word (Word64)
import System.Directory (
    doesDirectoryExist,
    doesFileExist,
    findExecutable,
    listDirectory,
 )
import System.FilePath ((</>))
import System.Timeout (timeout)
import Test.Hspec

import Sabela.Session.Materialize (
    CandidateSpec (..),
    DisposableResult,
    MaterializeSnapshot,
    captureMaterializeSnapshot,
 )
import Sabela.Session.Project (buildTimeSupportDir)
import Sabela.State (App (..))

requireLiveIntegration :: Expectation
requireLiveIntegration = do
    cabal <- findExecutable "cabal"
    case cabal of
        Nothing -> pendingWith "cabal not found on PATH; skipping materialization integration"
        Just _ -> pure ()
    supportPresent <-
        doesFileExist (buildTimeSupportDir </> "sabela-notebook.cabal")
    if supportPresent
        then pure ()
        else
            pendingWith
                "sabela-notebook support source not on disk; skipping materialization integration"

scratchDirectories :: FilePath -> IO [FilePath]
scratchDirectories root =
    filter ("sabela-try" `isPrefixOf`) <$> listDirectory root

{- | An uninstalled-package trial: tiny and dependency-free, so a real build
(network fetch + compile) is genuinely required, but stays fast.
-}
newPackageCandidate :: T.Text -> CandidateSpec
newPackageCandidate pkg =
    CandidateSpec
        { candidateMetadataSource =
            T.unlines
                [ "-- cabal: build-depends: " <> pkg
                , "1 + (1 :: Int)"
                ]
        , candidateSetup = ""
        , candidateExpression = Just "1 + (1 :: Int)"
        , candidateReplacesCellId = Nothing
        , candidateDeliberate = False
        }

requireCompleted :: IO DisposableResult -> IO DisposableResult
requireCompleted action = do
    completed <- timeout (180 * 1000000) action
    maybe
        (expectationFailure "disposable materialization timed out" >> fail "unreachable")
        pure
        completed

nsToSeconds :: Word64 -> Double
nsToSeconds ns = fromIntegral ns / 1e9

{- | Is a bucket marked complete, i.e. usable as a cache HIT? A shelved
(budget-breached) bucket keeps its store but never this marker.
-}
hasCompleteMarker :: FilePath -> FilePath -> IO Bool
hasCompleteMarker root bucket = doesFileExist (root </> bucket </> ".complete")

listCacheBuckets :: FilePath -> IO [FilePath]
listCacheBuckets root = do
    exists <- doesDirectoryExist root
    if exists then listDirectory root else pure []

requireSnapshot :: App -> IO MaterializeSnapshot
requireSnapshot app = do
    captured <- captureMaterializeSnapshot app
    case captured of
        Left message -> expectationFailure (T.unpack message) >> fail "unreachable"
        Right snapshot -> pure snapshot
