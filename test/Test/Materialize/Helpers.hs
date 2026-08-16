{-# LANGUAGE OverloadedStrings #-}

module Test.Materialize.Helpers (
    requireLiveIntegration,
    scratchDirectories,
    newPackageCandidate,
    packagesCandidate,
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
    listDirectory,
 )
import System.FilePath ((</>))
import System.Timeout (timeout)
import Test.Hspec

import Test.Live (requireLiveFor)

import Sabela.Session.Materialize (
    CandidateSpec (..),
    DisposableResult,
    MaterializeSnapshot,
    captureMaterializeSnapshot,
 )
import Sabela.State (App (..))

requireLiveIntegration :: Expectation
requireLiveIntegration = requireLiveFor "materialization integration"

scratchDirectories :: FilePath -> IO [FilePath]
scratchDirectories root =
    filter ("sabela-try" `isPrefixOf`) <$> listDirectory root

-- | Bucket-cold by construction; possibly store-warm since trials share it.
newPackageCandidate :: T.Text -> CandidateSpec
newPackageCandidate pkg = packagesCandidate [pkg]

packagesCandidate :: [T.Text] -> CandidateSpec
packagesCandidate pkgs =
    CandidateSpec
        { candidateMetadataSource =
            T.unlines
                [ "-- cabal: build-depends: " <> T.intercalate ", " pkgs
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

hasCompleteMarker :: FilePath -> FilePath -> IO Bool
hasCompleteMarker root bucket = doesFileExist (root </> bucket </> ".complete")

-- | Bucket directories only; the root also holds the lease lock dir.
listCacheBuckets :: FilePath -> IO [FilePath]
listCacheBuckets root = do
    exists <- doesDirectoryExist root
    if exists
        then filter ("env-" `isPrefixOf`) <$> listDirectory root
        else pure []

requireSnapshot :: App -> IO MaterializeSnapshot
requireSnapshot app = do
    captured <- captureMaterializeSnapshot app
    case captured of
        Left message -> expectationFailure (T.unpack message) >> fail "unreachable"
        Right snapshot -> pure snapshot
