{-# LANGUAGE OverloadedStrings #-}

{- | The one gate every integration spec goes through, so the suite can be run
without them: @SABELA_SKIP_LIVE=1@ marks each live item pending instead of
building a cabal project and spawning GHCi for it.
-}
module Test.Live (
    liveSpecs,
    requireLiveIntegration,
    requireLiveFor,
    requireExecutable,
    skipLiveVar,
) where

import Control.Monad (unless, when)
import System.Directory (doesFileExist, findExecutable)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Test.Hspec (Expectation, Spec, pendingWith, runIO)

import Sabela.Session.Project (buildTimeSupportDir)

skipLiveVar :: String
skipLiveVar = "SABELA_SKIP_LIVE"

{- | Registers the integration specs only when they are wanted. Skipping at the
registry costs nothing at run time, unlike a guard inside every item.
-}
liveSpecs :: Spec -> Spec
liveSpecs specs = do
    skipping <- runIO liveSkipped
    unless skipping specs

liveSkipped :: IO Bool
liveSkipped = do
    v <- lookupEnv skipLiveVar
    pure (maybe False (`notElem` ["", "0", "off", "false", "no"]) v)

requireLiveIntegration :: Expectation
requireLiveIntegration = requireLiveFor "integration"

{- | @label@ names what is being skipped, so a pending item says which slice of
the suite did not run.
-}
requireLiveFor :: String -> Expectation
requireLiveFor label = do
    skipping <- liveSkipped
    when skipping (pendingWith skipped)
    requireExecutable "cabal"
    supportPresent <-
        doesFileExist (buildTimeSupportDir </> "sabela-notebook.cabal")
    if supportPresent
        then pure ()
        else
            pendingWith ("sabela-notebook support source not on disk; skipping " <> label)
  where
    skipped = skipLiveVar <> " is set; skipping " <> label

requireExecutable :: String -> Expectation
requireExecutable name = do
    found <- findExecutable name
    case found of
        Nothing -> pendingWith (name <> " not found on PATH; skipping integration")
        Just _ -> pure ()
