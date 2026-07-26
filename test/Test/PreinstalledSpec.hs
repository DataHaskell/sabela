{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.PreinstalledSpec (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (atomically, tryReadTChan)
import Control.Monad (void)
import Data.IORef (modifyIORef, newIORef, readIORef)
import qualified Data.Set as Set
import Sabela.Handlers (buildTimeSupportDir, installAndRestart)
import Sabela.Model (NotebookEvent (..), SessionStatus (..))
import Sabela.Server (newApp)
import Sabela.State (App (..))
import Sabela.State.EventBus (subscribeBroadcast)
import ScriptHs.Parser (CabalMeta (..))
import System.Directory (doesFileExist, findExecutable)
import System.FilePath ((</>))
import Test.Hspec (Spec, describe, it, pendingWith, shouldSatisfy)

spec :: Spec
spec = describe "preinstalled packages" $ do
    it "installAndRestart skips SUpdateDeps for packages already in stGlobalDeps" $ do
        cabal <- findExecutable "cabal"
        case cabal of
            Nothing -> pendingWith "cabal not found on PATH; skipping integration test"
            Just _ -> pure ()
        supportPresent <-
            doesFileExist (buildTimeSupportDir </> "sabela-notebook.cabal")
        if supportPresent
            then pure ()
            else pendingWith "sabela-notebook support source not on disk; skipping"
        app <-
            newApp "." (Set.fromList ["containers"]) Nothing Nothing [buildTimeSupportDir]
        chan <- subscribeBroadcast (appEvents app)

        let meta =
                CabalMeta
                    { metaDeps = ["containers"]
                    , metaExts = []
                    , metaGhcOptions = []
                    , metaExtraLibDirs = []
                    , metaExtraIncludeDirs = []
                    , metaPackages = []
                    , metaSourceRepos = []
                    , metaUnknownKeys = []
                    }

        void $ forkIO $ void $ installAndRestart app 0 meta

        eventsRef <- newIORef ([] :: [NotebookEvent])
        let poll 0 = pure ()
            poll remaining = do
                mev <- atomically (tryReadTChan chan)
                case mev of
                    Nothing -> threadDelay 100_000 >> poll (remaining - 1)
                    Just ev -> do
                        modifyIORef eventsRef (ev :)
                        case ev of
                            EvSessionStatus SReady -> pure ()
                            _ -> poll remaining
        poll (1800 :: Int)
        events <- readIORef eventsRef

        let statuses = [s | EvSessionStatus s <- events]
            installEvents = [deps | SUpdateDeps deps <- statuses]

        statuses `shouldSatisfy` (SReady `elem`)

        concat installEvents `shouldSatisfy` notElem "containers"
