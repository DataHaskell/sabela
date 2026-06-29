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
        -- The session build-depends on sabela-notebook; supply it as a local
        -- overlay the way app/Main's locateSupportSource does, else skip.
        supportPresent <-
            doesFileExist (buildTimeSupportDir </> "sabela-notebook.cabal")
        if supportPresent
            then pure ()
            else pendingWith "sabela-notebook support source not on disk; skipping"
        -- Build state with "containers" declared as a global (preinstalled) dep
        app <-
            newApp "." (Set.fromList ["containers"]) Nothing Nothing [buildTimeSupportDir]
        chan <- subscribeBroadcast (appEvents app)

        -- gen=0 matches the freshly-initialised generation IORef
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

        -- Drain the broadcast channel until SReady, with a wall-clock deadline.
        -- Budget is spent ONLY while waiting (the threadDelay branch), never per
        -- event — under load the session emits a burst of status/log events, and
        -- decrementing on each would exhaust the budget long before SReady,
        -- failing spuriously. 180 s covers a contended cabal-install + restart.
        eventsRef <- newIORef ([] :: [NotebookEvent])
        let poll 0 = pure ()
            poll remaining = do
                mev <- atomically (tryReadTChan chan)
                case mev of
                    Nothing -> threadDelay 100_000 >> poll (remaining - 1)
                    Just ev -> do
                        modifyIORef eventsRef (ev :)
                        case ev of
                            EvSessionStatus SReady -> pure () -- done
                            _ -> poll remaining
        poll (1800 :: Int) -- 1800 × 100 ms idle ticks = 180 s wall-clock
        events <- readIORef eventsRef

        let statuses = [s | EvSessionStatus s <- events]
            installEvents = [deps | SUpdateDeps deps <- statuses]

        -- The session must have reached SReady
        statuses `shouldSatisfy` (SReady `elem`)

        -- "containers" must NOT appear in any SUpdateDeps broadcast
        concat installEvents `shouldSatisfy` notElem "containers"
