{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.PreinstalledSpec (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (atomically, dupTChan, tryReadTChan)
import Control.Monad (void)
import Data.IORef (modifyIORef, newIORef, readIORef)
import qualified Data.Set as Set
import Sabela.Handlers (installAndRestart)
import Sabela.Model (
    AppState (..),
    NotebookEvent (..),
    SessionStatus (..),
 )
import Sabela.Server (initState)
import ScriptHs.Parser (CabalMeta (..))
import Test.Hspec (Spec, describe, it, shouldSatisfy)

spec :: Spec
spec = describe "preinstalled packages" $ do
    it "installAndRestart skips SUpdateDeps for packages already in stGlobalDeps" $ do
        -- Build state with "containers" declared as a global (preinstalled) dep
        st <- initState "." [] (Set.fromList ["containers"])
        chan <- atomically $ dupTChan (stBroadcast st)

        -- gen=0 matches the freshly-initialised stGeneration IORef
        let meta =
                CabalMeta
                    { metaDeps = ["containers"]
                    , metaExts = []
                    , metaGhcOptions = []
                    }

        void $ forkIO $ void $ installAndRestart st 0 meta

        -- Poll the broadcast channel for up to 30 s, stop when SReady arrives
        eventsRef <- newIORef ([] :: [NotebookEvent])
        let poll 0 = pure ()
            poll remaining = do
                threadDelay 100_000 -- 100 ms
                mev <- atomically (tryReadTChan chan)
                case mev of
                    Nothing -> poll (remaining - 1)
                    Just ev -> do
                        modifyIORef eventsRef (ev :)
                        case ev of
                            EvSessionStatus SReady -> pure () -- done
                            _ -> poll (remaining - 1)
        poll (300 :: Int) -- 300 × 100 ms = 30 s

        events <- readIORef eventsRef

        let statuses = [s | EvSessionStatus s <- events]
            installEvents = [deps | SUpdateDeps deps <- statuses]

        -- The session must have reached SReady
        statuses `shouldSatisfy` (SReady `elem`)

        -- "containers" must NOT appear in any SUpdateDeps broadcast
        concat installEvents `shouldSatisfy` notElem "containers"
