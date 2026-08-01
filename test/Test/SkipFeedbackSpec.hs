{-# LANGUAGE OverloadedStrings #-}

module Test.SkipFeedbackSpec (spec) where

import Control.Concurrent.STM (atomically, tryReadTChan)
import qualified Data.Set as Set
import Sabela.Handlers (ReactiveNotebook (..), setupReactive)
import Sabela.Model (NotebookEvent (..))
import Sabela.State (App (..), newApp)
import Sabela.State.EventBus (subscribeBroadcast)
import Sabela.Deps (collectMetadata)
import Sabela.Handlers.Lifecycle (neededEnvSig)
import Sabela.State.NotebookStore (readNotebook)
import Sabela.State.SessionManager (installHaskellSession)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.WriteAckFixture (inertBackend)

{- | An App whose kernel is installed and whose (empty) notebook matches it, so
run-all takes its "nothing to do" branch.
-}
settledApp :: IO (App, ReactiveNotebook)
settledApp = do
    app <- newApp "." Set.empty Nothing Nothing []
    backend <- inertBackend
    nb <- readNotebook (appNotebook app)
    installHaskellSession
        (appSessions app)
        backend
        (neededEnvSig app (collectMetadata nb))
    rn <- setupReactive app
    pure (app, rn)

spec :: Spec
spec = describe "skip paths still report completion" $ do
    it "run-all on a settled notebook says it is done, not nothing at all" $ do
        (app, rn) <- settledApp
        chan <- subscribeBroadcast (appEvents app)
        rnRunAll rn
        seen <- atomically (tryReadTChan chan)
        (seen >>= isDone) `shouldBe` Just True

    it "an unforced run of a missing cell says it is done" $ do
        (app, rn) <- settledApp
        chan <- subscribeBroadcast (appEvents app)
        rnRunCell rn 999
        seen <- atomically (tryReadTChan chan)
        (seen >>= isDone) `shouldBe` Just True
  where
    isDone EvExecutionDone = Just True
    isDone _ = Just False
