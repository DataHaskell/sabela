{-# LANGUAGE OverloadedStrings #-}

module Test.GenerationSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, tryReadTChan)
import Control.Monad (void)
import Data.IORef (modifyIORef, newIORef, readIORef)
import qualified Data.Set as Set
import Sabela.Handlers (killAllSessions)
import Sabela.Handlers.Shared (bumpGeneration, isCurrentGen)
import Sabela.Model (Notebook (..), NotebookEvent (..))
import Sabela.Server (newApp)
import Sabela.State (App (..))
import Sabela.State.EventBus (subscribeBroadcast)
import Sabela.State.NotebookStore (modifyNotebook, readNotebook)
import Test.Hspec

spec :: Spec
spec = describe "Generation counter" $ do
    describe "bumpGeneration" $ do
        it "increments the generation counter" $ do
            app <- newApp "." Set.empty Nothing
            gen1 <- bumpGeneration app
            gen2 <- bumpGeneration app
            gen2 `shouldBe` gen1 + 1

        it "makes old generations stale" $ do
            app <- newApp "." Set.empty Nothing
            gen1 <- bumpGeneration app
            _ <- bumpGeneration app
            stale <- isCurrentGen app gen1
            stale `shouldBe` False

        it "current generation is valid" $ do
            app <- newApp "." Set.empty Nothing
            gen <- bumpGeneration app
            current <- isCurrentGen app gen
            current `shouldBe` True

    describe "notebook load cancellation" $ do
        it "bumpGeneration invalidates previous generation" $ do
            app <- newApp "." Set.empty Nothing
            gen1 <- bumpGeneration app
            -- Simulate loading a new notebook (bumps generation)
            gen2 <- bumpGeneration app
            -- Old generation should be stale
            stale <- isCurrentGen app gen1
            stale `shouldBe` False
            -- New generation should be current
            current <- isCurrentGen app gen2
            current `shouldBe` True

        it "killAllSessions does not crash on fresh app" $ do
            app <- newApp "." Set.empty Nothing
            -- Should not throw even with no sessions
            killAllSessions app
