{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.DeferredModeSpec (spec) where

import Control.Concurrent (threadDelay)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Unique (newUnique)

import Control.Concurrent.STM (atomically, newTVarIO, readTChan)
import Data.Aeson (Value (..), object, toJSON, (.=))
import Sabela.AI.Capabilities (commitAcceptedEdit)
import Sabela.AI.Capabilities.Try.Payload.Checked (
    RunRecord (..),
    runRecordOf,
 )
import Sabela.AI.CellResult (deferredCellResult)
import qualified Sabela.AI.Store as AIStore
import Sabela.AI.Types (
    AiEdit (..),
    EditStatus (..),
    ToolOutcome (..),
    toolOutcomeValue,
 )
import Sabela.Deps (collectMetadata)
import Sabela.Handlers (ReactiveNotebook (..), applyRunMode, setupReactive)
import Sabela.Handlers.Lifecycle (neededEnvSig)
import Sabela.Ids (EditId (..))
import Sabela.Model (
    Cell (..),
    CellType (..),
    Notebook (..),
    NotebookEvent (..),
    RunMode (..),
 )
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..), getRunMode, newApp, setRunMode)
import Sabela.State.EventBus (subscribeBroadcast)
import Sabela.State.NotebookStore (modifyNotebook, readNotebook)
import Sabela.State.SessionManager (installHaskellSession)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.WriteAckFixture (
    callTool,
    field,
    inertBackend,
    mkFixture,
    textField,
 )

-- | A kernel that records every script it is asked to run.
recordingBackend :: IO (ST.SessionBackend, IO [Text])
recordingBackend = do
    scripts <- newIORef []
    base <- inertBackend
    uid <- newUnique
    let record s = modifyIORef' scripts (s :)
        backend =
            base
                { ST.sbSessionId = uid
                , ST.sbRunBlock = \s -> record s >> pure ("", "")
                , ST.sbRunBlockStreaming = \s _ -> record s >> pure ("", "")
                }
    pure (backend, readIORef scripts)

cellWith :: Int -> Text -> Cell
cellWith cid src =
    Cell
        { cellId = cid
        , cellType = CodeCell
        , cellLang = ST.Haskell
        , cellSource = src
        , cellOutputs = []
        , cellError = Nothing
        , cellDirty = False
        }

-- | An app over two dependent cells, a recording kernel, and a fresh env.
mkDeferredFixture :: IO (App, ReactiveNotebook, IO [Text])
mkDeferredFixture = do
    app <- newApp "." Set.empty Nothing Nothing []
    (backend, getScripts) <- recordingBackend
    modifyNotebook (appNotebook app) $ \nb ->
        nb{nbCells = [cellWith 1 "x = 1", cellWith 2 "y = x + 1"]}
    nb <- readNotebook (appNotebook app)
    installHaskellSession
        (appSessions app)
        backend
        (neededEnvSig app (collectMetadata nb))
    rn <- setupReactive app
    pure (app, rn, getScripts)

-- | The two-cell fixture plus an AI store, for driving the tool surface.
mkToolFixture :: IO (App, AIStore.AIStore, ReactiveNotebook, IO [Text])
mkToolFixture = do
    (app, store) <- mkFixture
    (backend, getScripts) <- recordingBackend
    modifyNotebook (appNotebook app) $ \nb ->
        nb{nbCells = [cellWith 1 "x = 1", cellWith 2 "y = x + 1"]}
    nb <- readNotebook (appNotebook app)
    installHaskellSession
        (appSessions app)
        backend
        (neededEnvSig app (collectMetadata nb))
    rn <- setupReactive app
    pure (app, store, rn, getScripts)

ranCell :: Text -> [Text] -> Int
ranCell needle = length . filter (T.isInfixOf needle)

-- | Poll until the predicate holds or ~5s pass; deferred work is forked.
eventually :: IO Bool -> IO Bool
eventually check = go (50 :: Int)
  where
    go 0 = check
    go n = do
        ok <- check
        if ok then pure True else threadDelay 100_000 >> go (n - 1)

dirtyIds :: App -> IO [Int]
dirtyIds app = do
    nb <- readNotebook (appNotebook app)
    pure [cellId c | c <- nbCells nb, cellDirty c]

spec :: Spec
spec = describe "deferred run mode" $ do
    it "a deferred edit marks the cell and its dependents dirty without running" $ do
        (app, rn, getScripts) <- mkDeferredFixture
        setRunMode app RunDeferred
        rnCellEdit rn 1 "x = 2"
        threadDelay 300_000
        scripts <- getScripts
        ranCell "x = 2" scripts `shouldBe` 0
        dirty <- dirtyIds app
        dirty `shouldBe` [1, 2]

    it "the reactive default still executes on edit" $ do
        (_, rn, getScripts) <- mkDeferredFixture
        rnCellEdit rn 1 "x = 2"
        ran <- eventually ((> 0) . ranCell "x = 2" <$> getScripts)
        ran `shouldBe` True

    it "one drain runs each stale cell exactly once and clears the marks" $ do
        (app, rn, getScripts) <- mkDeferredFixture
        setRunMode app RunDeferred
        rnCellEdit rn 1 "x = 2"
        rnCellEdit rn 2 "y = x + 2"
        threadDelay 300_000
        before <- getScripts
        ranCell "x = 2" before + ranCell "y = x + 2" before `shouldBe` 0
        rnRunAll rn
        drained <-
            eventually $ do
                scripts <- getScripts
                dirty <- dirtyIds app
                pure
                    ( ranCell "x = 2" scripts == 1
                        && ranCell "y = x + 2" scripts == 1
                        && null dirty
                    )
        drained `shouldBe` True
        after <- getScripts
        ranCell "x = 2" after `shouldBe` 1
        ranCell "y = x + 2" after `shouldBe` 1

    it "a deferred widget change marks the cell and consumers stale, runs nothing" $ do
        (app, rn, getScripts) <- mkDeferredFixture
        setRunMode app RunDeferred
        rnWidgetCell rn 1
        threadDelay 300_000
        scripts <- getScripts
        length scripts `shouldBe` 0
        dirty <- dirtyIds app
        dirty `shouldBe` [1, 2]

    it "switching the mode broadcasts EvRunMode" $ do
        (app, rn, _) <- mkDeferredFixture
        chan <- subscribeBroadcast (appEvents app)
        applyRunMode app rn RunDeferred
        ev <- atomically (readTChan chan)
        let isDeferredEv = case ev of
                EvRunMode RunDeferred -> True
                _ -> False
        isDeferredEv `shouldBe` True

    it "the set_run_mode tool switches the mode and reports pending ids" $ do
        (app, store, rn, _) <- mkToolFixture
        out <-
            callTool app store rn "set_run_mode" (object ["mode" .= ("deferred" :: Text)])
        textField "mode" (toolOutcomeValue out) `shouldBe` Just "deferred"
        mode <- getRunMode app
        mode `shouldBe` RunDeferred

    it "the set_run_mode tool rejects an unknown mode" $ do
        (app, store, rn, _) <- mkToolFixture
        out <-
            callTool app store rn "set_run_mode" (object ["mode" .= ("lazy" :: Text)])
        let isErr = case out of
                ToolErr _ -> True
                _ -> False
        isErr `shouldBe` True

    it "the run_pending tool reports the stale set and drains it" $ do
        (app, store, rn, getScripts) <- mkToolFixture
        setRunMode app RunDeferred
        rnCellEdit rn 1 "x = 9"
        threadDelay 300_000
        out <- callTool app store rn "run_pending" (object [])
        field "pending" (toolOutcomeValue out)
            `shouldBe` Just (toJSON ([1, 2] :: [Int]))
        drained <-
            eventually $ do
                scripts <- getScripts
                dirty <- dirtyIds app
                pure (ranCell "x = 9" scripts == 1 && null dirty)
        drained `shouldBe` True

    it "an accepted patch in deferred mode commits and marks stale, no run" $ do
        (app, store, rn, getScripts) <- mkToolFixture
        setRunMode app RunDeferred
        st <- newTVarIO Pending
        let edit = AiEdit (EditId 7) 1 "x = 1" "x = 41" st Nothing
        mCell <- commitAcceptedEdit app store rn edit
        fmap cellSource mCell `shouldBe` Just "x = 41"
        threadDelay 300_000
        scripts <- getScripts
        ranCell "x = 41" scripts `shouldBe` 0
        dirty <- dirtyIds app
        dirty `shouldBe` [1, 2]

    it "an accepted patch in reactive mode still runs the cell" $ do
        (app, store, rn, getScripts) <- mkToolFixture
        st <- newTVarIO Pending
        let edit = AiEdit (EditId 8) 1 "x = 1" "x = 43" st Nothing
        _ <- commitAcceptedEdit app store rn edit
        ran <- eventually ((> 0) . ranCell "x = 43" <$> getScripts)
        ran `shouldBe` True

    it "a Deferred execution summary reads as not-attempted, not a run" $ do
        runRecordOf (toJSON deferredCellResult) `shouldBe` RunNotAttempted
        runRecordOf Null `shouldBe` RunNotAttempted

    it "flipping back to reactive drains the deferred set" $ do
        (app, rn, getScripts) <- mkDeferredFixture
        applyRunMode app rn RunDeferred
        rnCellEdit rn 1 "x = 3"
        threadDelay 300_000
        before <- getScripts
        ranCell "x = 3" before `shouldBe` 0
        applyRunMode app rn RunReactive
        drained <-
            eventually $ do
                scripts <- getScripts
                dirty <- dirtyIds app
                pure (ranCell "x = 3" scripts == 1 && null dirty)
        drained `shouldBe` True
