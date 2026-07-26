{-# LANGUAGE OverloadedStrings #-}

module Sabela.Session.MaterializeSnapshot (
    MaterializeSnapshot (..),
    captureMaterializeSnapshot,
    snapshotStillCurrent,
    withCurrentSnapshot,
) where

import Control.Concurrent.MVar (withMVar)
import Data.IORef (readIORef)
import qualified Data.Map.Strict as M
import Data.Text (Text)

import Sabela.Model (Notebook)
import Sabela.State (App (..))
import qualified Sabela.State.BridgeStore as BridgeStore
import qualified Sabela.State.EventBus as EventBus
import qualified Sabela.State.NotebookStore as NotebookStore
import qualified Sabela.State.WidgetStore as WidgetStore

data MaterializeSnapshot = MaterializeSnapshot
    { msNotebook :: Notebook
    , msEventGeneration :: Int
    , msBridgeValues :: M.Map Text Text
    , msWidgetValues :: M.Map Int (M.Map Text Text)
    }
    deriving (Eq)

snapshotCaptureRetries :: Int
snapshotCaptureRetries = 3

captureMaterializeSnapshot :: App -> IO (Either Text MaterializeSnapshot)
captureMaterializeSnapshot app = attempt (snapshotCaptureRetries + 1)
  where
    attempt remaining = do
        generationBefore <- readGeneration app
        snapshot <- copyMaterializeStores app generationBefore
        generationAfter <- readGeneration app
        if generationBefore == generationAfter
            then pure (Right snapshot)
            else
                if remaining > 1
                    then attempt (remaining - 1)
                    else
                        pure
                            ( Left
                                "notebook changed while capturing disposable context; no candidate code ran"
                            )

copyMaterializeStores :: App -> Int -> IO MaterializeSnapshot
copyMaterializeStores app generation =
    withMVar (NotebookStore.nsNotebook (appNotebook app)) $ \notebook ->
        withMVar (BridgeStore.bsValues (appBridge app)) $ \bridge ->
            withMVar (WidgetStore.wsValues (appWidgets app)) $ \widgets ->
                pure
                    MaterializeSnapshot
                        { msNotebook = notebook
                        , msEventGeneration = generation
                        , msBridgeValues = bridge
                        , msWidgetValues = widgets
                        }

readGeneration :: App -> IO Int
readGeneration = readIORef . EventBus.ebGeneration . appEvents

snapshotStillCurrent :: App -> MaterializeSnapshot -> IO (Either Text ())
snapshotStillCurrent app expected = do
    current <- captureMaterializeSnapshot app
    pure $ case current of
        Left message -> Left message
        Right actual
            | actual == expected -> Right ()
            | otherwise ->
                Left
                    "notebook or render context changed during disposable materialization; no candidate code ran"

withCurrentSnapshot ::
    App ->
    MaterializeSnapshot ->
    IO a ->
    IO (Either Text a)
withCurrentSnapshot app expected action =
    withMVar (NotebookStore.nsNotebook (appNotebook app)) $ \notebook ->
        withMVar (BridgeStore.bsValues (appBridge app)) $ \bridge ->
            withMVar (WidgetStore.wsValues (appWidgets app)) $ \widgets -> do
                generationBefore <- readGeneration app
                if notebook /= msNotebook expected
                    || bridge /= msBridgeValues expected
                    || widgets /= msWidgetValues expected
                    || generationBefore /= msEventGeneration expected
                    then
                        pure
                            ( Left
                                "notebook or render context changed during disposable materialization; no candidate code ran"
                            )
                    else do
                        result <- action
                        generationAfter <- readGeneration app
                        pure $
                            if generationAfter == msEventGeneration expected
                                then Right result
                                else
                                    Left
                                        "notebook generation changed while the isolated candidate ran; its result was discarded"
