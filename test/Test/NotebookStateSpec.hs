{-# LANGUAGE OverloadedStrings #-}

module Test.NotebookStateSpec (spec) where

import Control.Concurrent.STM (atomically, tryReadTChan)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson (Value (..), toJSON)
import Data.List (sort)
import qualified Data.Set as Set
import Sabela.Deps (collectMetadata)
import Sabela.Handlers (ReactiveNotebook (..), setupReactive)
import Sabela.Handlers.Lifecycle (neededEnvSig)
import Sabela.Model (Cell (..), NotebookEvent (..), Notebook (..))
import Sabela.State (App (..), newApp, notebookState)
import Sabela.State.EventBus (subscribeBroadcast)
import Sabela.State.NotebookStore (modifyNotebook, readNotebook)
import Sabela.State.SessionManager (installHaskellSession)
import Test.CellFixture (mkCell)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.WriteAckFixture (inertBackend)

-- | A chain: 1 defines x, 2 uses it, 3 uses what 2 defines.
chain :: [Cell]
chain = [mkCell 1 "x = 1", mkCell 2 "y = x + 1", mkCell 3 "z = y + 1"]

settled :: IO (App, ReactiveNotebook)
settled = do
    app <- newApp "." Set.empty Nothing Nothing []
    modifyNotebook (appNotebook app) (\nb -> nb{nbCells = chain})
    nb <- readNotebook (appNotebook app)
    backend <- inertBackend
    installHaskellSession
        (appSessions app)
        backend
        (neededEnvSig app (collectMetadata nb))
    rn <- setupReactive app
    pure (app, rn)

staleFrom :: NotebookEvent -> Maybe [Int]
staleFrom ev = case toJSON ev of
    Object o -> case KM.lookup (Key.fromString "staleIds") o of
        Just v -> case v of
            Array _ -> Just (sort (fromJSONInts v))
            _ -> Nothing
        Nothing -> Nothing
    _ -> Nothing
  where
    fromJSONInts (Array arr) = [round n | Number n <- foldr (:) [] arr]
    fromJSONInts _ = []

spec :: Spec
spec = describe "notebookState (what the browser is told is out of date)" $ do
    it "reports nothing stale for a settled notebook" $ do
        (app, _) <- settled
        ev <- notebookState app
        staleFrom ev `shouldBe` Just []

    it
        "names the edited cell AND its dependents: editing one cell invalidates\
        \ what depends on it, which the client could not otherwise learn"
        $ do
            (app, rn) <- settled
            rnCellEdit rn 1 "x = 99"
            ev <- notebookState app
            staleFrom ev `shouldBe` Just [1, 2, 3]

    it "leaves unrelated cells alone" $ do
        app <- newApp "." Set.empty Nothing Nothing []
        modifyNotebook (appNotebook app) $ \nb ->
            nb{nbCells = [mkCell 1 "x = 1", mkCell 2 "unrelated = 42"]}
        nb <- readNotebook (appNotebook app)
        backend <- inertBackend
        installHaskellSession
            (appSessions app)
            backend
            (neededEnvSig app (collectMetadata nb))
        rn <- setupReactive app
        rnCellEdit rn 1 "x = 99"
        ev <- notebookState app
        staleFrom ev `shouldBe` Just [1]

    it "carries the kernel epoch, so a client can spot a restart" $ do
        (app, _) <- settled
        ev <- notebookState app
        case toJSON ev of
            Object o -> KM.lookup (Key.fromString "epoch") o `shouldSatisfy` isNum
            _ -> fail "expected an object"

    it "is broadcast on an edit, not merely available on request" $ do
        (app, rn) <- settled
        chan <- subscribeBroadcast (appEvents app)
        rnCellEdit rn 1 "x = 99"
        seen <- atomically (tryReadTChan chan)
        (seen >>= staleFrom) `shouldBe` Just [1, 2, 3]
  where
    isNum (Just (Number _)) = True
    isNum _ = False
