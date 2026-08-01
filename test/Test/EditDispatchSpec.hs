{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.EditDispatchSpec (spec) where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import qualified Data.Set as Set
import Data.Unique (newUnique)
import GHC.Clock (getMonotonicTimeNSec)
import Sabela.Deps (collectMetadata)
import Sabela.Handlers (ReactiveNotebook (..), setupReactive)
import Sabela.Handlers.Lifecycle (neededEnvSig)
import Sabela.Model (Cell (..), CellType (..), Notebook (..))
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..), newApp)
import Sabela.State.NotebookStore (modifyNotebook, readNotebook)
import Sabela.State.SessionManager (installHaskellSession)
import System.Timeout (timeout)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.WriteAckFixture (inertBackend)

-- | A kernel whose every run blocks until released.
blockingBackend :: IO (ST.SessionBackend, IO ())
blockingBackend = do
    gate <- newEmptyMVar
    base <- inertBackend
    uid <- newUnique
    let backend =
            base
                { ST.sbSessionId = uid
                , ST.sbRunBlock = \_ -> takeMVar gate >> pure ("", "")
                , ST.sbRunBlockStreaming = \_ _ -> takeMVar gate >> pure ("", "")
                }
    pure (backend, putMVar gate ())

seeded :: Cell
seeded =
    Cell
        { cellId = 1
        , cellType = CodeCell
        , cellLang = ST.Haskell
        , cellSource = "x = 1"
        , cellOutputs = []
        , cellError = Nothing
        , cellDirty = False
        }

spec :: Spec
spec = describe "handleCellEdit dispatch" $
    it
        "returns before the cell finishes running, so a synchronous PUT is not\
        \ held open for the length of an execution"
        $ do
            app <- newApp "." Set.empty Nothing Nothing []
            (backend, release) <- blockingBackend
            modifyNotebook (appNotebook app) $ \nb ->
                nb{nbCells = [seeded]}
            nb <- readNotebook (appNotebook app)
            installHaskellSession
                (appSessions app)
                backend
                (neededEnvSig app (collectMetadata nb))
            rn <- setupReactive app
            t0 <- getMonotonicTimeNSec
            _ <- timeout 5_000_000 (rnCellEdit rn 1 "x = 2")
            t1 <- getMonotonicTimeNSec
            release
            -- Elapsed, not timeout's verdict: execCellWith catches
            -- SomeException, so it swallows the async Timeout and reports
            -- completion either way.
            ((t1 - t0) `div` 1_000_000 < 2_000) `shouldBe` True
