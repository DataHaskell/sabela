{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- | An edit that changes nothing the kernel sees — a comment, whitespace,
prose — must neither invalidate cells nor dispatch execution.
-}
module Test.EditNoOpSpec (spec) where

import Control.Concurrent (threadDelay)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Unique (newUnique)

import Sabela.Deps (collectMetadata)
import Sabela.Handlers (
    ReactiveNotebook (..),
    setupReactive,
    updateCellSource,
 )
import Sabela.Handlers.Lifecycle (neededEnvSig)
import Sabela.Model (Cell (..), Notebook (..))
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..), newApp)
import Sabela.State.NotebookStore (modifyNotebook, readNotebook)
import Sabela.State.SessionManager (installHaskellSession)
import Test.CellFixture (mkCell, proseCell)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.WriteAckFixture (inertBackend)

nbOf :: [Cell] -> Notebook
nbOf cs = Notebook{nbTitle = "t", nbCells = cs}

cells :: [Cell]
cells = [proseCell 0 "# Heading", mkCell 1 "a = 1", mkCell 2 "b = a + 1"]

cellById :: Int -> Notebook -> Cell
cellById cid nb = fromJust (lookup cid [(cellId c, c) | c <- nbCells nb])

spec :: Spec
spec = do
    describe "updateCellSource on an insignificant edit" $ do
        it "stores the source without invalidating the cell" $ do
            let nb = updateCellSource 1 "a = 1 -- note" (nbOf cells)
            cellSource (cellById 1 nb) `shouldBe` "a = 1 -- note"
            cellDirty (cellById 1 nb) `shouldBe` False

        it "leaves dependents alone" $ do
            let nb = updateCellSource 1 "a = 1\n-- probe" (nbOf cells)
            cellDirty (cellById 2 nb) `shouldBe` False

        it "never invalidates prose" $ do
            let nb = updateCellSource 0 "# Heading edited" (nbOf cells)
            cellSource (cellById 0 nb) `shouldBe` "# Heading edited"
            cellDirty (cellById 0 nb) `shouldBe` False

    describe "updateCellSource on a significant edit" $ do
        it "invalidates the cell and its dependents" $ do
            let nb = updateCellSource 1 "a = 2" (nbOf cells)
            cellDirty (cellById 1 nb) `shouldBe` True
            cellDirty (cellById 2 nb) `shouldBe` True

    describe "handleCellEdit dispatch" $ do
        it "a comment-only edit dispatches nothing" $ do
            (rn, transcript) <- mkReactive
            rnCellEdit rn 1 "a = 1 -- note"
            threadDelay 500_000
            readIORef transcript >>= (`shouldBe` [])

        it "a prose edit dispatches nothing" $ do
            (rn, transcript) <- mkReactive
            rnCellEdit rn 0 "# Heading edited"
            threadDelay 500_000
            readIORef transcript >>= (`shouldBe` [])

mkReactive :: IO (ReactiveNotebook, IORef [Text])
mkReactive = do
    app <- newApp "." Set.empty Nothing Nothing []
    transcript <- newIORef []
    base <- inertBackend
    uid <- newUnique
    let record t = modifyIORef' transcript (++ [t]) >> pure ("", "")
        backend =
            base
                { ST.sbSessionId = uid
                , ST.sbRunBlock = record
                , ST.sbRunBlockStreaming = \t _ -> record t
                }
    modifyNotebook (appNotebook app) (\nb -> nb{nbCells = cells})
    nb <- readNotebook (appNotebook app)
    installHaskellSession
        (appSessions app)
        backend
        (neededEnvSig app (collectMetadata nb))
    rn <- setupReactive app
    pure (rn, transcript)
