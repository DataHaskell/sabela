{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- | An explicit Run of one cell is reactive beyond its root: the editor sends
a PUT and then a run, and the run's generation bump supersedes the edit's plan,
so the run itself must carry the consumers or they stay dirty forever.
-}
module Test.RunDispatchSpec (spec) where

import Control.Concurrent (threadDelay)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Unique (newUnique)

import Sabela.Deps (collectMetadata)
import Sabela.Handlers (ReactiveNotebook (..), setupReactive)
import Sabela.Handlers.Lifecycle (neededEnvSig)
import Sabela.Model (Cell (..), Notebook (..))
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..), newApp)
import Sabela.State.NotebookStore (modifyNotebook, readNotebook)
import Sabela.State.SessionManager (installHaskellSession)
import Test.CellFixture (mkCell, proseCell)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.WriteAckFixture (inertBackend)

cells :: [Cell]
cells =
    [ proseCell 0 "# Heading"
    , mkCell 1 "factor = 3"
    , mkCell 2 "scaled = factor * 2"
    , mkCell 3 "unrelated = 0"
    ]

spec :: Spec
spec = describe "an explicit Run of one cell" $ do
    it "runs the cell and then its consumers, in order" $ do
        (rn, transcript) <- mkReactive
        rnRunCellForced rn 1
        threadDelay 500_000
        ran <- cellsRun <$> readIORef transcript
        ran `shouldBe` [1, 2]

    it "runs a leaf alone" $ do
        (rn, transcript) <- mkReactive
        rnRunCellForced rn 2
        threadDelay 500_000
        ran <- cellsRun <$> readIORef transcript
        ran `shouldBe` [2]

    it "does not reach cells outside the root's downstream" $ do
        (rn, transcript) <- mkReactive
        rnRunCellForced rn 3
        threadDelay 500_000
        ran <- cellsRun <$> readIORef transcript
        ran `shouldBe` [3]

-- | Which notebook cells a kernel transcript ran, by their source, prelude loads aside.
cellsRun :: [Text] -> [Int]
cellsRun transcript =
    [ cellId c
    | t <- transcript
    , c <- cells
    , not (T.null (cellSource c))
    , cellSource c `T.isInfixOf` t
    , not ("# Heading" `T.isInfixOf` t)
    ]

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
