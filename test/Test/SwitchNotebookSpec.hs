{-# LANGUAGE OverloadedStrings #-}

{- | Pins the notebook-switch contract: loading a different notebook tears
down the live Haskell session, so the next run rebuilds against the loaded
notebook. Previously the load left the session alive (only @:reload@-ing it,
which wiped its bindings) while the dependency tracker still matched, so the
next Run All took the stale path, found every freshly-loaded cell clean, and
executed nothing.
-}
module Test.SwitchNotebookSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (isNothing)
import qualified Data.Set as Set
import qualified Data.Text.IO as TIO
import Data.Unique (newUnique)
import Servant (runHandler)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import Sabela.Api (LoadRequest (..))
import Sabela.Handlers (setupReactive)
import Sabela.Server (newApp)
import Sabela.Server.Notebook (loadNotebookH)
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..))
import Sabela.State.SessionManager (getHaskellSession, setHaskellSession)
import Test.Hspec

-- | A fake backend that records when it is closed.
fakeBackend :: IORef Bool -> IO ST.SessionBackend
fakeBackend closed = do
    uid <- newUnique
    let backend =
            ST.SessionBackend
                { ST.sbSessionId = uid
                , ST.sbRunBlock = \_ -> pure ("", "")
                , ST.sbRunBlockStreaming = \_ _ -> pure ("", "")
                , ST.sbClose = writeIORef closed True
                , ST.sbReset = pure backend
                , ST.sbInterrupt = pure ()
                , ST.sbBusy = pure False
                , ST.sbSessionGen = pure 0
                , ST.sbRequestStale = \_ -> pure False
                , ST.sbQueryComplete = \_ -> pure []
                , ST.sbQueryType = \_ -> pure ""
                , ST.sbQueryInfo = \_ -> pure ""
                , ST.sbQueryKind = \_ -> pure ""
                , ST.sbQueryBrowse = \_ -> pure ""
                , ST.sbQueryDoc = \_ -> pure ""
                }
    pure backend

-- | Retry @check@ briefly so an off-thread teardown is not raced.
waitFor :: IO Bool -> IO Bool
waitFor check = go (50 :: Int)
  where
    go 0 = check
    go n = do
        ok <- check
        if ok then pure True else threadDelay 10000 >> go (n - 1)

spec :: Spec
spec = describe "loadNotebookH" $ do
    it "detaches the live session so the next run rebuilds, not reuses it" $
        withSystemTempDirectory "sabela-switch" $ \dir -> do
            TIO.writeFile (dir </> "b.md") "```haskell\nx = 1\n```\n"
            app <- newApp dir Set.empty Nothing Nothing []
            rn <- setupReactive app
            closed <- newIORef False
            backend <- fakeBackend closed
            setHaskellSession (appSessions app) (Just backend)
            void $ runHandler (loadNotebookH app rn (LoadRequest "b.md"))
            mSess <- getHaskellSession (appSessions app)
            isNothing mSess `shouldBe` True

    it "closes the previous session's backend to reclaim it" $
        withSystemTempDirectory "sabela-switch" $ \dir -> do
            TIO.writeFile (dir </> "b.md") "```haskell\nx = 1\n```\n"
            app <- newApp dir Set.empty Nothing Nothing []
            rn <- setupReactive app
            closed <- newIORef False
            backend <- fakeBackend closed
            setHaskellSession (appSessions app) (Just backend)
            void $ runHandler (loadNotebookH app rn (LoadRequest "b.md"))
            waitFor (readIORef closed) `shouldReturn` True
