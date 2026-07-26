{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.KernelStateIntegritySpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, readTChan)
import Control.Exception (bracket)
import Control.Monad (unless, void, when)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Servant (runHandler)
import System.Directory (doesFileExist, findExecutable)
import System.FilePath ((</>))
import System.Timeout (timeout)
import Test.Hspec

import Sabela.AI.Capabilities.Kernel (haskellKernelOccupied)
import Sabela.Handlers (
    ReactiveNotebook (..),
    buildTimeSupportDir,
    setupReactive,
 )
import Sabela.Model (
    Cell (..),
    CellType (..),
    Notebook (..),
    NotebookEvent (..),
 )
import Sabela.Server (newApp)
import Sabela.Server.Notebook (deleteCellH)
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..))
import Sabela.State.EventBus (subscribeBroadcast)
import Sabela.State.NotebookStore (modifyNotebook)
import Sabela.State.SessionManager (
    forceResetAllSessions,
    getHaskellSession,
 )

cell :: Int -> Text -> Cell
cell cid src = Cell cid CodeCell ST.Haskell src [] Nothing True

seedAndRun :: App -> ReactiveNotebook -> [Cell] -> IO ()
seedAndRun app rn cells = do
    modifyNotebook (appNotebook app) (\nb -> nb{nbCells = cells})
    runAndSettle app (rnRunAll rn)

deleteAndSettle :: App -> Int -> IO ()
deleteAndSettle app cid = runAndSettle app (void (runHandler (deleteCellH app cid)))

runAndSettle :: App -> IO () -> IO ()
runAndSettle app act = do
    chan <- subscribeBroadcast (appEvents app)
    act
    _ <- timeout settleBudgetUs (waitDone chan)
    settleIdle (200 :: Int)
  where
    waitDone chan = do
        ev <- atomically (readTChan chan)
        case ev of
            EvExecutionDone -> pure ()
            _ -> waitDone chan
    settleIdle :: Int -> IO ()
    settleIdle 0 = pure ()
    settleIdle n = do
        busy <- haskellKernelOccupied app
        when busy $ threadDelay 50_000 >> settleIdle (n - 1)

settleBudgetUs :: Int
settleBudgetUs = 90_000_000

queryType :: App -> Text -> IO Text
queryType app expr = withBackend app "<no live session>" (`ST.sbQueryType` expr)

runExpr :: App -> Text -> IO (Text, Text)
runExpr app expr = withBackend app ("", "<no live session>") (`ST.sbRunBlock` expr)

showCommand :: App -> Text -> IO Text
showCommand app cmd = fst <$> runExpr app cmd

withBackend :: App -> a -> (ST.SessionBackend -> IO a) -> IO a
withBackend app onNone act = do
    mSess <- getHaskellSession (appSessions app)
    maybe (pure onNone) act mSess

isFailure :: Text -> Bool
isFailure out =
    "error:" `T.isInfixOf` out
        || "\"severity\":\"Error\"" `T.isInfixOf` out

newLiveApp :: IO App
newLiveApp = do
    app <- newApp "." Set.empty Nothing Nothing [buildTimeSupportDir]
    _ <- setupReactive app
    pure app

withLiveApp :: (App -> IO a) -> IO a
withLiveApp = bracket newLiveApp (forceResetAllSessions . appSessions)

withCabal :: IO () -> IO ()
withCabal act = do
    mCabal <- findExecutable "cabal"
    supportPresent <-
        doesFileExist (buildTimeSupportDir </> "sabela-notebook.cabal")
    case (mCabal, supportPresent) of
        (Nothing, _) -> pendingWith "cabal not found on PATH; skipping integration test"
        (_, False) -> pendingWith "sabela-notebook support source not on disk; skipping"
        (Just _, True) -> act

spec :: Spec
spec = describe "delete-cell session integrity (C6)" $ do
    it "variable/binding: a deleted cell's binding no longer resolves live" $ withCabal $ withLiveApp $ \app -> do
        rn <- setupReactive app
        seedAndRun app rn [cell 1 "x = (5 :: Int)"]
        beforeType <- queryType app "x"
        beforeType `shouldSatisfy` T.isInfixOf "x :: "

        deleteAndSettle app 1

        afterType <- queryType app "x"
        afterType `shouldSatisfy` isFailure

    it "module/import: a deleted import cell's import no longer resolves live" $ withCabal $ withLiveApp $ \app -> do
        rn <- setupReactive app
        seedAndRun
            app
            rn
            [cell 1 "-- cabal: build-depends: containers\nimport qualified Data.Map as M"]
        beforeType <- queryType app "M.fromList [(1 :: Int, 2 :: Int)]"
        beforeType `shouldSatisfy` not . isFailure
        importsBefore <- showCommand app ":show imports"
        importsBefore `shouldSatisfy` T.isInfixOf "Data.Map"

        deleteAndSettle app 1

        afterType <- queryType app "M.fromList [(1 :: Int, 2 :: Int)]"
        afterType `shouldSatisfy` isFailure
        importsAfter <- showCommand app ":show imports"
        importsAfter `shouldNotSatisfy` T.isInfixOf "Data.Map"

    it "type-level: a deleted data declaration no longer resolves live" $ withCabal $ withLiveApp $ \app -> do
        rn <- setupReactive app
        seedAndRun app rn [cell 1 "data Foo = Foo Int"]
        beforeType <- queryType app "Foo 1"
        beforeType `shouldSatisfy` T.isInfixOf "Foo 1 :: Foo"

        deleteAndSettle app 1

        afterType <- queryType app "Foo 1"
        afterType `shouldSatisfy` isFailure

    it
        "type-level: a deleted instance is no longer selected by instance\
        \ resolution, while the surviving type stays intact"
        $ withCabal
        $ withLiveApp
        $ \app -> do
            rn <- setupReactive app
            seedAndRun
                app
                rn
                [ cell 1 "data Bar = Bar Int"
                , cell 2 "instance Show Bar where\n  show (Bar n) = \"Bar:\" ++ show n"
                ]
            beforeType <- queryType app "show (Bar 1)"
            beforeType `shouldSatisfy` T.isInfixOf ":: String"

            deleteAndSettle app 2

            afterInstance <- queryType app "show (Bar 1)"
            afterInstance `shouldSatisfy` isFailure
            afterType <- queryType app "Bar 1"
            afterType `shouldSatisfy` T.isInfixOf "Bar 1 :: Bar"

    it
        "unrelated-cell preservation: deleting one cell leaves another's binding intact"
        $ withCabal
        $ withLiveApp
        $ \app -> do
            rn <- setupReactive app
            seedAndRun
                app
                rn
                [cell 1 "delMe = (5 :: Int)", cell 2 "keepMe = (10 :: Int)"]

            deleteAndSettle app 1

            deleted <- queryType app "delMe"
            deleted `shouldSatisfy` isFailure
            (keepOut, keepErr) <- runExpr app "keepMe"
            T.strip keepOut `shouldBe` "10"
            unless (T.null keepErr) $
                expectationFailure ("keepMe errored: " <> T.unpack keepErr)

    it
        "dependency/extension residue: deleting the only cell needing an\
        \ extension drops it from the rebuilt session for free"
        $ withCabal
        $ withLiveApp
        $ \app -> do
            rn <- setupReactive app
            seedAndRun
                app
                rn
                [cell 1 "{-# LANGUAGE TupleSections #-}\ntsVal = (1,) (2 :: Int) :: (Int, Int)"]
            (beforeOut, beforeErr) <- runExpr app "tsVal"
            T.strip beforeOut `shouldBe` "(1,2)"
            beforeErr `shouldSatisfy` T.null

            deleteAndSettle app 1

            afterTupleSection <- queryType app "(1,) (2 :: Int)"
            afterTupleSection `shouldSatisfy` isFailure
