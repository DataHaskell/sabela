{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- | C6: deleting a cell must not leave its bindings, imports, or
type-level declarations resident in the live GHCi session — GHCi has no
partial-unbind primitive, so 'Sabela.Server.Notebook.deleteCellH' must
trigger the same full-rebuild path ('Sabela.Handlers.Plan.executeFullRestart')
already used for other structural changes. Distinct from
'Test.KernelStateWireSpec', which only pins the @kernel_status@ JSON shape;
this drives a real GHCi-backed session and checks actual session-content
consistency after a delete, table-driven across the ways interpreter state
can outlive the notebook that produced it.
-}
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

-- | A dirty code cell ready to be picked up by the next run-all/rebuild.
cell :: Int -> Text -> Cell
cell cid src = Cell cid CodeCell ST.Haskell src [] Nothing True

-- | Replace the whole cell list and trigger the reactive run-all path.
seedAndRun :: App -> ReactiveNotebook -> [Cell] -> IO ()
seedAndRun app rn cells = do
    modifyNotebook (appNotebook app) (\nb -> nb{nbCells = cells})
    runAndSettle app (rnRunAll rn)

{- | Delete a cell through the real HTTP handler and let any triggered
rebuild settle before the caller inspects session state.
-}
deleteAndSettle :: App -> Int -> IO ()
deleteAndSettle app cid = runAndSettle app (void (runHandler (deleteCellH app cid)))

{- | Subscribe to the broadcast bus BEFORE triggering the action (so the
fence cannot fire unseen), run it, then block for @EvExecutionDone@ up to
'settleBudgetUs'. The pre-fix bug triggers no cascade at all, so this
simply exhausts the budget once and proves the bug rather than hanging
forever — the bounded timeout is what keeps that proof affordable.
-}
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

-- | Ask the live GHCi session for the type of an expression.
queryType :: App -> Text -> IO Text
queryType app expr = withBackend app "<no live session>" (`ST.sbQueryType` expr)

-- | Run an expression against the live GHCi session, returning (stdout, stderr).
runExpr :: App -> Text -> IO (Text, Text)
runExpr app expr = withBackend app ("", "<no live session>") (`ST.sbRunBlock` expr)

-- | Raw GHCi introspection command (e.g. @:show imports@) via runBlock.
showCommand :: App -> Text -> IO Text
showCommand app cmd = fst <$> runExpr app cmd

withBackend :: App -> a -> (ST.SessionBackend -> IO a) -> IO a
withBackend app onNone act = do
    mSess <- getHaskellSession (appSessions app)
    maybe (pure onNone) act mSess

{- | GHCi failure signal: the textual diagnostic marker shared with
'Sabela.Session.Query.typecheckValueWith', plus the @-fdiagnostics-as-json@
severity field this GHC emits instead ('Sabela.Errors.Json').
-}
isFailure :: Text -> Bool
isFailure out =
    "error:" `T.isInfixOf` out
        || "\"severity\":\"Error\"" `T.isInfixOf` out

{- | The @sabela-notebook@ support source overlay 'installAndRestart' always
needs (it targets @WithNotebookSupport@ unconditionally); without it a
fresh session can never resolve @sabela-notebook@ from Hackage.
-}
newLiveApp :: IO App
newLiveApp = do
    app <- newApp "." Set.empty Nothing Nothing [buildTimeSupportDir]
    _ <- setupReactive app
    pure app

{- | Run one test against a fresh kernel, releasing it afterwards so a leaked
GHCi cannot hold its nursery for the rest of the suite.
-}
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
            -- The surviving type itself must be untouched by deleting the instance.
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
