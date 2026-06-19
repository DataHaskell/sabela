{-# LANGUAGE OverloadedStrings #-}

{- | Pins the compile-reload contract: a compile phase that actually issues
@:load@ wipes every GHCi prompt binding, so the executor must mark all
interpreted cells dirty and re-run the full interpreted set — not just the
compiled cell's textual dependents.
-}
module Test.CompileEscalationSpec (spec) where

import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Unique (newUnique)

import Sabela.Compiled (CompilePlan (..))
import Sabela.Handlers.Plan (runPlanPhases)
import Sabela.Handlers.Shared (bumpGeneration)
import Sabela.Model (Cell (..), CellType (..), Notebook (..))
import Sabela.Reactivity (
    ExecutionPlan (..),
    computeExecutionPlan,
    computeStaleExecutionPlan,
    escalatedCellsToRun,
    markAllInterpretedDirty,
 )
import Sabela.Server (newApp)
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..))
import Sabela.State.NotebookStore (modifyNotebook, readNotebook)
import Sabela.State.SessionManager (setHaskellSession)
import Test.Hspec
import Test.TopoSpec.Helpers (mkCell)

nbOf :: [Cell] -> Notebook
nbOf cs = Notebook{nbTitle = "t", nbCells = cs}

{- | One compiled module, one dependent of it (3), and a chain (2 → 4)
entirely unrelated to the module — the cells a reload still wipes.
-}
cells :: [Cell]
cells =
    [ mkCell 1 "-- compile: Model\nstep x = x + 1"
    , mkCell 2 "a = 1"
    , mkCell 3 "b = step 2"
    , mkCell 4 "c = a + 1"
    ]

-- | A backend that records every executed block and always succeeds.
fakeBackend :: IORef [Text] -> (Text -> (Text, Text)) -> IO ST.SessionBackend
fakeBackend transcript respond = do
    uid <- newUnique
    let record t = modifyIORef' transcript (++ [t]) >> pure (respond t)
        backend =
            ST.SessionBackend
                { ST.sbSessionId = uid
                , ST.sbRunBlock = record
                , ST.sbRunBlockStreaming = \t _ -> record t
                , ST.sbClose = pure ()
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

-- | App wired to a fake session, with @cells@ as the notebook.
mkApp :: (Text -> (Text, Text)) -> IO (App, IORef [Text])
mkApp respond = do
    app <- newApp "." Set.empty Nothing Nothing []
    transcript <- newIORef []
    backend <- fakeBackend transcript respond
    setHaskellSession (appSessions app) (Just backend)
    modifyNotebook (appNotebook app) (\nb -> nb{nbCells = cells})
    pure (app, transcript)

alwaysOk :: Text -> (Text, Text)
alwaysOk _ = ("", "")

{- | Indices of transcript entries containing @needle@, with the display
prelude (re-run before every cell, full of coincidental substrings)
filtered out first.
-}
hits :: Text -> [Text] -> [Int]
hits needle ts =
    [ i
    | (i, t) <- zip [0 ..] (filter (not . T.isInfixOf "_sabelaScatterJs") ts)
    , needle `T.isInfixOf` t
    ]

spec :: Spec
spec = describe "compile-reload escalation" $ do
    describe "escalatedCellsToRun" $ do
        it "covers every interpreted cell in dependency order" $
            map cellId (escalatedCellsToRun (nbOf cells))
                `shouldBe` [2, 3, 4]

        it "is a strict superset of the incremental plan's run set" $ do
            -- The regression pin: rooted at the compiled cell, the
            -- incremental plan omits cells 2 and 4 — the ones a reload
            -- still wipes.
            let plan = computeExecutionPlan 1 cells (nbOf cells)
            map cellId (epCellsToRun plan) `shouldBe` [3]

        it "excludes compiled cells and skip-set cells" $ do
            let cs = cells ++ [mkCell 5 "a = 2"]
            map cellId (escalatedCellsToRun (nbOf cs))
                `shouldBe` [2, 3, 4]

    describe "markAllInterpretedDirty" $ do
        let prose = (mkCell 6 "notes"){cellType = ProseCell}
            python = (mkCell 7 "x = 1"){cellLang = ST.Python}
            marked = markAllInterpretedDirty (nbOf (cells ++ [prose, python]))

        it "dirties interpreted Haskell code cells only" $
            [(cellId c, cellDirty c) | c <- nbCells marked]
                `shouldBe` [ (1, False)
                           , (2, True)
                           , (3, True)
                           , (4, True)
                           , (6, False)
                           , (7, False)
                           ]

        it "makes the stale plan recover every interpreted cell" $ do
            -- An interrupted recovery run must stay recoverable: the
            -- dirty marks alone make the next stale run-all re-run all.
            let stale = computeStaleExecutionPlan (nbCells marked) marked
            map cellId (epCellsToRun stale) `shouldBe` [2, 3, 4]

    describe "runPlanPhases after a compiled-cell edit" $ do
        it "a changed module reloads, then re-runs every interpreted cell" $ do
            (app, transcript) <- mkApp alwaysOk
            -- Seed: Model is loaded with stale content, so the phase reloads.
            writeIORef (appCompiledModules app) (M.singleton "Model" "old")
            nb <- readNotebook (appNotebook app)
            gen <- bumpGeneration app
            runPlanPhases app gen (computeExecutionPlan 1 cells nb)
            ts <- readIORef transcript
            let loads = hits ":load" ts
            length loads `shouldBe` 1
            -- Every interpreted cell ran, after the load, in plan order.
            let runs = [hits src ts | src <- ["a = 1", "b = step 2", "c = a + 1"]]
            map length runs `shouldBe` [1, 1, 1]
            let [[ia], [ib], [ic]] = runs
            ia `shouldSatisfy` (> head loads)
            ib `shouldSatisfy` (> ia)
            ic `shouldSatisfy` (> ib)

        it "an unchanged module neither reloads nor escalates" $ do
            (app, transcript) <- mkApp alwaysOk
            nb <- readNotebook (appNotebook app)
            let plan = computeExecutionPlan 1 cells nb
            writeIORef
                (appCompiledModules app)
                (cpModulesOf plan)
            gen <- bumpGeneration app
            runPlanPhases app gen plan
            ts <- readIORef transcript
            hits ":load" ts `shouldBe` []
            hits "a = 1" ts `shouldBe` []
            length (hits "b = step 2" ts) `shouldBe` 1

        it "a failed reload escalates too, skipping only compiled dependents" $ do
            let failLoads t
                    | ":load" `T.isInfixOf` t =
                        ("Failed, no modules loaded.", "sabela-cell-1:2:1: error:\n    boom")
                    | otherwise = ("", "")
            (app, transcript) <- mkApp failLoads
            writeIORef (appCompiledModules app) (M.singleton "Model" "old")
            nb <- readNotebook (appNotebook app)
            gen <- bumpGeneration app
            runPlanPhases app gen (computeExecutionPlan 1 cells nb)
            ts <- readIORef transcript
            -- The wipe happened, so unrelated cells re-run; the compiled
            -- module's dependent is blocked instead.
            length (hits "a = 1" ts) `shouldBe` 1
            length (hits "c = a + 1" ts) `shouldBe` 1
            hits "b = step 2" ts `shouldBe` []
            -- The blocked dependent carries an error, so it stays stale.
            nb' <- readNotebook (appNotebook app)
            let Just blocked = lookup 3 [(cellId c, c) | c <- nbCells nb']
            cellError blocked `shouldSatisfy` (/= Nothing)

cpModulesOf :: ExecutionPlan -> M.Map Text Text
cpModulesOf = cpModules . epCompilePlan
