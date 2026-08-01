{-# LANGUAGE OverloadedStrings #-}

module Test.CompileEscalationSpec (spec) where

import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Unique (newUnique)

import Sabela.Compiled (CompilePlan (..))
import Sabela.Deps (collectMetadata)
import Sabela.Handlers.Lifecycle (neededEnvSig)
import Sabela.Handlers.Plan (planInputs, runPlanPhases)
import Sabela.Handlers.Shared (bumpGeneration)
import Sabela.Model (Cell (..), CellType (..), Notebook (..))
import Sabela.Reactivity (
    ExecutionPlan (..),
    ModuleState (..),
    computeExecutionPlan,
    computeExecutionPlanIn,
    computeStaleExecutionPlan,
    escalatedCellsToRun,
 )
import Sabela.Server (newApp)
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..), setLoadedModules)
import Sabela.State.NotebookStore (modifyNotebook, readNotebook)
import Sabela.State.SessionManager (getHaskellSession, installHaskellSession)
import Test.CellFixture (mkCell)
import Test.Hspec

nbOf :: [Cell] -> Notebook
nbOf cs = Notebook{nbTitle = "t", nbCells = cs}

cells :: [Cell]
cells =
    [ mkCell 1 "-- compile: Model\nstep x = x + 1"
    , mkCell 2 "a = 1"
    , mkCell 3 "b = step 2"
    , mkCell 4 "c = a + 1"
    ]

fakeBackend :: IORef [Text] -> (Text -> (Text, Text)) -> IO ST.SessionBackend
fakeBackend transcript respond = do
    uid <- newUnique
    let record t = modifyIORef' transcript (++ [t]) >> pure (respond t)
        backend =
            ST.SessionBackend
                { ST.sbSessionId = uid
                , ST.sbJsonDiagnostics = False
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
                , ST.sbQueryBindings = pure ""
                , ST.sbQueryDoc = \_ -> pure ""
                , ST.sbQueryHoleFits = \_ -> pure ""
                , ST.sbEvalPureLive = \req -> pure (ST.pureEvalUnavailableResult req "fake backend")
                }
    pure backend

mkApp :: (Text -> (Text, Text)) -> IO (App, IORef [Text])
mkApp respond = do
    app <- newApp "." Set.empty Nothing Nothing []
    transcript <- newIORef []
    backend <- fakeBackend transcript respond
    modifyNotebook (appNotebook app) (\nb -> nb{nbCells = cells})
    nb <- readNotebook (appNotebook app)
    installHaskellSession
        (appSessions app)
        backend
        (neededEnvSig app (collectMetadata nb))
    pure (app, transcript)

seedLoaded :: App -> M.Map Text Text -> IO ()
seedLoaded app mods = do
    mSess <- getHaskellSession (appSessions app)
    case mSess of
        Nothing -> error "seedLoaded: no session"
        Just backend -> setLoadedModules app (ST.sbSessionId backend) mods

-- | Build the plan the way a real edit does, so the module state is detected.
planFor :: App -> Int -> IO ExecutionPlan
planFor app cid = do
    nb <- readNotebook (appNotebook app)
    (env, mods) <- planInputs app nb
    pure (computeExecutionPlanIn env mods cid cells nb)

alwaysOk :: Text -> (Text, Text)
alwaysOk _ = ("", "")

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
            let plan = computeExecutionPlan 1 cells (nbOf cells)
            map cellId (epCellsToRun plan) `shouldBe` [3]

        it "excludes compiled cells and skip-set cells" $ do
            let cs = cells ++ [mkCell 5 "a = 2"]
            map cellId (escalatedCellsToRun (nbOf cs))
                `shouldBe` [2, 3, 4]

    describe "a pending :load is a plan input, not an afterthought" $ do
        it "detects that the kernel's modules no longer match the notebook" $ do
            (app, _) <- mkApp alwaysOk
            seedLoaded app (M.singleton "Model" "old")
            nb <- readNotebook (appNotebook app)
            snd <$> planInputs app nb `shouldReturn` ModulesWiped

        it "sees nothing pending when the kernel already has these modules" $ do
            (app, _) <- mkApp alwaysOk
            nb0 <- readNotebook (appNotebook app)
            seedLoaded app (cpModulesOf (computeExecutionPlan 1 cells nb0))
            nb <- readNotebook (appNotebook app)
            snd <$> planInputs app nb `shouldReturn` ModulesLoaded

        it
            "runs every interpreted cell when a load is coming, because the\
            \ wipe takes their bindings with it"
            $ do
                (app, _) <- mkApp alwaysOk
                seedLoaded app (M.singleton "Model" "old")
                plan <- planFor app 1
                map cellId (epCellsToRun plan) `shouldBe` [2, 3, 4]

    describe "runPlanPhases after a compiled-cell edit" $ do
        it "a changed module reloads, then re-runs every interpreted cell" $ do
            (app, transcript) <- mkApp alwaysOk
            seedLoaded app (M.singleton "Model" "old")
            plan <- planFor app 1
            gen <- bumpGeneration app
            runPlanPhases app gen plan
            ts <- readIORef transcript
            let loads = hits ":load" ts
            length loads `shouldBe` 1
            let runs = [hits src ts | src <- ["a = 1", "b = step 2", "c = a + 1"]]
            map length runs `shouldBe` [1, 1, 1]
            [[ia], [ib], [ic]] <- pure runs
            ia `shouldSatisfy` (> head loads)
            ib `shouldSatisfy` (> ia)
            ic `shouldSatisfy` (> ib)

        it "an unchanged module neither reloads nor escalates" $ do
            (app, transcript) <- mkApp alwaysOk
            nb0 <- readNotebook (appNotebook app)
            seedLoaded app (cpModulesOf (computeExecutionPlan 1 cells nb0))
            plan <- planFor app 1
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
            seedLoaded app (M.singleton "Model" "old")
            plan <- planFor app 1
            gen <- bumpGeneration app
            runPlanPhases app gen plan
            ts <- readIORef transcript
            length (hits "a = 1" ts) `shouldBe` 1
            length (hits "c = a + 1" ts) `shouldBe` 1
            hits "b = step 2" ts `shouldBe` []
            nb' <- readNotebook (appNotebook app)
            Just blocked <- pure (lookup 3 [(cellId c, c) | c <- nbCells nb'])
            cellError blocked `shouldSatisfy` (/= Nothing)

cpModulesOf :: ExecutionPlan -> M.Map Text Text
cpModulesOf = cpModules . epCompilePlan
