{-# LANGUAGE OverloadedStrings #-}

{- | The sticky-door regression: a kernel still holding compiled modules for a
notebook that no longer has any compiled cells reported ModulesWiped forever,
because the reconciling unload only ran when the plan carried compile cells.
-}
module Test.OrphanModuleReconcileSpec (spec) where

import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Unique (newUnique)

import Sabela.Deps (collectMetadata)
import Sabela.Handlers.Lifecycle (neededEnvSig)
import Sabela.Handlers.Plan (planInputs, runPlanPhases)
import Sabela.Handlers.Shared (bumpGeneration)
import Sabela.Model (Cell (..), Notebook (..))
import Sabela.Reactivity (
    ExecutionPlan (..),
    ModuleState (..),
    computeExecutionPlanIn,
 )
import Sabela.Server (newApp)
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..), setLoadedModules)
import Sabela.State.NotebookStore (modifyNotebook, readNotebook)
import Sabela.State.SessionManager (getHaskellSession, installHaskellSession)
import Test.CellFixture (mkCell)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.WriteAckFixture (inertBackend)

-- | An interpreted-only notebook: no cell carries a compile directive.
cells :: [Cell]
cells = [mkCell 1 "a = 1", mkCell 2 "b = a + 1", mkCell 3 "c = 99"]

mkApp :: IO (App, IORef [Text])
mkApp = do
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
    pure (app, transcript)

seedLoaded :: App -> M.Map Text Text -> IO ()
seedLoaded app mods = do
    mSess <- getHaskellSession (appSessions app)
    case mSess of
        Nothing -> error "seedLoaded: no session"
        Just backend -> setLoadedModules app (ST.sbSessionId backend) mods

planFor :: App -> Int -> IO ExecutionPlan
planFor app cid = do
    nb <- readNotebook (appNotebook app)
    (env, mods) <- planInputs app nb
    pure (computeExecutionPlanIn env mods cid cells nb)

runPlan :: App -> ExecutionPlan -> IO ()
runPlan app plan = do
    gen <- bumpGeneration app
    runPlanPhases app gen plan

moduleState :: App -> IO ModuleState
moduleState app = do
    nb <- readNotebook (appNotebook app)
    snd <$> planInputs app nb

loads :: [Text] -> [Text]
loads = filter (":load" `T.isInfixOf`)

spec :: Spec
spec = describe "orphaned compiled modules reconcile instead of sticking" $ do
    it "running the plan clears the pending wipe" $ do
        (app, _) <- mkApp
        seedLoaded app (M.singleton "Model" "old")
        moduleState app >>= (`shouldBe` ModulesWiped)
        planFor app 1 >>= runPlan app
        moduleState app >>= (`shouldBe` ModulesLoaded)

    it "the reconciling unload issues exactly one bare :load" $ do
        (app, transcript) <- mkApp
        seedLoaded app (M.singleton "Model" "old")
        planFor app 1 >>= runPlan app
        ts <- readIORef transcript
        length (loads ts) `shouldBe` 1

    it "the edit after reconciliation is incremental again" $ do
        (app, transcript) <- mkApp
        seedLoaded app (M.singleton "Model" "old")
        planFor app 1 >>= runPlan app
        plan2 <- planFor app 1
        map cellId (epCellsToRun plan2) `shouldBe` [1, 2]
        runPlan app plan2
        ts <- readIORef transcript
        length (loads ts) `shouldBe` 1

    it "a kernel holding nothing never issues a :load at all" $ do
        (app, transcript) <- mkApp
        planFor app 1 >>= runPlan app
        ts <- readIORef transcript
        loads ts `shouldBe` []
