{- | The two facts the pure planner needs from live session state: whether the
kernel is the one this notebook needs, and whether a @:load@ is about to wipe
the interpreted namespace.
-}
module Sabela.Handlers.Plan.Inputs (
    currentEnvState,
    currentModuleState,
    planInputs,
    isSessionUpToDate,
) where

import Sabela.Compiled (planCompiledModules)
import Sabela.Deps (collectMetadata)
import Sabela.Handlers.Compile (moduleReloadPending)
import Sabela.Handlers.Lifecycle (envStale, sessionMetaMatches)
import Sabela.Model (Notebook (..))
import Sabela.Reactivity (
    EnvState (..),
    ModuleState (..),
    cellPositionMap,
    haskellCodeCells,
 )
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..), loadedModules)
import Sabela.State.SessionManager (getHaskellSession)

currentEnvState :: App -> Notebook -> IO EnvState
currentEnvState app nb = do
    stale <- envStale app (collectMetadata nb)
    pure (if stale then EnvStale else EnvFresh)

{- | Is a @:load@ coming? Asked before planning, because it wipes every
interpreted binding and so invalidates those cells as surely as a new kernel
would — which the plan can then express as roots rather than a second pass.
-}
currentModuleState :: App -> Notebook -> IO ModuleState
currentModuleState app nb = do
    mSess <- getHaskellSession (appSessions app)
    case mSess of
        Nothing -> pure ModulesLoaded
        Just backend -> do
            loaded <- loadedModules app (ST.sbSessionId backend)
            let cplan = planCompiledModules (cellPositionMap nb) (haskellCodeCells nb)
            pure $
                if moduleReloadPending loaded cplan
                    then ModulesWiped
                    else ModulesLoaded

-- | Both inputs the planner needs from live session state.
planInputs :: App -> Notebook -> IO (EnvState, ModuleState)
planInputs app nb =
    (,) <$> currentEnvState app nb <*> currentModuleState app nb

isSessionUpToDate :: App -> Notebook -> IO Bool
isSessionUpToDate app nb = sessionMetaMatches app (collectMetadata nb)
