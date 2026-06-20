{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | The compile phase: write generated module files for compiled cells,
load them with @:add@ (GHC recompiles only what changed — see
'Sabela.Compiled' for the pure planning), route diagnostics back to cells,
and bring the modules into the prompt's scope. Runs before any interpreted
cell of the same plan.
-}
module Sabela.Handlers.Compile (
    CompileOutcome (..),
    runCompilePhase,
    compiledDependents,
) where

import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, void)
import Data.IORef (readIORef, writeIORef)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Clock (getMonotonicTime)
import System.Directory (createDirectoryIfMissing, removeFile)
import System.FilePath (takeDirectory, (</>))

import Sabela.Compiled (CompilePlan (..), moduleFilePath)
import Sabela.Errors (parseCompiledErrors)
import Sabela.Handlers.Lifecycle (handleKernelCrash)
import Sabela.Handlers.Shared
import Sabela.Model (
    Cell (..),
    CellError (..),
    MimeType (MimePlain),
    Notebook (..),
    NotebookEvent (..),
    OutputItem (..),
 )
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..), clearCompiledModules, withBuilding)
import Sabela.State.Environment (Environment (..))
import Sabela.State.SessionManager (getHaskellSession)
import qualified Sabela.Topo as Topo

{- | What the compile phase did to the session. Any @:load@ — successful or
not — resets GHCi's interactive context, wiping every prompt binding; the
caller must then escalate to re-running all interpreted cells.
-}
data CompileOutcome
    = -- | No usable session; nothing was loaded, nothing was wiped.
      CompileNoSession
    | -- | Module set already loaded; no reload, prompt context intact.
      CompileNoChange
    | -- | Reload succeeded; the prompt context was wiped.
      CompileReloaded
    | -- | Reload failed (or crashed); the prompt context was wiped.
      CompileFailed
    deriving (Eq, Show)

{- | Compile the generated modules for @affectedCells@ (the plan's
'Sabela.Reactivity.epCellsToRun' compiled counterpart). On 'CompileFailed'
the caller must skip interpreted dependents of compiled cells.
-}
runCompilePhase :: App -> Int -> CompilePlan -> [Cell] -> IO CompileOutcome
runCompilePhase app gen cplan affectedCells = do
    mSess <- getHaskellSession (appSessions app)
    case mSess of
        Nothing -> do
            forM_ affectedCells $ \c ->
                broadcastCellError app (cellId c) "No GHCi session"
            pure CompileNoSession
        Just backend -> do
            forM_ affectedCells $ \c ->
                broadcast app (EvCellCompiling (cellId c))
            loaded <- readIORef (appCompiledModules app)
            let changed = M.differenceWith keepChanged (cpModules cplan) loaded
                orphans = M.keysSet loaded `S.difference` M.keysSet (cpModules cplan)
            if M.null changed && S.null orphans
                then do
                    broadcastCompiled app gen affectedCells Nothing
                    pure CompileNoChange
                else compileChanged app gen backend cplan affectedCells changed orphans
  where
    keepChanged new old = if new == old then Nothing else Just new

compileChanged ::
    App ->
    Int ->
    ST.SessionBackend ->
    CompilePlan ->
    [Cell] ->
    M.Map Text Text ->
    S.Set Text ->
    IO CompileOutcome
compileChanged app gen backend cplan affectedCells changed orphans = withBuilding app $ do
    let projDir = envTmpDir (appEnv app) </> "repl-project"
    forM_ (M.toList changed) $ \(name, src) -> do
        let path = projDir </> moduleFilePath name
        createDirectoryIfMissing True (takeDirectory path)
        TIO.writeFile path src
    forM_ (S.toList orphans) $ \name ->
        removeQuiet (projDir </> moduleFilePath name)
    onLine <- mkCompileStreamCallback app affectedCells
    t0 <- getMonotonicTime
    -- :load (not :add) replaces the target set: the stub Main.hs never gets
    -- re-resolved (it would fail after the session's :cd), and modules of
    -- deleted/renamed cells drop out without explicit unloading.
    let loadCmd =
            T.unwords $
                ":load"
                    : [ T.pack (show (projDir </> moduleFilePath m))
                      | m <- M.keys (cpModules cplan)
                      ]
    result <- try (ST.sbRunBlockStreaming backend loadCmd onLine)
    t1 <- getMonotonicTime
    case result of
        Left (e :: SomeException) -> do
            handleKernelCrash app backend ("Kernel crashed: " <> T.pack (show e))
            clearCompiledModules app
            pure CompileFailed
        Right (rawOut, rawErr) -> do
            let (perCell, loose) = parseCompiledErrors rawErr
                failed = "Failed," `T.isInfixOf` rawOut || not (M.null perCell) || not (null loose)
            if failed
                then do
                    broadcastCompileErrors app gen affectedCells perCell loose
                    -- Forget loaded state so a retry rewrites and reloads.
                    clearCompiledModules app
                    pure CompileFailed
                else do
                    importModules backend (M.keys (cpModules cplan))
                    writeIORef (appCompiledModules app) (cpModules cplan)
                    broadcastCompiled app gen affectedCells (Just (t1 - t0))
                    pure CompileReloaded

-- | Bring the loaded modules into the prompt's scope.
importModules :: ST.SessionBackend -> [Text] -> IO ()
importModules backend current =
    unless (null current) $
        ignoring (ST.sbRunBlock backend (T.unlines ["import " <> m | m <- current]))
  where
    ignoring act = void (try act :: IO (Either SomeException (Text, Text)))

removeQuiet :: FilePath -> IO ()
removeQuiet p = void (try (removeFile p) :: IO (Either SomeException ()))

-- | GHC's progress lines stream into every affected cell's output area.
mkCompileStreamCallback :: App -> [Cell] -> IO (Text -> IO ())
mkCompileStreamCallback app cells =
    pure $ \line ->
        unless (T.null (T.strip line)) $
            forM_ cells $ \c ->
                broadcast app (EvCellPartialOutput (cellId c) line)

broadcastCompiled :: App -> Int -> [Cell] -> Maybe Double -> IO ()
broadcastCompiled app gen cells mElapsed = do
    let msg = case mElapsed of
            Just secs ->
                "compiled ✓ (native -O2, "
                    <> T.pack (show (fromIntegral (round (secs * 10) :: Int) / 10 :: Double))
                    <> "s)"
            Nothing -> "compiled ✓ (cached)"
        out = OutputItem MimePlain msg
    forM_ cells $ \c ->
        whenCurrentGen app gen $
            updateAndBroadcast
                app
                (\nb -> nb{nbCells = map (applyCompiled (cellId c) [out] Nothing) (nbCells nb)})
                (EvCellResult (cellId c) [out] Nothing [])

broadcastCompileErrors ::
    App -> Int -> [Cell] -> M.Map Int [CellError] -> [CellError] -> IO ()
broadcastCompileErrors app gen cells perCell loose = do
    let looseText = T.intercalate "\n" (map ceMessage loose)
    forM_ cells $ \c -> do
        let cid = cellId c
            errs = M.findWithDefault [] cid perCell
            msg = case errs of
                [] ->
                    if T.null looseText
                        then "compilation failed in another compiled cell"
                        else looseText
                _ -> T.intercalate "\n\n" (map ceMessage errs)
        whenCurrentGen app gen $
            updateAndBroadcast
                app
                (\nb -> nb{nbCells = map (applyCompiled cid [] (Just msg)) (nbCells nb)})
                (EvCellResult cid [] (Just msg) errs)

applyCompiled :: Int -> [OutputItem] -> Maybe Text -> Cell -> Cell
applyCompiled cid outs err c
    | cellId c == cid = c{cellOutputs = outs, cellError = err, cellDirty = False}
    | otherwise = c

{- | Interpreted cells transitively downstream of any compiled cell — the
ones to skip when the compile phase fails.
-}
compiledDependents :: CompilePlan -> M.Map Text Int -> [Cell] -> S.Set Int
compiledDependents cplan defMap allCode =
    let deps = Topo.buildDepGraph defMap allCode
        roots = M.keysSet (cpCellModule cplan)
     in Topo.reachableFrom roots (Topo.reverseDeps deps) `S.difference` roots
