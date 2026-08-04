{-# LANGUAGE OverloadedStrings #-}

{- | Putting the notebook back into a fresh session: writing out the compiled
modules, then re-running each replayable cell with the widget and bridge values
the snapshot captured.
-}
module Sabela.Session.Materialize.Replay (
    RenderContext (..),
    snapshotRenderContext,
    renderCell,
    replayCells,
    applyBridgeExports,
    loadCompiled,
) where

import Control.Monad (forM_, void)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))

import Sabela.Bridge (bridgePreamble, widgetPreamble)
import Sabela.Compiled (CompilePlan (..), moduleFilePath)
import Sabela.Handlers.Shared (partitionExports)
import Sabela.Model (Cell (..))
import Sabela.Output (displayPrelude, parseMimeOutputs)
import Sabela.Session.Materialize.Run (
    runChecked,
    runLoadChecked,
 )
import Sabela.Session.MaterializeSnapshot (MaterializeSnapshot (..))
import Sabela.Session.MaterializeStage (MaterializeStage (..))
import qualified Sabela.SessionTypes as ST
import ScriptHs.Parser (ScriptFile (..), parseScriptNumbered)
import ScriptHs.Render (toGhciScript)

data RenderContext = RenderContext
    { rcBridgeValues :: M.Map Text Text
    , rcWidgetValues :: M.Map Int (M.Map Text Text)
    }

snapshotRenderContext :: MaterializeSnapshot -> RenderContext
snapshotRenderContext snapshot =
    RenderContext
        { rcBridgeValues = msBridgeValues snapshot
        , rcWidgetValues = msWidgetValues snapshot
        }

renderCell :: RenderContext -> M.Map Text Text -> Cell -> Text
renderCell context bridge cell =
    widgetPreamble
        (cellId cell)
        (M.findWithDefault M.empty (cellId cell) (rcWidgetValues context))
        <> bridgePreamble bridge
        <> toGhciScript (scriptLines (fst (parseScriptNumbered (cellSource cell))))

replayCells ::
    ST.SessionBackend ->
    RenderContext ->
    [Cell] ->
    IO
        ( Either
            ([Int], Maybe Int, MaterializeStage, Text)
            ([Int], M.Map Text Text)
        )
replayCells backend context = go True [] (rcBridgeValues context)
  where
    go _ done bridge [] = pure (Right (reverse done, bridge))
    go preludeReady done bridge (cell : rest) = do
        preludeResult <-
            if preludeReady
                then pure (Right ("", ""))
                else runChecked backend displayPrelude
        case preludeResult of
            Left msg ->
                pure (Left (reverse done, Nothing, StagePrelude, msg))
            Right _ -> do
                result <- runChecked backend (renderCell context bridge cell)
                case result of
                    Left msg ->
                        pure
                            ( Left
                                (reverse done, Just (cellId cell), StageCellReplay, msg)
                            )
                    Right (out, _) ->
                        go
                            False
                            (cellId cell : done)
                            (applyBridgeExports bridge out)
                            rest

applyBridgeExports :: M.Map Text Text -> Text -> M.Map Text Text
applyBridgeExports bridge rawOut =
    M.union
        (M.fromList [(name, T.strip value) | (name, value) <- exports])
        bridge
  where
    (exports, _) = partitionExports (parseMimeOutputs rawOut)

loadCompiled ::
    FilePath ->
    ST.SessionBackend ->
    CompilePlan ->
    IO (Either Text ())
loadCompiled projectDir backend cplan
    | M.null (cpModules cplan) = pure (Right ())
    | otherwise = do
        forM_ (M.toList (cpModules cplan)) $ \(name, source) -> do
            let path = projectDir </> moduleFilePath name
            createDirectoryIfMissing True (takeDirectory path)
            TIO.writeFile path source
        let moduleNames = M.keys (cpModules cplan)
            loadCommand =
                T.unwords
                    ( ":load"
                        : [T.pack (show (projectDir </> moduleFilePath name)) | name <- moduleNames]
                    )
        loaded <- runLoadChecked backend loadCommand
        case loaded of
            Left msg -> pure (Left msg)
            Right _ -> do
                imported <-
                    runChecked
                        backend
                        (T.unlines ["import " <> name | name <- moduleNames])
                pure (void imported)
