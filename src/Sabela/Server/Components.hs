{-# LANGUAGE OverloadedStrings #-}

{- | Components over HTTP: list the workspace's, preview and accept an
extraction, bundle one for sharing, and install a bundle. Writing a module
changes the package every session overlays, so accept and install end in a
kernel restart, as deleting a cell does.
-}
module Sabela.Server.Components (
    listComponentsH,
    extractPreviewH,
    extractAcceptH,
    componentBundleH,
    installComponentH,
) where

import Control.Concurrent (forkIO)
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Servant (Handler, ServerError (..), err400, err404, throwError)

import Sabela.Api.Components
import Sabela.Components
import Sabela.Components.Extract (Extraction (..), extractModule)
import Sabela.Handlers.Plan (executeFullRestart)
import Sabela.Handlers.Shared (bumpGeneration)
import Sabela.Model (Cell (..), Notebook (..), lookupCell)
import Sabela.State (App (..), broadcastNotebook)
import Sabela.State.Environment (Environment (..))
import Sabela.State.NotebookStore (modifyNotebook, readNotebook)

workDirOf :: App -> FilePath
workDirOf = envWorkDir . appEnv

info :: Component -> ComponentInfo
info c =
    ComponentInfo
        { ciModule = cmModule c
        , ciPath = T.pack (cmPath c)
        , ciPurpose = cmPurpose c
        , ciKind = cmKind c
        , ciExports = cmExports c
        , ciDeps = cmDeps c
        }

listComponentsH :: App -> Handler [ComponentInfo]
listComponentsH app = liftIO (map info <$> listComponents (workDirOf app))

badRequest :: Text -> Handler a
badRequest msg = throwError err400{errBody = LBS.fromStrict (TE.encodeUtf8 msg)}

-- | The module a component would become, without writing anything.
extractPreviewH :: App -> ExtractRequest -> Handler ExtractPreview
extractPreviewH app req = do
    nb <- liftIO (readNotebook (appNotebook app))
    let modName = qualify (erName req)
        cells = mapMaybe (`lookupCell` nb) (erCellIds req)
    unless (validComponentModule modName) $
        badRequest
            "The name must be a capitalised identifier, like Retention or Charts.Revenue."
    when (null cells) $ badRequest "No cells to extract."
    let ex = extractModule modName (fromMaybe "" (erPurpose req)) cells
    pure
        ExtractPreview
            { epModule = modName
            , epPath = T.pack (componentsDirName <> "/" <> moduleToPath modName)
            , epSource = exSource ex
            , epExports = exExports ex
            , epWarnings = exWarnings ex
            , epReplacement = exReplacement ex
            }

qualify :: Text -> Text
qualify n
    | "Components." `T.isPrefixOf` n = T.strip n
    | otherwise = "Components." <> T.strip n

{- | Write the module, replace the first selected cell with the call site,
drop the others, and restart the kernel so the package is overlaid.
-}
extractAcceptH :: App -> ExtractAccept -> Handler Notebook
extractAcceptH app acc = do
    let modName = qualify (eaModule acc)
    unless (validComponentModule modName) $ badRequest "Invalid module name."
    case eaCellIds acc of
        [] -> badRequest "No cells to replace."
        (first : rest) -> liftIO $ do
            _ <- writeComponent (workDirOf app) modName (eaSource acc)
            modifyNotebook (appNotebook app) $ \nb ->
                nb
                    { nbCells =
                        [ if cellId c == first
                            then
                                c
                                    { cellSource = eaReplacement acc
                                    , cellOutputs = []
                                    , cellError = Nothing
                                    , cellDirty = True
                                    }
                            else c
                        | c <- nbCells nb
                        , cellId c `notElem` rest
                        ]
                    }
            restartAfterPackageChange app
            readNotebook (appNotebook app)

restartAfterPackageChange :: App -> IO ()
restartAfterPackageChange app = do
    broadcastNotebook app
    gen <- bumpGeneration app
    void $ forkIO $ executeFullRestart app gen

componentBundleH :: App -> Maybe Text -> Handler ComponentBundle
componentBundleH app mModule = do
    m <- maybe (badRequest "module is required") (pure . qualify) mModule
    mSrc <- liftIO (readComponentSource (workDirOf app) m)
    case mSrc of
        Nothing -> throwError err404
        Just src -> do
            let h = parseHeader src
            pure
                ComponentBundle
                    { cbModule = m
                    , cbPurpose = hdPurpose h
                    , cbKind = hdKind h
                    , cbExports = hdExports h
                    , cbDeps = hdDeps h
                    , cbSource = src
                    , cbVersion = 1
                    }

-- | Put a bundle's module into this workspace's package.
installComponentH :: App -> InstallRequest -> Handler ComponentInfo
installComponentH app (InstallRequest b overwrite) = do
    let m = qualify (cbModule b)
    unless (validComponentModule m) $ badRequest "Invalid module name in bundle."
    existing <- liftIO (readComponentSource (workDirOf app) m)
    case existing of
        Just _
            | not overwrite ->
                badRequest
                    ( "A component named "
                        <> m
                        <> " already exists here; pass irOverwrite to replace it."
                    )
        _ -> liftIO $ do
            path <- writeComponent (workDirOf app) m (cbSource b)
            restartAfterPackageChange app
            let h = parseHeader (cbSource b)
            pure
                ComponentInfo
                    { ciModule = m
                    , ciPath = T.pack path
                    , ciPurpose = hdPurpose h
                    , ciKind = hdKind h
                    , ciExports = hdExports h
                    , ciDeps = hdDeps h
                    }
