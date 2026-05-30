{-# LANGUAGE OverloadedStrings #-}

{- | Export endpoints: serve a notebook as @.html@ (dashboard /
slideshow / static notebook), @.md@, @.hs@ (cabal script), @.lhs@, or
@reactive.hs@. The HTML shells (@dashboardHtml@ / @slideshowHtml@) come
from "Sabela.Server.Static"; here we wrap them with content-type and
Content-Disposition headers and produce file-download responses.
-}
module Sabela.Server.Export (
    exportDashboardApp,
    exportSlideshowApp,
    exportNotebookApp,
    exportMarkdownApp,
    exportHaskellApp,
    exportLhsApp,
    exportReactiveApp,

    -- * Pieces (exposed for testing / reuse)
    cellParam,
    lastHaskellCellId,
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (HeaderName, hContentType, status200)
import Network.Wai (Application, Request, queryString, responseLBS)
import Text.Read (readMaybe)

import Sabela.Dashboard (renderStaticDashboard, renderStaticNotebook)
import qualified Sabela.Export as Export
import qualified Sabela.Export.Reactive as Reactive
import Sabela.Model
import Sabela.Server.Notebook (cellsToSegments)
import Sabela.Server.Static (dashboardHtml, slideshowHtml)
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..))
import Sabela.State.NotebookStore (readNotebook)
import ScriptHs.Markdown (reassemble)

exportDashboardApp :: App -> Application
exportDashboardApp app _req resp = do
    nb <- readNotebook (appNotebook app)
    let body = renderStaticDashboard dashboardHtml nb
        title = nbTitle nb
        filename = T.takeWhileEnd (/= '/') (T.dropWhileEnd (== '/') title)
        htmlName =
            if T.null filename
                then "dashboard.html"
                else T.replace ".md" ".html" filename
    resp $
        responseLBS
            status200
            (downloadHeaders "text/html; charset=utf-8" htmlName)
            body

exportSlideshowApp :: App -> Application
exportSlideshowApp app _req resp = do
    nb <- readNotebook (appNotebook app)
    let body = renderStaticDashboard slideshowHtml nb
        title = nbTitle nb
        filename = T.takeWhileEnd (/= '/') (T.dropWhileEnd (== '/') title)
        htmlName =
            if T.null filename
                then "slideshow.html"
                else T.replace ".md" ".slides.html" filename
    resp $
        responseLBS
            status200
            (downloadHeaders "text/html; charset=utf-8" htmlName)
            body

{- | Export the notebook as a self-contained, read-only "notebook" (tutorial)
page — prose + code + rendered outputs, in document order — with the canonical
markdown embedded for an in-page download. Served as a viewable page (no
attachment); the hub stores this body and serves it at @\/s\/<slug>@.
-}
exportNotebookApp :: App -> Application
exportNotebookApp app _req resp = do
    nb <- readNotebook (appNotebook app)
    let md = reassemble (cellsToSegments (nbCells nb))
        body = renderStaticNotebook dashboardHtml nb md
    resp $
        responseLBS
            status200
            [(hContentType, "text/html; charset=utf-8")]
            body

exportMarkdownApp :: App -> Application
exportMarkdownApp app _req resp = do
    nb <- readNotebook (appNotebook app)
    let md = reassemble (cellsToSegments (nbCells nb))
        title = nbTitle nb
        filename = T.takeWhileEnd (/= '/') (T.dropWhileEnd (== '/') title)
        mdName =
            if T.null filename
                then "notebook.md"
                else filename
    resp $
        responseLBS
            status200
            (downloadHeaders "text/markdown; charset=utf-8" mdName)
            (LBS.fromStrict (TE.encodeUtf8 md))

-- | Export the pipeline ending at @?cell=<id>@ as a single-file cabal script.
exportHaskellApp :: App -> Application
exportHaskellApp = exportSourceApp Export.exportCabalScript "hs" "text/x-haskell"

-- | Export the pipeline ending at @?cell=<id>@ as literate Haskell.
exportLhsApp :: App -> Application
exportLhsApp = exportSourceApp Export.exportLiterate "lhs" "text/x-literate-haskell"

-- | Export the whole notebook as a headless reactive-banana program.
exportReactiveApp :: App -> Application
exportReactiveApp = exportSourceApp Reactive.exportReactive "reactive.hs" "text/x-haskell"

{- | Shared download handler for the source exporters. Reads an optional
@?cell=<id>@ slice target (defaulting to the last Haskell code cell), renders,
and serves the result as a file download named after the notebook.
-}
exportSourceApp ::
    (App -> Int -> IO Text) -> Text -> BS.ByteString -> App -> Application
exportSourceApp render ext mime app req resp = do
    nb <- readNotebook (appNotebook app)
    let target = fromMaybe (lastHaskellCellId nb) (cellParam req)
    src <- render app target
    let title = nbTitle nb
        base = T.takeWhileEnd (/= '/') (T.dropWhileEnd (== '/') title)
        stem = if T.null base then "notebook" else T.replace ".md" "" base
        fname = stem <> "." <> ext
    resp $
        responseLBS
            status200
            (downloadHeaders (mime <> "; charset=utf-8") fname)
            (LBS.fromStrict (TE.encodeUtf8 src))

downloadHeaders :: BS.ByteString -> Text -> [(HeaderName, BS.ByteString)]
downloadHeaders contentType filename =
    [ (hContentType, contentType)
    ,
        ( "Content-Disposition"
        , "attachment; filename=\"" <> TE.encodeUtf8 filename <> "\""
        )
    ]

-- | Parse @?cell=<id>@ from the request query string.
cellParam :: Request -> Maybe Int
cellParam req = do
    mv <- lookup "cell" (queryString req)
    bs <- mv
    readMaybe (T.unpack (TE.decodeUtf8 bs))

-- | The last Haskell code cell's id (the default slice target), or -1 if none.
lastHaskellCellId :: Notebook -> Int
lastHaskellCellId nb =
    case [cellId c | c <- nbCells nb, cellType c == CodeCell, cellLang c == ST.Haskell] of
        [] -> -1
        ids -> last ids
