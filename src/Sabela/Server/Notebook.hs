{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Notebook GET/load/save handlers plus the markdown<->cell conversion
helpers (segment ↔ cell, MIME indicator round-tripping, prose-cell
boundary marker) and per-cell mutation handlers (update, save source,
insert, delete) that everything else routes through.
-}
module Sabela.Server.Notebook (
    -- * GET/load/save
    getNotebookH,
    loadNotebookH,
    saveNotebookH,
    safeWorkPath,

    -- * Per-cell mutation handlers
    updateCellH,
    saveCellSourceH,
    insertCellH,
    deleteCellH,

    -- * Cell ↔ segment conversion (exposed for testing)
    segmentToCell,
    cellToSegment,
    cellsToSegments,
    splitProseSegments,
    parseLang,
    parseCodeOutputItems,
    mimeIndicator,
    textToMime,
    proseMarker,
    proseSep,
    serializeOutputs,
    langTag,
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (modifyMVar)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Servant (Handler)
import System.Directory (canonicalizePath, createDirectoryIfMissing)
import System.FilePath (normalise, splitDirectories, takeDirectory, takeFileName, (</>))

import ScriptHs.Markdown (
    CodeOutput (..),
    Segment (..),
    parseMarkdown,
    reassemble,
 )
import Sabela.Api
import Sabela.Handlers (
    ReactiveNotebook (..),
    insertCellAt,
    reloadHaskellSession,
    updateCellSource,
 )
import Sabela.Handlers.Shared (bumpGeneration)
import Sabela.Model
import Sabela.Output (parseMimeOutputs)
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..), broadcastNotebook)
import Sabela.State.Environment (Environment (..))
import Sabela.State.NotebookStore (
    NotebookStore (..),
    freshCellId,
    modifyNotebook,
    readNotebook,
 )

getNotebookH :: App -> Handler Notebook
getNotebookH app = liftIO $ readNotebook (appNotebook app)

loadNotebookH :: App -> ReactiveNotebook -> LoadRequest -> Handler Notebook
loadNotebookH app _rn (LoadRequest path) = liftIO $ do
    mAbs <- safeWorkPath (appEnv app) path
    case mAbs of
        Nothing -> do
            putStrLn $ "[sabela] load rejected (path outside workspace): " ++ path
            readNotebook (appNotebook app)
        Just absPath -> do
            raw <- TIO.readFile absPath
            cells <-
                mapM
                    (segmentToCell (appNotebook app))
                    (splitProseSegments (parseMarkdown raw))
            let nb = Notebook (T.pack path) cells
            -- Cancel any in-flight execution and reclaim GHCi memory via :reload
            void $ bumpGeneration app
            void $ forkIO $ reloadHaskellSession app
            modifyNotebook (appNotebook app) (const nb)
            broadcastNotebook app
            pure nb

{- | Resolve a notebook path against the work directory, canonicalised and
confined: returns 'Nothing' for any input that resolves outside @envWorkDir@
(absolute paths to system files, @../@ traversal). The check is identical
in shape to 'Sabela.Server.Files.isPrefixOfPath', applied to the file's
parent so we don't require the target to exist yet.
-}
safeWorkPath :: Environment -> FilePath -> IO (Maybe FilePath)
safeWorkPath env path = do
    let workDir = envWorkDir env
        candidate = workDir </> path
    workCanon <- canonicalizePath workDir
    parentCanon <- canonicalizePath (takeDirectory candidate)
    if isPrefixOfPath workCanon parentCanon
        then pure (Just (parentCanon </> takeFileName candidate))
        else pure Nothing

-- | Path-component-aware @isPrefixOf@; avoids @/home/alice@ matching @/home/alice-secret@.
isPrefixOfPath :: FilePath -> FilePath -> Bool
isPrefixOfPath prefix p =
    let pp = splitDirectories (normalise prefix)
        pq = splitDirectories (normalise p)
     in pp `isPrefixOf` pq

segmentToCell :: NotebookStore -> Segment -> IO Cell
segmentToCell store (Prose t) = do
    nid <- freshCellId store
    pure (Cell nid ProseCell ST.Haskell t [] Nothing False)
segmentToCell store (CodeBlock lang code Nothing) = do
    nid <- freshCellId store
    pure (Cell nid CodeCell (parseLang lang) code [] Nothing False)
segmentToCell store (CodeBlock lang code (Just (CodeOutput m o))) = do
    nid <- freshCellId store
    let items = parseCodeOutputItems m o
    pure (Cell nid CodeCell (parseLang lang) code items Nothing False)

parseCodeOutputItems :: MimeType -> Text -> [OutputItem]
parseCodeOutputItems MimePlain o =
    [ OutputItem (textToMime mt) b
    | (mt, b) <- parseMimeOutputs o
    , not (T.null (T.strip b))
    ]
parseCodeOutputItems m o = [OutputItem m o]

parseLang :: Text -> ST.CellLang
parseLang lang
    | lang `elem` ["python", "python3", "py"] = ST.Python
    | otherwise = ST.Haskell

saveNotebookH :: App -> SaveRequest -> Handler Notebook
saveNotebookH app (SaveRequest mPath) = liftIO $ do
    nb <- readNotebook (appNotebook app)
    let path = fromMaybe (T.unpack (nbTitle nb)) mPath
    mAbs <- safeWorkPath (appEnv app) path
    case mAbs of
        Nothing -> do
            putStrLn $ "[sabela] save rejected (path outside workspace): " ++ path
            pure nb
        Just absPath -> do
            let md = reassemble (cellsToSegments (nbCells nb))
            createDirectoryIfMissing True (takeDirectory absPath)
            TIO.writeFile absPath md
            let nb' = nb{nbTitle = T.pack path}
            modifyNotebook (appNotebook app) (const nb')
            putStrLn $ "[sabela] Saved to: " ++ absPath
            pure nb'

cellToSegment :: Cell -> Segment
cellToSegment c = case cellType c of
    ProseCell -> Prose (cellSource c)
    CodeCell -> codeToSegment c

{- | Markdown has no delimiter between adjacent prose blocks, so two
consecutive prose cells would otherwise merge on reload. We separate them
with an invisible HTML-comment marker (consistent with the existing
@\<!-- sabela:mime ... --\>@ convention). The marker never appears in any
cell's source — it is added on save and stripped on load.
-}
proseMarker :: Text
proseMarker = "<!-- sabela:cell -->"

proseSep :: Text
proseSep = "\n\n" <> proseMarker <> "\n\n"

{- | Collapse each maximal run of consecutive prose cells into a single
'Prose' segment, joining the sources with 'proseSep'. Non-prose cells are
mapped exactly as before. Inverse of 'splitProseSegments'.
-}
cellsToSegments :: [Cell] -> [Segment]
cellsToSegments [] = []
cellsToSegments (c : cs)
    | cellType c == ProseCell =
        let (run, rest) = span ((== ProseCell) . cellType) (c : cs)
         in Prose (T.intercalate proseSep (map cellSource run))
                : cellsToSegments rest
    | otherwise = cellToSegment c : cellsToSegments cs

{- | Invert 'cellsToSegments' on load: split any 'Prose' segment that
contains boundary-marker lines back into one segment per cell.
-}
splitProseSegments :: [Segment] -> [Segment]
splitProseSegments = concatMap expand
  where
    expand (Prose t)
        | any isMarker (T.lines t) =
            map (Prose . T.strip . T.unlines) (chunk (T.lines t))
        | otherwise = [Prose t]
    expand other = [other]
    isMarker l = T.strip l == proseMarker
    chunk ls = case break isMarker ls of
        (grp, []) -> [grp]
        (grp, _ : rest) -> grp : chunk rest

codeToSegment :: Cell -> Segment
codeToSegment c =
    let tag = langTag (cellLang c)
     in case filter (not . T.null . T.strip . oiOutput) (cellOutputs c) of
            [] -> CodeBlock tag (cellSource c) Nothing
            [OutputItem mime o] ->
                CodeBlock tag (cellSource c) (Just (CodeOutput mime o))
            items ->
                CodeBlock
                    tag
                    (cellSource c)
                    (Just (CodeOutput MimePlain (serializeOutputs items)))

langTag :: ST.CellLang -> Text
langTag ST.Haskell = "haskell"
langTag ST.Python = "python"

serializeOutputs :: [OutputItem] -> Text
serializeOutputs items =
    T.concat
        [ "<!-- MIME:" <> mimeIndicator mime <> " -->\n" <> o
        | OutputItem mime o <- items
        ]

updateCellH ::
    App -> ReactiveNotebook -> Int -> Maybe Text -> UpdateCell -> Handler Cell
updateCellH app rn cid mSession (UpdateCell src) = liftIO $ do
    rnCellEdit rn cid src
    nb <- readNotebook (appNotebook app)
    -- External callers (siza, curl) mark themselves with X-Sabela-Session
    -- so the browser refreshes its editor. Browser keystrokes omit the
    -- header and thus don't echo back as SSE noise.
    for_ mSession (const (broadcastNotebook app))
    case lookupCell cid nb of
        Just c -> pure c
        Nothing -> pure (Cell cid CodeCell ST.Haskell src [] Nothing True)

{- | Save cell source without triggering reactive execution.
Used by runAll to sync editor content before running.
-}
saveCellSourceH :: App -> Int -> Maybe Text -> UpdateCell -> Handler Cell
saveCellSourceH app cid mSession (UpdateCell src) = liftIO $ do
    modifyNotebook (appNotebook app) $ updateCellSource cid src
    nb <- readNotebook (appNotebook app)
    for_ mSession (const (broadcastNotebook app))
    case lookupCell cid nb of
        Just c -> pure c
        Nothing -> pure (Cell cid CodeCell ST.Haskell src [] Nothing True)

insertCellH :: App -> InsertCell -> Handler Cell
insertCellH app (InsertCell at typ lang src) = liftIO $ do
    nid <- freshCellId (appNotebook app)
    let cell = Cell nid typ lang src [] Nothing True
    modifyNotebook (appNotebook app) $ \nb ->
        nb{nbCells = insertCellAt at cell (nbCells nb)}
    broadcastNotebook app
    pure cell

deleteCellH :: App -> Int -> Handler Notebook
deleteCellH app cid = liftIO $ do
    nb' <- modifyMVar (nsNotebook (appNotebook app)) $ \nb -> do
        let !nb'' = nb{nbCells = filter (\c -> cellId c /= cid) (nbCells nb)}
        pure (nb'', nb'')
    broadcastNotebook app
    pure nb'
