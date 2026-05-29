{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Export a notebook's dependency slice to a runnable, standalone Haskell
artifact: a single-file cabal script (@.hs@) or literate Haskell (@.lhs@).

This module is the orchestrator: backward slice (from "Sabela.Export.Analyze"),
GHCi→module transform (from "Sabela.Export.Block"), widget freezing (from
"Sabela.Export.Widget"), type-aware trailing-expression resolution (from
"Sabela.Export.Trailing"), the stand-in prelude for the GHCi-injected display
API, and the prose/output comment framing — assembled into one runnable file.
-}
module Sabela.Export (
    exportCabalScript,
    exportLiterate,

    -- * Shared with the reactive exporter
    WidgetBind (..),
    parseWidgetBind,
    widgetDefault,
    exportPreludeDecls,
    mkTrailingResolver,

    -- * Pieces (exposed for testing)
    freezeWidgetSource,
    proseComment,
    splitArgs,
) where

import Data.Containers.ListUtils (nubOrd)
import Data.List (intercalate)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.Deps (collectMetadata, mergedMeta)
import Sabela.Export.Analyze (backwardSlice, buildNotebookGraph)
import Sabela.Export.Block (Hoisted (..), programActionExprs, splitProgram)
import Sabela.Export.Trailing (mkTrailingResolver)
import Sabela.Export.Widget (
    WidgetBind (..),
    freezeWidgetSource,
    parseWidgetBind,
    splitArgs,
    widgetDefault,
 )
import Sabela.Model (Cell (..), CellType (..), Notebook (..), OutputItem (..))
import Sabela.Reactivity (cellPositionMap, haskellCodeCells)
import qualified Sabela.SessionTypes as ST
import Sabela.State (App (..))
import Sabela.State.Environment (Environment (..))
import Sabela.State.NotebookStore (readNotebook)
import Sabela.State.SessionManager (getHaskellSession)
import Sabela.State.WidgetStore (getWidgetValues)
import ScriptHs.Parser (CabalMeta (..))
import ScriptHs.Render (
    LhsBlock (..),
    TrailKind (..),
    renderCabalScriptHeader,
    renderLiterate,
 )

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Export the pipeline ending at @target@ as a single-file cabal script.
exportCabalScript :: App -> Int -> IO Text
exportCabalScript app target = do
    ex <- buildExport app target
    pure $
        assembleSections
            [ T.stripEnd (renderCabalScriptHeader (exMeta ex))
            , proseComment (exProse ex)
            , warningsComment (exWarnings ex)
            , T.stripEnd (exModule ex)
            , outputsComment (exOutputs ex)
            ]

-- | Export the pipeline ending at @target@ as literate Haskell.
exportLiterate :: App -> Int -> IO Text
exportLiterate app target = do
    ex <- buildExport app target
    let intro = T.intercalate "\n\n" (filter (not . T.null) (literateNote : exProse ex))
        blocks =
            [ LhsProse intro
            , LhsCode (T.lines (T.stripEnd (renderCabalScriptHeader (exMeta ex))))
            ]
                ++ [LhsProse (T.intercalate "\n" (exWarnings ex)) | not (null (exWarnings ex))]
                ++ [LhsCode (T.lines (T.stripEnd (exModule ex)))]
                ++ [ LhsProse ("**Recorded outputs:**\n\n" <> exOutputs ex)
                   | not (T.null (T.strip (exOutputs ex)))
                   ]
    pure (renderLiterate blocks <> "\n")
  where
    literateNote = "Exported from Sabela as a literate-Haskell pipeline."

-- ---------------------------------------------------------------------------
-- Building the export
-- ---------------------------------------------------------------------------

data Export = Export
    { exMeta :: CabalMeta
    , exModule :: Text
    , exProse :: [Text]
    , exOutputs :: Text
    , exWarnings :: [Text]
    }

buildExport :: App -> Int -> IO Export
buildExport app target = do
    nb <- readNotebook (appNotebook app)
    msession <- getHaskellSession (appSessions app)
    let ng = buildNotebookGraph nb
        allCode = haskellCodeCells nb
        sliceIds = S.fromList (map cellId (backwardSlice target ng))
        -- TH splices generate names cellNames can't see, so the dependency
        -- graph can't slice on them — always keep any cell with a splice.
        slice =
            filter
                (\c -> cellId c `S.member` sliceIds || "$(" `T.isInfixOf` cellSource c)
                allCode
        posMap = cellPositionMap nb
    frozen <- mapM (freezeCell app) slice
    resolver <- mkTrailingResolver msession (programActionExprs frozen)
    let (hoisted, doStmts) = splitProgram resolver S.empty frozen
        -- Imports/pragmas come from the *whole* notebook so a slice never
        -- misses an import authored in an excluded cell (imports create no
        -- dependency edges). Over-inclusion only yields unused-import warnings,
        -- which the cabal header suppresses.
        (importsH, _) = splitProgram (const TrailUnknown) S.empty (map cellSource allCode)
    pure
        Export
            { exMeta = mergedMeta (envGlobalDeps (appEnv app)) (collectMetadata nb)
            , exModule = assembleModule importsH hoisted (exportPreludeDecls slice) doStmts
            , exProse = sliceProse nb posMap slice
            , exOutputs = outputsText posMap slice
            , exWarnings = buildWarnings nb slice hoisted doStmts
            }

freezeCell :: App -> Cell -> IO Text
freezeCell app c = do
    vals <- getWidgetValues (appWidgets app) (cellId c)
    pure (freezeWidgetSource vals (cellSource c))

{- | Assemble the module: hoisted pragmas, @module Main where@, notebook-wide
imports, the display stand-in prelude, top-level declarations (data/class/
instance/TH), and a generated @main@ holding every statement in document order
— so value bindings see the @\<-@ binds they depend on.
-}
assembleModule :: Hoisted -> Hoisted -> [Text] -> [Text] -> Text
assembleModule importsH hoisted prelude doStmts =
    T.unlines . intercalate [""] . filter (not . null) $
        [ dedupT (hPragmas importsH)
        , ["module Main where"]
        , dedupT (hImports importsH)
        , prelude
        , hTopDecls hoisted
        , mainBlock
        ]
  where
    mainBlock = case doStmts of
        [] -> ["main :: IO ()", "main = pure ()"]
        _ ->
            "main :: IO ()"
                : "main = do"
                : concatMap (map ("    " <>) . T.lines) (doStmts ++ ["pure ()"])

buildWarnings :: Notebook -> [Cell] -> Hoisted -> [Text] -> [Text]
buildWarnings nb slice hoisted doStmts =
    concat
        [ [ "-- [sabela:export] "
                <> tShow npy
                <> " Python cell(s) omitted (cross-language pipeline not supported)."
          | npy > 0
          ]
        , [ "-- [sabela:export] target is not a Haskell code cell; nothing to export."
          | null slice
          ]
        , [thNote | hasTH && hasBind]
        ]
  where
    npy = length [c | c <- nbCells nb, cellType c == CodeCell, cellLang c == ST.Python]
    hasTH = any (T.isInfixOf "$(") (hTopDecls hoisted)
    hasBind = any (T.isInfixOf "<-") doStmts
    thNote =
        T.intercalate
            "\n"
            [ "-- [sabela:export] NOTE: uses Template Haskell splices that may depend on"
            , "-- values bound at runtime (e.g. `df <- ...`). Top-level splices cannot see"
            , "-- main-local bindings, so this may need manual lifting to compile."
            ]

-- | Order-preserving deduplication. O(n log n) via 'Data.Set'.
dedupT :: [Text] -> [Text]
dedupT = nubOrd

-- ---------------------------------------------------------------------------
-- Stand-in prelude for the GHCi-injected display API
-- ---------------------------------------------------------------------------

{- | Definitions for the @display*@ functions the notebook relies on (injected
into GHCi by 'Sabela.Output.displayPrelude', absent from a standalone module).
Only the ones referenced by the slice are emitted, so there are no unused
bindings. Each prints to stdout — the closest standalone behaviour.
-}
exportPreludeDecls :: [Cell] -> [Text]
exportPreludeDecls slice =
    let src = T.concat (map cellSource slice)
        wanted = [def | (fn, def) <- preludeDefs, fn `T.isInfixOf` src]
     in [preludeHeader <> T.intercalate "\n\n" wanted | not (null wanted)]
  where
    preludeHeader = "-- [sabela:export] standalone stand-ins for the notebook display API\n"
    preludeDefs =
        [ ("displayHtml", sig "displayHtml" <> "displayHtml = putStrLn")
        , ("displayMarkdown", sig "displayMarkdown" <> "displayMarkdown = putStrLn")
        , ("displaySvg", sig "displaySvg" <> "displaySvg = putStrLn")
        , ("displayLatex", sig "displayLatex" <> "displayLatex = putStrLn")
        , ("displayJson", sig "displayJson" <> "displayJson = putStrLn")
        ,
            ( "displayImage"
            , "displayImage :: String -> String -> IO ()\ndisplayImage _ b64 = putStrLn b64"
            )
        ]
    sig n = n <> " :: String -> IO ()\n"

-- ---------------------------------------------------------------------------
-- Prose, outputs, and comment helpers
-- ---------------------------------------------------------------------------

-- | Non-empty prose cells positioned at or before the last slice cell.
sliceProse :: Notebook -> M.Map Int Int -> [Cell] -> [Text]
sliceProse nb posMap slice =
    let maxPos = maximum (1 : mapMaybe (\c -> M.lookup (cellId c) posMap) slice)
     in [ T.strip (cellSource c)
        | c <- nbCells nb
        , cellType c == ProseCell
        , not (T.null (T.strip (cellSource c)))
        , maybe False (<= maxPos) (M.lookup (cellId c) posMap)
        ]

-- | Recorded outputs of the slice cells, keyed by 1-based position.
outputsText :: M.Map Int Int -> [Cell] -> Text
outputsText posMap slice =
    T.intercalate "\n\n" $
        [ "cell "
            <> tShow (M.findWithDefault (cellId c) (cellId c) posMap)
            <> ":\n"
            <> body
        | c <- slice
        , let body =
                T.intercalate
                    "\n"
                    [ T.stripEnd (oiOutput o)
                    | o <- cellOutputs c
                    , not (T.null (T.strip (oiOutput o)))
                    ]
        , not (T.null body)
        ]

proseComment :: [Text] -> Text
proseComment [] = ""
proseComment chunks =
    "{- \n" <> sanitizeComment (T.intercalate "\n\n" chunks) <> "\n-}"

outputsComment :: Text -> Text
outputsComment t
    | T.null (T.strip t) = ""
    | otherwise = "{- Recorded notebook outputs:\n\n" <> sanitizeComment t <> "\n-}"

warningsComment :: [Text] -> Text
warningsComment [] = ""
warningsComment ws = T.intercalate "\n" ws

-- | Neutralize comment delimiters so embedded text can't break out of a block.
sanitizeComment :: Text -> Text
sanitizeComment = T.replace "-}" "- }" . T.replace "{-" "{ -"

assembleSections :: [Text] -> Text
assembleSections =
    (<> "\n") . T.intercalate "\n\n" . filter (not . T.null) . map T.stripEnd

-- ---------------------------------------------------------------------------
-- Small helpers
-- ---------------------------------------------------------------------------

tShow :: (Show a) => a -> Text
tShow = T.pack . show
