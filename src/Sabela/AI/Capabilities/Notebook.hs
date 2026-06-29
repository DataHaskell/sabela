{-# LANGUAGE OverloadedStrings #-}

{- | Read-only notebook inspection tools: @list_cells@, @read_cell@,
@read_cell_output@, @find_cells_by_content@. Each returns a
'ToolOutcome' so success and failure are distinct constructors rather
than a positional @(Value, Bool)@.
-}
module Sabela.AI.Capabilities.Notebook (
    execListCells,
    execReadCell,
    execReadCellOutput,
    execFindCells,
    cellDefines,
    cellListEntry,
    listCellSourceCap,
) where

import Data.Aeson (Value, object, (.=))
import Data.Maybe (isJust, mapMaybe)
import qualified Data.Set as S
import qualified Data.Text as T

import Sabela.AI.Capabilities.Util (fieldBool, fieldInt, fieldText)
import Sabela.AI.Doc (cellHash)
import Sabela.AI.Types (ToolOutcome, errOutcome, okOutcome)
import Sabela.Api (errorJson)
import Sabela.Model
import Sabela.Parse (cellNames)
import Sabela.SessionTypes (CellLang (..))
import Sabela.State (App (..))
import Sabela.State.NotebookStore (readNotebook)

{- | @list_cells@. The default is a PREVIEW (every cell's metadata + first line),
which always lists the whole notebook compactly; @full=true@ adds each cell's
source (capped per cell). Preview-by-default so the first call never blows the
context budget or hides cells behind a truncation cut.
-}
execListCells :: App -> Value -> IO ToolOutcome
execListCells app input = do
    nb <- readNotebook (appNotebook app)
    let entries = zipWith (cellListEntry (fieldBool "full" input)) [1 :: Int ..] (nbCells nb)
    pure $ okOutcome $ object ["title" .= nbTitle nb, "cells" .= entries]

{- | Per-cell source budget for a @full@ @list_cells@. A cell longer than this is
truncated with a @truncated@ flag; the model fetches the rest with @read_cell@.
-}
listCellSourceCap :: Int
listCellSourceCap = 4000

{- | One @list_cells@ entry. Preview (@full=False@): metadata + @firstLine@ +
@lineCount@, no source — compact, so the whole notebook fits one call. Full
(@full=True@): adds @source@, capped to 'listCellSourceCap'. A pure projection.
-}
cellListEntry :: Bool -> Int -> Cell -> Value
cellListEntry full pos c =
    object $
        [ "id" .= cellId c
        , "hash" .= cellHash c
        , "position" .= pos
        , "type" .= cellType c
        , "lang" .= cellLang c
        , "defines" .= cellDefines c
        , "hasError" .= isJust (cellError c)
        , "dirty" .= cellDirty c
        ]
            ++ if full
                then ("source" .= source) : ["truncated" .= True | overCap]
                else ["firstLine" .= firstLine, "lineCount" .= length ls]
  where
    src = cellSource c
    ls = T.lines src
    firstLine = T.take 120 (case ls of (x : _) -> x; [] -> "")
    overCap = T.length src > listCellSourceCap
    source
        | overCap =
            T.take listCellSourceCap src
                <> "\n-- … (truncated; use read_cell for the full source)"
        | otherwise = src

{- | Top-level binding names a Haskell code cell introduces — the @defines@
field of @list_cells@, so the model reuses real names instead of inventing
them. A pure projection of the parser's def set; empty for prose/Python cells.
-}
cellDefines :: Cell -> [T.Text]
cellDefines c
    | cellType c == CodeCell && cellLang c == Haskell =
        S.toAscList (fst (cellNames (cellSource c)))
    | otherwise = []

{- | @read_cell@. The default returns the cell's full SOURCE and error but omits
its @outputs@ (which can be large rendered HTML/SVG) — a @hasOutputs@ flag points
at them; @full=true@ includes @outputs@. Preview-by-default keeps the first read
cheap; escalate to @full@ only when the output itself is needed.
-}
execReadCell :: App -> Value -> IO ToolOutcome
execReadCell app input = do
    let mcid = fieldInt "cell_id" input
        full = fieldBool "full" input
    case mcid of
        Nothing -> pure (errOutcome (errorJson "cell_id required"))
        Just cid -> do
            nb <- readNotebook (appNotebook app)
            case lookupCell cid nb of
                Nothing ->
                    pure (errOutcome (errorJson ("Cell not found: " <> T.pack (show cid))))
                Just c ->
                    pure $
                        okOutcome $
                            object $
                                [ "id" .= cellId c
                                , "hash" .= cellHash c
                                , "type" .= cellType c
                                , "lang" .= cellLang c
                                , "source" .= cellSource c
                                , "error" .= cellError c
                                ]
                                    ++ if full
                                        then ["outputs" .= cellOutputs c]
                                        else ["hasOutputs" .= not (null (cellOutputs c))]

execReadCellOutput :: App -> Value -> IO ToolOutcome
execReadCellOutput app input = do
    let mcid = fieldInt "cell_id" input
    case mcid of
        Nothing -> pure (errOutcome (errorJson "cell_id required"))
        Just cid -> do
            nb <- readNotebook (appNotebook app)
            case lookupCell cid nb of
                Nothing ->
                    pure (errOutcome (errorJson ("Cell not found: " <> T.pack (show cid))))
                Just c ->
                    pure $
                        okOutcome $
                            object
                                [ "id" .= cellId c
                                , "outputs" .= cellOutputs c
                                , "error" .= cellError c
                                ]

execFindCells :: App -> Value -> IO ToolOutcome
execFindCells app input = do
    let pat = fieldText "pattern" input
    if T.null pat
        then pure (errOutcome (errorJson "pattern required"))
        else do
            nb <- readNotebook (appNotebook app)
            let matches = mapMaybe (matchCell pat) (nbCells nb)
            pure $ okOutcome $ object ["matches" .= matches]
  where
    matchCell pat c
        | pat `T.isInfixOf` cellSource c =
            let ls = zip [1 :: Int ..] (T.lines (cellSource c))
                matchingLines =
                    [ object ["line" .= n, "text" .= T.take 120 l]
                    | (n, l) <- ls
                    , pat `T.isInfixOf` l
                    ]
             in Just $
                    object
                        [ "id" .= cellId c
                        , "lang" .= cellLang c
                        , "matchingLines" .= take 5 matchingLines
                        ]
        | otherwise = Nothing
