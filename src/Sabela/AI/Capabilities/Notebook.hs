{-# LANGUAGE OverloadedStrings #-}

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
import Sabela.AI.Doc (cellHash, defaultDocOpts, firstNonBlank, ndoFirstLineLen)
import Sabela.AI.Types (ToolOutcome, errOutcome, okOutcome)
import Sabela.Api (errorJson)
import Sabela.Model
import Sabela.Parse (cellNames)
import Sabela.SessionTypes (CellLang (..))
import Sabela.State (App (..))
import Sabela.State.NotebookStore (readNotebook)

execListCells :: App -> Value -> IO ToolOutcome
execListCells app input = do
    nb <- readNotebook (appNotebook app)
    let entries = zipWith (cellListEntry (fieldBool "full" input)) [1 :: Int ..] (nbCells nb)
    pure $ okOutcome $ object ["title" .= nbTitle nb, "cells" .= entries]

listCellSourceCap :: Int
listCellSourceCap = 4000

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
                else ["firstLine" .= firstLine, "lineCount" .= length (T.lines src)]
  where
    src = cellSource c
    firstLine = T.take (ndoFirstLineLen defaultDocOpts) (firstNonBlank src)
    overCap = T.length src > listCellSourceCap
    source
        | overCap =
            T.take listCellSourceCap src
                <> "\n-- … (truncated; use read_cell for the full source)"
        | otherwise = src

cellDefines :: Cell -> [T.Text]
cellDefines c
    | cellType c == CodeCell && cellLang c == Haskell =
        S.toAscList (fst (cellNames (cellSource c)))
    | otherwise = []

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
