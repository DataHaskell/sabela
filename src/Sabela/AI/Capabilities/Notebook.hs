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
) where

import Data.Aeson (Value, object, (.=))
import Data.Maybe (isJust, mapMaybe)
import qualified Data.Text as T

import Sabela.AI.Capabilities.Util (fieldInt, fieldText)
import Sabela.AI.Doc (cellHash)
import Sabela.AI.Types (ToolOutcome, errOutcome, okOutcome)
import Sabela.Api (errorJson)
import Sabela.Model
import Sabela.State (App (..))
import Sabela.State.NotebookStore (readNotebook)

execListCells :: App -> IO ToolOutcome
execListCells app = do
    nb <- readNotebook (appNotebook app)
    let summaries = zipWith summarize [1 :: Int ..] (nbCells nb)
    pure $ okOutcome $ object ["title" .= nbTitle nb, "cells" .= summaries]
  where
    summarize pos c =
        object
            [ "id" .= cellId c
            , "hash" .= cellHash c
            , "position" .= pos
            , "type" .= cellType c
            , "lang" .= cellLang c
            , "firstLine" .= T.take 80 (head' (T.lines (cellSource c)))
            , "hasError" .= isJust (cellError c)
            , "dirty" .= cellDirty c
            ]
    head' [] = ""
    head' (x : _) = x

execReadCell :: App -> Value -> IO ToolOutcome
execReadCell app input = do
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
                                , "hash" .= cellHash c
                                , "type" .= cellType c
                                , "lang" .= cellLang c
                                , "source" .= cellSource c
                                , "outputs" .= cellOutputs c
                                , "error" .= cellError c
                                ]

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
