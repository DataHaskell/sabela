{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.Doc (
    NotebookDocOpts (..),
    defaultDocOpts,
    cellHash,
    renderNotebookDoc,
    cellSummary,
    firstNonBlank,
) where

import Data.Aeson (Value, object, (.=))
import Data.Hashable (hash)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Numeric (showHex)

import Sabela.Model (Cell (..), Notebook (..))

data NotebookDocOpts = NotebookDocOpts
    { ndoIncludeFirstLine :: Bool
    , ndoFirstLineLen :: Int
    }

defaultDocOpts :: NotebookDocOpts
defaultDocOpts = NotebookDocOpts{ndoIncludeFirstLine = True, ndoFirstLineLen = 80}

{- | Stable-within-process hash of a cell's identity inputs.
Used to (a) detect external drift under optimistic-concurrency edits
and (b) give the LLM a compact identity it can reason about.
-}
cellHash :: Cell -> Text
cellHash c =
    let h = hash (show (cellLang c), show (cellType c), cellSource c)
        sign = if h < 0 then 'n' else 'p'
        bits = fromIntegral (abs h) :: Integer
     in T.pack (sign : showHex bits "")

-- | Render a compact JSON document summarizing the notebook for the LLM.
renderNotebookDoc :: NotebookDocOpts -> Notebook -> Value
renderNotebookDoc opts nb =
    object
        [ "title" .= nbTitle nb
        , "cellCount" .= length (nbCells nb)
        , "cells" .= zipWith (cellSummary opts) [1 :: Int ..] (nbCells nb)
        ]

cellSummary :: NotebookDocOpts -> Int -> Cell -> Value
cellSummary opts pos c =
    object $
        [ "id" .= cellId c
        , "hash" .= cellHash c
        , "position" .= pos
        , "type" .= cellType c
        , "lang" .= cellLang c
        , "hasError" .= isJust (cellError c)
        , "dirty" .= cellDirty c
        , "lineCount" .= length (T.lines (cellSource c))
        , "outputCount" .= length (cellOutputs c)
        ]
            ++ [ "firstLine" .= T.take (ndoFirstLineLen opts) (firstNonBlank (cellSource c))
               | ndoIncludeFirstLine opts
               ]

firstNonBlank :: Text -> Text
firstNonBlank src =
    case dropWhile (T.null . T.strip) (T.lines src) of
        (l : _) -> l
        [] -> ""
