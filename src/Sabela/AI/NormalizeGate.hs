{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.NormalizeGate (
    acceptsRewrite,
    currentSourceNote,
    gatedNormalizeInsert,
    gatedRewrite,
    parseHealth,
    revertNote,
) where

import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Health (DiagnosticKey (..), Health (..), normalizeMsg)
import Sabela.AI.RepairDispatch (acceptRepair)
import Sabela.Model (CellType (..))
import Sabela.Parse (unparseableChunks)
import Sabela.Parse.Normalize (definesMain, looksLikeHaskellCode, normalizeCode)

parseHealth :: Text -> Health
parseHealth src = Health (null bad) (Set.fromList (map key bad))
  where
    bad = unparseableChunks src
    key c = DiagnosticKey Nothing Nothing ("does not parse: " <> normalizeMsg c)

acceptsRewrite :: Text -> Text -> Bool
acceptsRewrite before after =
    acceptRepair
        Set.empty
        [(target, parseHealth before)]
        [(target, parseHealth after)]
        target
  where
    target = "candidate"

gatedNormalizeInsert :: CellType -> Text -> (CellType, Text, [Text])
gatedNormalizeInsert ty src = (ty', gatedSrc, reclassNotes <> gateNotes)
  where
    reclassified = ty == ProseCell && looksLikeHaskellCode src
    ty' = if reclassified then CodeCell else ty
    reclassNotes =
        ["Inserted as a CodeCell — the source is Haskell, not prose." | reclassified]
    (gatedSrc, gateNotes)
        | ty' == CodeCell = gatedRewrite src
        | otherwise = (src, [])

gatedRewrite :: Text -> (Text, [Text])
gatedRewrite src
    | cand == src = (src, notes)
    | acceptsRewrite src cand = (cand, notes <> [currentSourceNote cand])
    | otherwise = (src, revertNote src cand : mainOutcomeNote src)
  where
    (cand, notes) = normalizeCode src

mainOutcomeNote :: Text -> [Text]
mainOutcomeNote src =
    [ "The committed cell defines `main` but nothing invokes it, so running \
      \the cell executes nothing. Write the body as a top-level `do` block."
    | definesMain src
    ]

currentSourceNote :: Text -> Text
currentSourceNote src' =
    "Build on the CURRENT source (normalized before run):\n" <> src'

revertNote :: Text -> Text -> Text
revertNote before after =
    "attempted a source rewrite and reverted it (the rewrite did not parse \
    \better than the submission); the cell holds your original source. \
    \Proposed diff was: "
        <> diffLine before after

diffLine :: Text -> Text -> Text
diffLine before after = case [p | p <- zipLines, uncurry (/=) p] of
    ((old, new) : _) -> "-`" <> clip old <> "` +`" <> clip new <> "`"
    [] -> "(line count changed)"
  where
    zipLines = zipPad (T.lines before) (T.lines after)
    zipPad xs ys =
        let n = max (length xs) (length ys)
            pad ls = take n (ls ++ repeat "")
         in zip (pad xs) (pad ys)
    clip t = if T.length t > 60 then T.take 60 t <> "…" else t
