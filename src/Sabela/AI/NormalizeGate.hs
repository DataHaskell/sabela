{-# LANGUAGE OverloadedStrings #-}

{- | ONE acceptance law over every machine rewrite (search-api.md section
9.3): the pre-run normalizer ('Sabela.Parse.Normalize') is a candidate
GENERATOR whose output is kept iff the repair cascade's own 'acceptRepair'
law admits it at the parse stage. A rejected rewrite preserves the
submission byte-identically and is disclosed as attempted-and-reverted —
the environment may suggest, only the verifier may assert.
-}
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
import Sabela.Parse.Normalize (normalizeCode, normalizeInsert)

{- | Parse health of a raw cell source: one diagnostic per unparseable
GHCi-fragment chunk, keyed by the chunk's own (normalized) text so a rewrite
that removes a failure without adding one reads as a strict improvement.
-}
parseHealth :: Text -> Health
parseHealth src = Health (null bad) (Set.fromList (map key bad))
  where
    bad = unparseableChunks src
    key c = DiagnosticKey Nothing Nothing ("does not parse: " <> normalizeMsg c)

{- | The ONE acceptance law, applied at the parse stage: the candidate is the
sole target cell and 'acceptRepair' — the same verifier the repair cascade
feeds TierHoleFit\/TierArity candidates through — renders the verdict.
-}
acceptsRewrite :: Text -> Text -> Bool
acceptsRewrite before after =
    acceptRepair
        Set.empty
        [(target, parseHealth before)]
        [(target, parseHealth after)]
        target
  where
    target = "candidate"

{- | 'normalizeInsert' with its rewrite gated by 'acceptsRewrite': kept
rewrites keep the generator's notes; a rejected rewrite returns the
submission byte-identically with the attempt disclosed ('revertNote').
-}
gatedNormalizeInsert :: CellType -> Text -> (CellType, Text, [Text])
gatedNormalizeInsert ty src
    | src' == src = (ty', src, notes)
    | acceptsRewrite src src' = (ty', src', notes <> [currentSourceNote src'])
    | otherwise = (ty', src, [revertNote src src'])
  where
    (ty', src', notes) = normalizeInsert ty src

{- | The replace-path form of the gate: the same 'normalizeCode' generator
composition, kept iff the law admits it.
-}
gatedRewrite :: Text -> Text
gatedRewrite src
    | cand == src = src
    | acceptsRewrite src cand = cand
    | otherwise = src
  where
    cand = fst (normalizeCode src)

{- | The kept-rewrite disclosure (R7.1): the note that announces a machine
rewrite always carries the post-rewrite source it asks the caller to build on.
-}
currentSourceNote :: Text -> Text
currentSourceNote src' =
    "Build on the CURRENT source (normalized before run):\n" <> src'

{- | The attempted-and-reverted disclosure (R7.5): names the rejection and
carries a one-line diff of the proposal — a proposal, never an assertion.
-}
revertNote :: Text -> Text -> Text
revertNote before after =
    "attempted a source rewrite and reverted it (the rewrite did not parse \
    \better than the submission); the cell holds your original source. \
    \Proposed diff was: "
        <> diffLine before after

-- | The first differing line pair, as a bounded @-old +new@ summary.
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
