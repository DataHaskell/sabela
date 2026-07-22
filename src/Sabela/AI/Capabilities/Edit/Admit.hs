{-# LANGUAGE OverloadedStrings #-}

{- | Insert-admission policy: turn a rejected insert into actionable guidance,
and recognise SUPERSESSION — an insert that rewrites the same definitions as
the pending red cell is a fix attempt, so the caller redirects it to a replace
of that cell instead of rejecting it and re-dumping the old error.
-}
module Sabela.AI.Capabilities.Edit.Admit (
    conflictJson,
    holeBodyCompletion,
    pendingErrorFor,
    restickCabal,
    signatureBodyProposal,
    sigBodyProposalFor,
    supersedeNote,
    supersedesRedCell,
    violationJson,
) where

import Data.Aeson (Value, (.=))
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.WriteAck (pendingErrorAck, refusalAck)
import Sabela.Api (errorJsonWith)
import Sabela.Handlers (DefConflict (..), NotebookViolation (..))
import Sabela.Model (CellType (..))
import Sabela.Parse (cellNames)
import Sabela.Parse.Declared (signatureWithoutEquation)
import Sabela.SessionTypes (CellLang (..))

{- | Does the new source rewrite the red cell — i.e. define at least one of the
same top-level names? A shared def means the model is revising that cell, not
starting an unrelated one.
-}
supersedesRedCell :: Text -> Text -> Bool
supersedesRedCell newSrc redSrc =
    not (S.null (defs newSrc `S.intersection` defs redSrc))
  where
    defs = fst . cellNames

{- | Re-add the previous source's @-- cabal:@ lines when a replace dropped
them but still imports modules — silently losing a repair-added dep costs a
kernel restart on the next run. A new source with its own cabal line, or with
no imports at all (a deliberate rewrite), is left untouched.
-}
restickCabal :: Text -> Text -> Text
restickCabal old new
    | not (null oldCabal)
    , null (cabalLines new)
    , any isImport (T.lines new) =
        T.intercalate "\n" (oldCabal ++ [new])
    | otherwise = new
  where
    oldCabal = cabalLines old
    cabalLines = filter (T.isPrefixOf "-- cabal:" . T.stripStart) . T.lines
    isImport = T.isPrefixOf "import " . T.stripStart

{- | Complete a signature-only cell by appending a @name = _@ typed-hole body
for each undefined name — the source the write-boundary proposal hands back. The
hole compiles into a hole-fit diagnostic that enumerates producers, not the
session-damming @Variable not in scope: name@.
-}
holeBodyCompletion :: [Text] -> Text -> Text
holeBodyCompletion names src =
    T.intercalate "\n" (T.stripEnd src : [n <> " = _" | n <- names])

{- | The write-boundary proposal for a top-level signature with no equation
(R9-T4): name the undefined bindings and hand back the same cell completed with
a @name = _@ typed-hole body. A proposal, never a forbid — it offers the correct
next write and suggests no search, so the AI path need never commit the red cell.
-}
signatureBodyProposal :: [Text] -> Text -> Value
signatureBodyProposal names src =
    refusalAck "needs-body" Nothing $
        errorJsonWith
            ( "This cell declares "
                <> T.intercalate ", " ["`" <> n <> "`" | n <- names]
                <> " with a type but no equation, so the binding is empty and the run \
                   \reports it not in scope. Add a body: insert this completed cell, \
                   \which gives each name a typed-hole body the compiler fills from its \
                   \producers.\n"
                <> completed
            )
            ["needsBody" .= names, "suggestedSource" .= completed]
  where
    completed = holeBodyCompletion names src

{- | The write-boundary verdict for an AI insert (R9-T4): a proposal when the
Haskell code cell leaves a top-level signature bodiless, else 'Nothing'
(proceed). Keeps the insert chokepoint one arm; non-Haskell/prose cells pass.
-}
sigBodyProposalFor :: CellLang -> CellType -> Text -> Maybe Value
sigBodyProposalFor Haskell CodeCell src =
    case signatureWithoutEquation src of
        [] -> Nothing
        ns -> Just (signatureBodyProposal ns src)
sigBodyProposalFor _ _ _ = Nothing

-- | The response note when an insert was redirected onto the red cell.
supersedeNote :: Int -> Text
supersedeNote cid =
    "This insert REPLACED cell "
        <> tShow cid
        <> ": that cell was red and your insert defines the same definitions, \
           \so it was applied as replace_cell_source there."
  where
    tShow = T.pack . show

{- | Turn a rejected mutation into actionable guidance: name the binding, point
at the cell that already owns it, and tell the model to use it or edit the owner
instead of redefining it in a new cell.
-}
conflictJson :: DefConflict -> Value
conflictJson (DefConflict name owner) =
    refusalAck "duplicate-def" (Just owner) $
        errorJsonWith
            ( "`"
                <> name
                <> "` is already defined in cell "
                <> tShow owner
                <> " and is in scope. Use it directly, or call replace_cell_source"
                <> " on cell "
                <> tShow owner
                <> " to change that definition. Do not redefine it in a new cell."
            )
            ["binding" .= name, "ownerCell" .= owner]
  where
    tShow = T.pack . show

{- | Turn a rejected insert into actionable guidance. A duplicate reuses
'conflictJson'; a pending error names the failing cell and the two legal moves
('pendingErrorAck') — the raw compiler error is never re-shipped downstream.
-}
violationJson :: NotebookViolation -> Value
violationJson (VDuplicateDef dc) = conflictJson dc
violationJson (VPendingError cid _msg) = pendingErrorAck cid Nothing

{- | The pending-error refusal for a blocking red cell, carrying an explicitly
unverified completion proposal when the cell is a bodiless signature (the
write-boundary class), else naming the two legal moves. The raw
compiler error is never embedded; the candidate is decided by the red cell's
diagnostic class, never its library.
-}
pendingErrorFor :: Int -> Text -> Value
pendingErrorFor cid redSrc = pendingErrorAck cid (redCellCandidate redSrc)
  where
    redCellCandidate src = case signatureWithoutEquation src of
        [] -> Nothing
        ns -> Just (holeBodyCompletion ns src)
