{-# LANGUAGE OverloadedStrings #-}

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

supersedesRedCell :: Text -> Text -> Bool
supersedesRedCell newSrc redSrc =
    not (S.null (defs newSrc `S.intersection` defs redSrc))
  where
    defs = fst . cellNames

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

holeBodyCompletion :: [Text] -> Text -> Text
holeBodyCompletion names src =
    T.intercalate "\n" (T.stripEnd src : [n <> " = _" | n <- names])

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

sigBodyProposalFor :: CellLang -> CellType -> Text -> Maybe Value
sigBodyProposalFor Haskell CodeCell src =
    case signatureWithoutEquation src of
        [] -> Nothing
        ns -> Just (signatureBodyProposal ns src)
sigBodyProposalFor _ _ _ = Nothing

supersedeNote :: Int -> Text
supersedeNote cid =
    "This insert REPLACED cell "
        <> tShow cid
        <> ": that cell was red and your insert defines the same definitions, \
           \so it was applied as replace_cell_source there."
  where
    tShow = T.pack . show

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

violationJson :: NotebookViolation -> Value
violationJson (VDuplicateDef dc) = conflictJson dc
violationJson (VPendingError cid _msg) = pendingErrorAck cid Nothing

pendingErrorFor :: Int -> Text -> Value
pendingErrorFor cid redSrc = pendingErrorAck cid (redCellCandidate redSrc)
  where
    redCellCandidate src = case signatureWithoutEquation src of
        [] -> Nothing
        ns -> Just (holeBodyCompletion ns src)
