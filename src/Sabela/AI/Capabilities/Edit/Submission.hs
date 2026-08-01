{-# LANGUAGE OverloadedStrings #-}

{- | What the caller wrote, and what the harness will actually put to the
compiler. Both write paths normalise before they gate, so the two differ; the
constructor stays private so no path can hand the gate its own rewrite as if
the caller had typed it.
-}
module Sabela.AI.Capabilities.Edit.Submission (
    Submission,
    submittedText,
    compiledText,
    submissionNotes,
    insertSubmission,
    replaceSubmission,
    reroutedSubmission,
    verbatimSubmission,
) where

import Data.Text (Text)

import Sabela.AI.Capabilities.Edit.Admit (restickCabal)
import Sabela.AI.NormalizeGate (gatedNormalizeInsert, gatedRewrite)
import Sabela.Model (CellType (..))
import Sabela.SessionTypes (CellLang (..))

data Submission = Submission
    { submittedText :: Text
    , compiledText :: Text
    , submissionNotes :: [Text]
    }
    deriving (Eq, Show)

{- | An insert: the cell type may be reclassified and the source normalised,
and every rewrite that happened is a note the caller is owed.
-}
insertSubmission :: CellType -> Text -> (CellType, Submission)
insertSubmission ty src =
    let (ty', src', notes) = gatedNormalizeInsert ty src
     in (ty', Submission src src' notes)

{- | A replace: the cell's @-- cabal:@ line is put back when the replacement
dropped it, then the result is normalised like any other write.
-}
replaceSubmission :: CellLang -> Text -> Text -> Submission
replaceSubmission lang oldSrc newSrc
    | lang /= Haskell = verbatimSubmission newSrc
    | otherwise = Submission newSrc rewritten (stickNote <> gateNotes)
  where
    sticked = restickCabal oldSrc newSrc
    (rewritten, gateNotes) = gatedRewrite sticked
    stickNote =
        [ "Kept the -- cabal: line your replace omitted (its imports still need it)."
        | sticked /= newSrc
        ]

{- | A write rerouted onto an existing cell. The caller's own bytes stay the
submission across the reroute, and both sets of notes are owed.
-}
reroutedSubmission :: CellLang -> Text -> Submission -> Submission
reroutedSubmission lang oldSrc sub =
    onto
        { submittedText = submittedText sub
        , submissionNotes = submissionNotes sub <> submissionNotes onto
        }
  where
    onto = replaceSubmission lang oldSrc (compiledText sub)

-- | A source nothing rewrote: the compiler reads exactly what was submitted.
verbatimSubmission :: Text -> Submission
verbatimSubmission src = Submission src src []
