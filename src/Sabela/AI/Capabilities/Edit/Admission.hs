{-# LANGUAGE OverloadedStrings #-}

{- | What an admitted write carries besides its source: the repairs the
harness applied to it, and the property the gate settled about it. They are
kept apart because one field for both would publish a disclosure about a
candidate as an edit to it.
-}
module Sabela.AI.Capabilities.Edit.Admission (
    Admission (..),
    admitted,
    withRepairs,
    admissionPairs,
    admissionNotes,
) where

import Data.Aeson ((.=))
import Data.Aeson.Types (Pair)
import Data.Text (Text)

import Sabela.AI.Capabilities.Try.Payload.Checked (RunRecord, runRecordNote)

data Admission = Admission
    { admittedSource :: Text
    , admittedRepairs :: [Text]
    , admittedChecked :: [Text]
    }
    deriving (Eq, Show)

-- | A source admitted untouched, with nothing claimed about it.
admitted :: Text -> Admission
admitted src =
    Admission{admittedSource = src, admittedRepairs = [], admittedChecked = []}

-- | Repairs earned earlier in the gate stack, kept ahead of later ones.
withRepairs :: [Text] -> Admission -> Admission
withRepairs earlier a = a{admittedRepairs = earlier <> admittedRepairs a}

{- | The admission as the write's payload publishes it. Each field appears
only when something was computed for it: no repairs means no @repairs@ key,
so an absent key is never read as an empty edit.
-}
admissionPairs :: RunRecord -> Admission -> [Pair]
admissionPairs run a =
    ["repairs" .= admittedRepairs a | not (null (admittedRepairs a))]
        <> ["checked" .= checkedFor run a | not (null (admittedChecked a))]

{- | The same two, as prose, for the acknowledgement path that has no payload
object to hang fields on.
-}
admissionNotes :: RunRecord -> Admission -> [Text]
admissionNotes run a = admittedRepairs a <> checkedFor run a

{- | The gate's finding published together with what the payload around it
settles, so the reader is never left to infer that a candidate the same
payload reports running was never run.
-}
checkedFor :: RunRecord -> Admission -> [Text]
checkedFor run a
    | null (admittedChecked a) = []
    | otherwise = admittedChecked a <> [runRecordNote run]
