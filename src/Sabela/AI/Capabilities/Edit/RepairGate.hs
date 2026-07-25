{- | G2: self_heal demoted to propose-verify-disclose. A repair candidate never
touches the live notebook before it is known-green: 'classifyCandidate' runs
every proposed rewrite through G1's 'compileGateCheck' FIRST. A gate-green
rewrite that only compiles by adding a package the model did not write is
downgraded to a disclosed 'CandidateSuggested' — never committed, however
green it verifies (a dependency is a deliberate, visible act).
-}
module Sabela.AI.Capabilities.Edit.RepairGate (
    CandidateVerdict (..),
    classifyCandidate,
) where

import Data.Aeson (Value)
import Data.Text (Text)

import Sabela.AI.Capabilities.Edit.CompileGate (compileGateCheck)
import Sabela.AI.DepRepair (newDependencies)
import Sabela.AI.SelfHeal (dependencySuggestionNote)
import Sabela.Model (CellType)
import Sabela.SessionTypes (CellLang)
import Sabela.State (App)

-- | What a proposed rewrite may do next: never committed unless it is Applyable.
data CandidateVerdict
    = -- | Failed the compile gate: never touches the notebook.
      CandidateRejected
    | -- | Compiles, but only by adding a dependency the model never wrote:
      -- disclosed as a suggestion, never applied (G2 hard rule 1).
      CandidateSuggested Value
    | -- | Compiles with no new dependency: may commit.
      CandidateApplyable Text

{- | Gate-check @candidate@ against the notebook prefix cell @cid@ replaces
(G1's 'compileGateCheck'), then classify it against @priorSrc@ (the cell's
source before this repair round) to decide whether it may commit.
-}
classifyCandidate ::
    App -> Int -> CellLang -> CellType -> Text -> Text -> IO CandidateVerdict
classifyCandidate app cid lang ty priorSrc candidate = do
    gate <- compileGateCheck app (Just cid) lang ty candidate
    pure $ case gate of
        Left _ -> CandidateRejected
        Right () -> case newDependencies priorSrc candidate of
            [] -> CandidateApplyable candidate
            pkgs -> CandidateSuggested (dependencySuggestionNote pkgs candidate)
