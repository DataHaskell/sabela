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

data CandidateVerdict
    = CandidateRejected
    | CandidateSuggested Value
    | CandidateApplyable Text

classifyCandidate ::
    App -> Int -> CellLang -> CellType -> Text -> Text -> IO CandidateVerdict
classifyCandidate app cid lang ty priorSrc candidate = do
    gate <- compileGateCheck app (Just cid) lang ty candidate
    pure $ case gate of
        Left _ -> CandidateRejected
        Right () -> case newDependencies priorSrc candidate of
            [] -> CandidateApplyable candidate
            pkgs -> CandidateSuggested (dependencySuggestionNote pkgs candidate)
