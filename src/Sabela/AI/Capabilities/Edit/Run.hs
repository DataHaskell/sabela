{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Synchronous cell-execution helpers split out of "Sabela.AI.Capabilities.Edit".

These are the pieces every mutating tool (@replace_cell_source@,
@insert_cell@) calls so the tool response carries the freshly-computed
execution summary, plus the @execute_cell@ tool itself. Kept as a
separate module because the listener-and-timeout pattern in 'executeCell'
is also a natural reuse point for a REST blocking-run endpoint. The repair
cascade itself (self_heal's propose-verify-disclose driver, G2) lives in
"Sabela.AI.Capabilities.Edit.Cascade".
-}
module Sabela.AI.Capabilities.Edit.Run (
    autoExecuteAfterMutation,
    execExecuteCell,
    executeCell,
    missingCellError,
    parseRepairBudget,
    repairTierOrder,
    abortCancelled,
    abortSuperseded,
    abortTimedOut,
) where

import Data.Aeson (Value (..), (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.Bindings (attachWriteEcho)
import Sabela.AI.Capabilities.Edit.Cascade (
    executeWithRepair,
    parseRepairBudget,
    repairTierOrder,
 )
import Sabela.AI.Capabilities.Edit.Exec (
    abortCancelled,
    abortSuperseded,
    abortTimedOut,
    executeCell,
 )
import Sabela.AI.Capabilities.Util (fieldInt)
import Sabela.AI.CellResult (mergeToolOk, toCellResult)
import Sabela.AI.ErrorIndex (errorInfoForCell, errorInfoPairs, withErrorInfo)
import Sabela.AI.Health (healthOfResult, isClean)
import Sabela.AI.PathRepair (pathNotFoundGuidance)
import Sabela.AI.SelfHeal (
    attachSelfHeal,
    attachSelfHealSuggestions,
    selfHealNote,
 )
import Sabela.AI.Store
import Sabela.AI.Triage (triageResult)
import Sabela.AI.Types
import Sabela.Anthropic.Types (CancelToken)
import Sabela.Api (errorJson)
import Sabela.Diagnose (
    cellResultWithExtraGuidance,
    guidanceForCell,
    guidancePairs,
 )
import Sabela.Handlers (ReactiveNotebook (..))
import Sabela.Model
import Sabela.State

{- | Run a single cell via the reactive notebook and return the typed
'CellResult' JSON for embedding as the mutation-tool @execution@ summary.
The outcome sum (Succeeded/Raised/Rejected/Aborted) and the @ok@ boolean
ride on the same value the @execute_cell@ tool emits.

@_store@ is the (currently unused) carrier for a staged Output chokepoint;
@crOutputs@ inline raw here. The in-browser chat is bounded by
'Sabela.AI.Orchestrator.Compact'; the REST bridge ('aiToolH') is deliberately
un-stashed on this path.
-}
autoExecuteAfterMutation ::
    App -> AIStore -> ReactiveNotebook -> CancelToken -> Int -> IO Value
autoExecuteAfterMutation app store rn cancelTok cid = do
    pre <- cellSrc app cid
    (res0, suggestions, mitigations) <- executeWithRepair app store rn cid cancelTok
    post <- cellSrc app cid
    pathGuidance <- pathNotFoundGuidance (envWorkDir (appEnv app)) res0
    let res = triageResult post res0
    let cr = toCellResult res (resultOutputs res)
    attachWriteEcho app (isClean (healthOfResult res)) post $
        attachMitigations mitigations $
            attachSelfHealSuggestions suggestions $
                attachSelfHeal
                    (selfHealNote pre post)
                    (withErrorInfo cr (cellResultWithExtraGuidance (maybeToList pathGuidance) cr))

{- | @execute_cell@. @_store@ is the staged Output-chokepoint carrier — see
'autoExecuteAfterMutation'; @crOutputs@ inline raw on this path.
-}
execExecuteCell ::
    App -> AIStore -> ReactiveNotebook -> CancelToken -> Value -> IO ToolOutcome
execExecuteCell app store rn cancelTok input =
    case fieldInt "cell_id" input of
        Nothing -> pure (errOutcome (errorJson "cell_id required"))
        Just cid -> do
            nb <- readNotebook (appNotebook app)
            case missingCellError (nbCells nb) cid of
                Just msg -> pure (errOutcome (errorJson msg))
                Nothing -> do
                    pre <- cellSrc app cid
                    (result0, suggestions, mitigations) <-
                        executeWithRepair app store rn cid cancelTok
                    post <- cellSrc app cid
                    pathGuidance <- pathNotFoundGuidance (envWorkDir (appEnv app)) result0
                    let result = triageResult post result0
                    let cr = toCellResult result (resultOutputs result)
                        heal =
                            maybe [] (\n -> ["self_heal" .= n]) (selfHealNote pre post)
                                <> ["self_heal_suggestions" .= suggestions | not (null suggestions)]
                                <> maybe [] (\m -> ["mitigations" .= m]) mitigations
                    pure $
                        mergeToolOk
                            cr
                            ( ["cellId" .= cid]
                                <> guidancePairs (guidanceForCell cr ++ maybeToList pathGuidance)
                                <> errorInfoPairs (errorInfoForCell cr)
                                <> heal
                            )

{- | The @execute_cell@ pre-check: a target id absent from the notebook fails
fast with a clear, id-naming message. Without it the cell is never dispatched,
so no @EvCellResult@ ever broadcasts and 'executeCell' waits out its full
130s timeout before reporting a misleading abort.
-}
missingCellError :: [Cell] -> Int -> Maybe Text
missingCellError cells cid
    | any ((== cid) . cellId) cells = Nothing
    | otherwise = Just ("No cell with id " <> T.pack (show cid))

-- | Attach G6's mitigation-table disclosure under @mitigations@; identity when 'Nothing'.
attachMitigations :: Maybe Value -> Value -> Value
attachMitigations (Just note) (Object o) = Object (KM.insert "mitigations" note o)
attachMitigations _ v = v

-- | Outputs an @executeCell@ result carried; @[]@ for an abort 'Left'.
resultOutputs :: Either Text ExecutionResult -> [OutputItem]
resultOutputs (Left _) = []
resultOutputs (Right er) = erOutputs er

-- | The cell's current source, for the before/after self-heal delta.
cellSrc :: App -> Int -> IO Text
cellSrc app cid =
    maybe "" cellSource . lookupCell cid <$> readNotebook (appNotebook app)
