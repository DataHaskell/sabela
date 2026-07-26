{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

missingCellError :: [Cell] -> Int -> Maybe Text
missingCellError cells cid
    | any ((== cid) . cellId) cells = Nothing
    | otherwise = Just ("No cell with id " <> T.pack (show cid))

attachMitigations :: Maybe Value -> Value -> Value
attachMitigations (Just note) (Object o) = Object (KM.insert "mitigations" note o)
attachMitigations _ v = v

resultOutputs :: Either Text ExecutionResult -> [OutputItem]
resultOutputs (Left _) = []
resultOutputs (Right er) = erOutputs er

cellSrc :: App -> Int -> IO Text
cellSrc app cid =
    maybe "" cellSource . lookupCell cid <$> readNotebook (appNotebook app)
