{-# LANGUAGE OverloadedStrings #-}

{- | Client-side write-ack reconciliation (R6.1): when a mutation write acks
@executing@ (the server's ack outran the cell's execution), follow up with
bounded @await_idle@ calls and merge the settled outcome back into the ack,
so every downstream consumer — the health gate ('Siza.Agent.Owned'), the
repair cascade and rejection sampling — sees a settled execution status
instead of misreading the ack as a red cell.
-}
module Siza.Agent.Ack (
    reconcileWrite,
    settledWriteFor,
    mergeSettled,
    maxAwaitRounds,
) where

import Data.Aeson (Value (..), object)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.Text (Text)

import Sabela.AI.Capabilities.ToolName (ToolName (..))
import Sabela.AI.Types (ToolOutcome (..), toolOutcomeValue)
import Sabela.AI.WriteAck (executingAckCell)

-- | Bounded @await_idle@ follow-ups before the honest executing ack is kept.
maxAwaitRounds :: Int
maxAwaitRounds = 8

{- | Reconcile an @executing@ write ack through the injected tool caller.
Any other outcome (settled ack, error, transport failure) passes through
untouched; a write that never settles keeps its truthful executing ack.
-}
reconcileWrite ::
    (ToolName -> Value -> IO (Either Text ToolOutcome)) ->
    Either Text ToolOutcome ->
    IO (Either Text ToolOutcome)
reconcileWrite call out = case out of
    Right (ToolOk v)
        | Just cid <- executingAckCell v -> loop cid maxAwaitRounds v
    _ -> pure out
  where
    loop _ 0 v = pure (Right (ToolOk v))
    loop cid n v = do
        r <- call AwaitIdle (object [])
        case r of
            Left _ -> pure (Right (ToolOk v))
            Right o -> case settledWriteFor cid (toolOutcomeValue o) of
                Just entry -> pure (Right (ToolOk (mergeSettled v entry)))
                Nothing -> loop cid (n - 1) v

-- | The @writes@ reconciliation entry for a cell in an @await_idle@ reply.
settledWriteFor :: Int -> Value -> Maybe Value
settledWriteFor cid (Object o) = case KM.lookup "writes" o of
    Just (Array ws) ->
        headMaybe [w | w@(Object wo) <- toList ws, cellIdOf wo == Just cid]
    _ -> Nothing
  where
    cellIdOf wo = case KM.lookup "cellId" wo of
        Just (Number n) -> Just (round n)
        _ -> Nothing
    headMaybe (x : _) = Just x
    headMaybe [] = Nothing
settledWriteFor _ _ = Nothing

{- | Merge a settled reconciliation entry into the original executing ack:
status becomes the settled one and the execution summary rides along, so the
merged value reads exactly like an inline completed ack.
-}
mergeSettled :: Value -> Value -> Value
mergeSettled (Object ack) (Object entry) =
    Object
        ( KM.delete "note" $
            foldr
                copy
                ack
                ["status", "execution"]
        )
  where
    copy k m = case KM.lookup (K.fromText k) entry of
        Just v -> KM.insert (K.fromText k) v m
        Nothing -> m
mergeSettled ack _ = ack
