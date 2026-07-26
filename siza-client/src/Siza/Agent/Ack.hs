{-# LANGUAGE OverloadedStrings #-}

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

maxAwaitRounds :: Int
maxAwaitRounds = 8

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
