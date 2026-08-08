{-# LANGUAGE OverloadedStrings #-}

module Siza.Agent.Ack (
    reconcileWrite,
    settledWriteFor,
    mergeSettled,
    maxAwaitRounds,
    withDeclaredModules,
) where

import Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.Text (Text)

import Sabela.AI.Capabilities.ToolName (ToolName (..))
import Sabela.AI.Types (ToolOutcome (..), toolOutcomeValue)
import Sabela.AI.WriteAck (executingAckCell)
import Siza.Agent.Discover (declaredPackages)
import Siza.Agent.Discover.CabalFacts (PkgFacts (..))
import Siza.Agent.Discover.Hackage (hackageFactsFor)
import Siza.Agent.Discover.ModuleList (shownModules)

maxAwaitRounds :: Int
maxAwaitRounds = 8

{- | What a committed write made importable, so the caller need not guess module
names. The gate refuses a candidate whose project stage fails, so a package a
committed cell declares is one that resolved.
-}
withDeclaredModules ::
    Value -> Either Text ToolOutcome -> IO (Either Text ToolOutcome)
withDeclaredModules args out = case out of
    Right (ToolOk v)
        | pkgs@(_ : _) <- declaredPackages (sourceOf args) -> do
            facts <- hackageFactsFor pkgs
            pure (Right (ToolOk (withRows (declaredRows facts) v)))
    _ -> pure out
  where
    withRows [] v = v
    withRows rows (Object o) = Object (KM.insert "declared" (toJSON rows) o)
    withRows _ v = v

{- | One row per declared package the index can describe. A package it holds no
modules for states nothing, rather than an empty list that reads as "exposes
nothing".
-}
declaredRows :: [(Text, PkgFacts)] -> [Value]
declaredRows facts =
    [ object ["package" .= p, "modules" .= shownModules f]
    | (p, f) <- facts
    , not (null (pfModules f))
    ]

sourceOf :: Value -> Text
sourceOf (Object o) = case KM.lookup "source" o of
    Just (String s) -> s
    _ -> ""
sourceOf _ = ""

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
