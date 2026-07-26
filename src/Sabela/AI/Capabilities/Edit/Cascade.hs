{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sabela.AI.Capabilities.Edit.Cascade (
    executeWithRepair,
    parseRepairBudget,
    repairTierOrder,
) where

import Data.Aeson (Value)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Time (diffUTCTime, getCurrentTime)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import Sabela.AI.Capabilities.Edit.Assemble (applicationCandidates)
import Sabela.AI.Capabilities.Edit.Cascade.Commit (
    applyFresh,
    proposeDependency,
    restoreIfStale,
    verifyAndRevert,
 )
import Sabela.AI.Capabilities.Edit.Exec (executeCell)
import Sabela.AI.Capabilities.Edit.HoleSearch (
    argInsertCandidates,
    holeFitCandidates,
    holeSearchCandidates,
 )
import Sabela.AI.Capabilities.Edit.Repair (
    dependencySuggestion,
    firstFix,
    moduleDepStep,
 )
import Sabela.AI.Capabilities.Edit.Repair.Mitigate.Loop (runMitigations)
import Sabela.AI.Capabilities.Edit.Repair.Resolvers (
    hoogleCandidates,
    moduleResolveCandidates,
    pathNearMissCandidates,
    typeDiscoverCandidates,
 )
import Sabela.AI.Capabilities.Edit.TypeSelect (selectCleanByTypeCheck)
import Sabela.AI.Health (healthOfResult, isClean)
import Sabela.AI.Repair (firstJustM)
import Sabela.AI.RepairTrace (RepairEvent (..), recordRepair)
import Sabela.AI.Store (AIStore)
import Sabela.AI.TypedHole (containsTypedHole)
import Sabela.AI.Types (ExecutionResult (..))
import Sabela.Anthropic.Types (CancelToken)
import Sabela.Handlers (ReactiveNotebook)
import Sabela.Model
import Sabela.State

executeWithRepair ::
    App ->
    AIStore ->
    ReactiveNotebook ->
    Int ->
    CancelToken ->
    IO (Either Text ExecutionResult, [Value], Maybe Value)
executeWithRepair app store rn cid cancelTok = do
    res0 <- executeCell app rn cid cancelTok
    mCell0 <- lookupCell cid <$> readNotebook (appNotebook app)
    staleRef <- newIORef False
    sugRef <- newIORef []
    mitigateRef <- newIORef Nothing
    start <- getCurrentTime
    budget <- parseRepairBudget <$> lookupEnv "SABELA_REPAIR_BUDGET_SECS"
    final <-
        if maybe False (containsTypedHole . cellSource) mCell0
            then pure res0
            else go start budget staleRef sugRef mitigateRef repairCap res0
    suggestions <- reverse <$> readIORef sugRef
    mitigations <- readIORef mitigateRef
    pure (final, suggestions, mitigations)
  where
    go start budget staleRef sugRef mitigateRef n res
        | n <= 0 = restoreIfStale app rn cancelTok cid staleRef res
        | otherwise = do
            now <- getCurrentTime
            if realToFrac (diffUTCTime now start) > budget
                then restoreIfStale app rn cancelTok cid staleRef res
                else do
                    fired <-
                        firstJustM
                            (\nm -> tierBody nm staleRef sugRef mitigateRef res)
                            repairTierOrder
                    case fired of
                        Just (_, newRes) ->
                            go start budget staleRef sugRef mitigateRef (n - 1) newRes
                        Nothing -> restoreIfStale app rn cancelTok cid staleRef res
    tierBody nm = case nm of
        "mitigate" -> tierMitigate
        "firstFix" -> tierFirstFix
        "moduleDep" -> tierModuleDep
        "speculative" -> tierSpeculative
        "resolvers" -> tierResolvers
        "restart" -> tierRestart
        _ -> \_ _ _ _ -> pure Nothing
    tierMitigate staleRef sugRef mitigateRef res = do
        (result, disclosure) <-
            runMitigations app store rn cid cancelTok sugRef staleRef mitigationRoundCap res
        case disclosure of
            Just v -> writeIORef mitigateRef (Just v)
            Nothing -> pure ()
        pure result
    tierFirstFix staleRef sugRef _mitigateRef res = do
        nb <- readNotebook (appNotebook app)
        case lookupCell cid nb of
            Nothing -> pure Nothing
            Just c -> do
                let src = cellSource c
                proposeDependency app cid sugRef src (dependencySuggestion res src)
                case firstFix res src of
                    Just newSrc -> applyFresh app rn cancelTok cid staleRef src newSrc
                    Nothing -> pure Nothing
    tierModuleDep staleRef sugRef _mitigateRef res = withCellSrc $ \src -> do
        mDep <- moduleDepStep res src
        verifyAndRevert
            app
            rn
            cancelTok
            cid
            sugRef
            staleRef
            res
            src
            (maybe [] pure mDep)
    tierSpeculative staleRef sugRef _mitigateRef res = withCellSrc $ \src -> do
        arityCands <- applicationCandidates app res src
        argCands <- argInsertCandidates app res src
        holeSearchCands <- holeSearchCandidates app res src
        (endorsed, lexical) <- holeFitCandidates app store res src
        lexWins <-
            selectCleanByTypeCheck
                app
                (arityCands ++ argCands ++ lexical ++ holeSearchCands)
        let wins = endorsed ++ lexWins
        traceSpeculative
            res
            cid
            (arityCands ++ argCands)
            holeSearchCands
            (endorsed ++ lexical)
            (listToMaybe wins)
        verifyAndRevert
            app
            rn
            cancelTok
            cid
            sugRef
            staleRef
            res
            src
            (take speculativeCap wins)
    tierResolvers staleRef sugRef _mitigateRef res = withCellSrc $ \src -> do
        modCands <- moduleResolveCandidates app res src
        pathCands <- pathNearMissCandidates app res src
        verifyAndRevert
            app
            rn
            cancelTok
            cid
            sugRef
            staleRef
            res
            src
            (modCands ++ pathCands)
    tierRestart staleRef sugRef _mitigateRef res = withCellSrc $ \src -> do
        typeCands <- typeDiscoverCandidates app store res src
        hoogCands <- hoogleCandidates app store res src
        traceRestart res typeCands hoogCands
        verifyAndRevert
            app
            rn
            cancelTok
            cid
            sugRef
            staleRef
            res
            src
            (typeCands ++ hoogCands)
    withCellSrc k = do
        mCell <- lookupCell cid <$> readNotebook (appNotebook app)
        maybe (pure Nothing) (k . cellSource) mCell
    traceSpeculative res' tracedCell arity holeSearch holeFit mWin
        | isClean (healthOfResult res') = pure ()
        | otherwise =
            recordRepair (envWorkDir (appEnv app)) $
                RepairEvent
                    { reCellId = tracedCell
                    , reCounts =
                        [ ("arity", length arity)
                        , ("holeSearch", length holeSearch)
                        , ("holeFit", length holeFit)
                        ]
                    , reWinner = mWin >>= sourceOf
                    }
      where
        sourceOf w
            | w `elem` arity = Just "arity"
            | w `elem` holeSearch = Just "holeSearch"
            | w `elem` holeFit = Just "holeFit"
            | otherwise = Nothing
    traceRestart res' typeCands hoogCands
        | isClean (healthOfResult res') = pure ()
        | otherwise =
            recordRepair (envWorkDir (appEnv app)) $
                RepairEvent
                    { reCellId = cid
                    , reCounts =
                        [ ("typeDiscover", length typeCands)
                        , ("hoogle", length hoogCands)
                        ]
                    , reWinner = Nothing
                    }

repairTierOrder :: [Text]
repairTierOrder =
    ["mitigate", "firstFix", "moduleDep", "speculative", "resolvers", "restart"]

repairCap :: Int
repairCap = 3

mitigationRoundCap :: Int
mitigationRoundCap = 5

speculativeCap :: Int
speculativeCap = 3

parseRepairBudget :: Maybe String -> Double
parseRepairBudget m = fromMaybe 150 (m >>= readMaybe)
