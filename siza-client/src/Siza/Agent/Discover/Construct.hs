{-# LANGUAGE OverloadedStrings #-}

module Siza.Agent.Discover.Construct (
    attachProducers,
    constructAnswers,
    constructEnvelope,
    producerKey,
) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Char (isUpper)
import Data.List (partition, sortOn)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.AI.Capabilities.ToolName (ToolName (..))
import Sabela.AI.HoleFits (HoleFit (..), parseHoleFits)
import Sabela.AI.Types (ToolOutcome (..))
import Siza.Agent.Discover.Classify (capabilityAnswer)
import Siza.Agent.Discover.Goal (
    argTypesOf,
    nominalArgType,
    producesExact,
    producesGoal,
 )
import Siza.Agent.Discover.Merge (envelopeFrom, mergedHits)
import Siza.Agent.Discover.Rank (RankKey, rankKey)
import Siza.Agent.Discover.Types (
    DHit (..),
    HackageInfo,
    InstallState (..),
    Interpreted (..),
    MatchKind (..),
    NotebookEnv,
    Scope,
    SourceAnswer (..),
    StandingGoal (..),
    okAnswer,
 )

constructAnswers ::
    (ToolName -> Value -> IO (Either Text ToolOutcome)) ->
    Interpreted ->
    IO [SourceAnswer]
constructAnswers call interp = do
    blob <- callResult (call FindByType (object ["goal" .= ("_ :: " <> goal)]))
    cap <-
        callOk . call SearchCapability $
            object ["query" .= (":: " <> goal), "semantic" .= False]
    pure [sessionProducers goal blob, capabilityAnswer interp cap]
  where
    goal = iName interp

sessionProducers :: Text -> Text -> SourceAnswer
sessionProducers goal blob =
    okAnswer
        "session"
        [producerHit goal f | f <- fits, producesGoal goal (hfType f)]
  where
    fits = [f | f <- parseHoleFits blob, not (hfRefined f)]

producerHit :: Text -> HoleFit -> DHit
producerHit goal f =
    DHit
        (hfWrite f)
        (hfType f)
        ""
        ""
        ""
        InstInstalled
        (if producesExact goal (hfType f) then MkExact else MkType)
        "session"
        Nothing
        Nothing

constructEnvelope ::
    Maybe StandingGoal ->
    NotebookEnv ->
    Interpreted ->
    Scope ->
    Int ->
    [SourceAnswer] ->
    HackageInfo ->
    Value
constructEnvelope mSG env interp scope limit answers hk =
    envelopeFrom env interp scope limit answers hk Nothing ranked
  where
    goal = iName interp
    (producers, others) =
        partition (producesGoal goal . dhType) (mergedHits env interp answers hk)
    ranked =
        sortOn (producerKey mSG env interp goal) producers
            ++ sortOn (otherKey mSG env interp) others

otherKey ::
    Maybe StandingGoal ->
    NotebookEnv ->
    Interpreted ->
    DHit ->
    (Int, RankKey)
otherKey mSG env interp h = (targetBand, rankKey env interp h)
  where
    targetBand = case mSG of
        Just sg
            | not (T.null (sgPackage sg))
            , dhPackage h == sgPackage sg ->
                0
        _ -> 1

producerKey ::
    Maybe StandingGoal ->
    NotebookEnv ->
    Interpreted ->
    Text ->
    DHit ->
    (Int, Int, Int, Int, RankKey)
producerKey mSG env interp goal h =
    ( goalProvenance
    , argCount (dhType h)
    , if isConstructor (dhName h) then 0 else 1
    , if producesExact goal (dhType h) then 0 else 1
    , rankKey env interp h
    )
  where
    goalProvenance = case mSG of
        Just sg
            | sgType sg == goal
            , not (T.null (sgPackage sg))
            , dhPackage h == sgPackage sg ->
                0
        _ -> 1

isConstructor :: Text -> Bool
isConstructor n = maybe False (isUpper . fst) (T.uncons n)

argCount :: Text -> Int
argCount ty = length (T.splitOn "->" (T.unwords (T.words ty))) - 1

attachProducers ::
    Maybe StandingGoal ->
    (ToolName -> Value -> IO (Either Text ToolOutcome)) ->
    NotebookEnv ->
    Interpreted ->
    [SourceAnswer] ->
    IO [SourceAnswer]
attachProducers mSG call env interp answers = case neededType of
    Nothing -> pure []
    Just (consumer, ty) -> do
        pAnswers <- constructAnswers call interp{iName = ty}
        let keep =
                take maxAttached
                    . nubOnName
                    . sortOn (producerKey mSG env interp ty)
                    . map (tagProducer consumer ty)
                    $ concatMap
                        (filter (producesGoal ty . dhType) . saHits)
                        pAnswers
        pure [okAnswer "session" keep | not (null keep)]
  where
    hits = concatMap saHits answers
    neededType =
        listToMaybe
            [ (dhName h, ty)
            | h <- hits
            , dhKind h == MkExact
            , dhName h == iName interp
            , not (T.null (dhType h))
            , ty <- take 1 (unconstructedArgs (dhType h))
            ]
    unconstructedArgs sig =
        [t | t <- argTypesOf sig, nominalArgType t, not (surfaced t)]
    surfaced t = any (producesGoal t . dhType) hits

nubOnName :: [DHit] -> [DHit]
nubOnName = go []
  where
    go _ [] = []
    go seen (h : hs)
        | dhName h `elem` seen = go seen hs
        | otherwise = h : go (dhName h : seen) hs

tagProducer :: Text -> Text -> DHit -> DHit
tagProducer consumer ty h =
    h
        { dhKind = MkType
        , dhUse = Just ("produces " <> ty <> " — the argument `" <> consumer <> "` needs")
        }

maxAttached :: Int
maxAttached = 2

callResult ::
    IO (Either Text ToolOutcome) -> IO Text
callResult act = do
    r <- act
    pure $ case r of
        Right (ToolOk (Object o)) -> case KM.lookup "result" o of
            Just (String s) -> s
            _ -> ""
        _ -> ""

callOk :: IO (Either Text ToolOutcome) -> IO (Maybe Value)
callOk act = do
    r <- act
    pure $ case r of
        Right (ToolOk v) -> Just v
        _ -> Nothing
