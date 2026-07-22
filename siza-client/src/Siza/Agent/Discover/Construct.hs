{-# LANGUAGE OverloadedStrings #-}

{- | The constructibility facet (docs/discover/search-api.md section 7.1): a
"value of type T" question answered by PRODUCERS of T, ranked nullary-first
(a ready-made value), goal provenance ahead of arity (the held consumer's own
package outranks foreign lexical winners), non-producers demoted below every
true producer — never dropped — and the section 8.3 same-envelope attachment
of an unconstructible argument type's producers. Keyed by diagnostic class,
never a library name.
-}
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

{- | Consult the two producer sources for the goal type: the session's hole
fits of @_ :: T@ (installed producers, in scope now) and the hoogle
result-type search (@:: T@), unioned — a miss in one never discards the other.
-}
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

-- | The session's installed producers of @goal@, parsed from a hole-fit blob.
sessionProducers :: Text -> Text -> SourceAnswer
sessionProducers goal blob =
    okAnswer
        "session"
        [producerHit goal f | f <- fits, producesGoal goal (hfType f)]
  where
    fits = [f | f <- parseHoleFits blob, not (hfRefined f)]

-- | One producer as an installed, session-provenanced hit.
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

{- | Render the facet's envelope: true producers ranked producer-distance
first (goal provenance, nullary, constructors, arity), then every
non-producing hit below them — demoted, never dropped (sections 7.1, 8.3).
-}
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

{- | The non-producer ordering (R5.6): once a target package is established by
ledger evidence (the standing goal's derivation), a hit from that package leads
the non-producer tail and every cross-package hit demotes below it — provenance
evidence keyed, never a package name. Falls back to the plain key with no goal.
-}
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

{- | Producer ordering (section 7.1): goal provenance first — a producer from
the package of the cluster's held consumer signature outranks every foreign
package (ledger evidence, never a package name) — then nullary bindings,
record constructors, arity, exact-goal result, and the deterministic key.
-}
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

-- | An upper-head name is a data constructor (a producer by construction).
isConstructor :: Text -> Bool
isConstructor n = maybe False (isUpper . fst) (T.uncons n)

-- | Top-level argument arrows in a signature.
argCount :: Text -> Int
argCount ty = length (T.splitOn "->" (T.unwords (T.words ty))) - 1

{- | Same-envelope producer attachment (section 8.3): when the query's own
call-ready exact hit needs an argument type with no surfaced producer, the
top 1-2 producers of that type join the SAME envelope — the consumer and how
to build its arguments in one call. Diagnostic-class keyed, never a library.
-}
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

-- | One row per producer name: the best-ranked source copy represents it.
nubOnName :: [DHit] -> [DHit]
nubOnName = go []
  where
    go _ [] = []
    go seen (h : hs)
        | dhName h `elem` seen = go seen hs
        | otherwise = h : go (dhName h : seen) hs

-- | An attached producer never outranks the query's own exact hits (R3.2).
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
