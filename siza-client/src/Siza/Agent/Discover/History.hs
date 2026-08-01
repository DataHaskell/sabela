module Siza.Agent.Discover.History (
    SearchLedger,
    slRefuted,
    emptyLedger,
    heldEvidence,
    ladderState,
    ledgerSeed,
    ledgerShortcut,
    ledgerPressure,
    ledgerRecord,
    ledgerResolve,
    ledgerWorldChanged,
    ledgerClose,
    heldFacts,
    callReadyFacts,
    duplicateEnvelope,
    missClusters,
    slGoalSpent,
    takeEscalation,
) where

import Data.Aeson (Value)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Siza.Agent.Discover.Advice (
    answerDup,
    answerKey,
    clusterName,
    clusterOf,
    clusterScope,
    duplicateEnvelope,
    factsClause,
    foundSummary,
    harvestInto,
    missSummary,
    resolvedTarget,
    scopedFacts,
    setField,
    stripTried,
    strongEvidence,
    tShow,
    topText,
 )
import Siza.Agent.Discover.Closure (
    bestHeldFor,
    blockedDenial,
    closedSummary,
    consultedOf,
    entityOf,
    heldHitsFor,
    protectedBy,
    recordEvidence,
 )
import Siza.Agent.Discover.Goal (
    goalClusterKey,
    goalProduced,
    goalSatisfied,
    standingGoal,
    withGoal,
 )
import Siza.Agent.Discover.Ledger
import Siza.Agent.Discover.MissLadder (
    MissOutcome (..),
    missAdvice,
 )
import Siza.Agent.Discover.Resolved (resolvedWhy)
import Siza.Agent.Discover.Steer (goalTypeOf)
import Siza.Agent.Discover.Types (StandingGoal (..))

ledgerShortcut :: SearchLedger -> Text -> Maybe Value
ledgerShortcut led q
    | slClosed led = case closedSeen of
        Just (_, summary) ->
            Just $
                duplicateEnvelope
                    qn
                    "already answered"
                    ( closedSummary
                        (bestHeldFor (slEvidence led) scopedKey)
                        (factsClause (scopedFacts qn (slFacts led)))
                        summary
                    )
                    held
        Nothing -> Nothing
    | Just (n, summary) <- Map.lookup qn (slSeen led) =
        Just
            ( duplicateEnvelope
                qn
                ("call " <> tShow n)
                (stripTried (slTried led) summary)
                held
            )
    | otherwise = Nothing
  where
    qn = T.strip q
    scopedKey = entityOf qn <> clusterScope qn
    held = heldHitsFor (slEvidence led) scopedKey
    closedSeen =
        case Map.lookup qn (slSeen led) of
            Just hit -> Just hit
            Nothing ->
                snd
                    <$> Map.lookupMin
                        (Map.filterWithKey (\k _ -> sameCluster k qn) (slSeen led))

sameCluster :: Text -> Text -> Bool
sameCluster a b = queryHead a == queryHead b && clusterScope a == clusterScope b
  where
    queryHead = T.toLower . T.takeWhile (\c -> c /= ' ' && c /= '[') . T.strip

ledgerRecord :: Text -> Value -> SearchLedger -> (SearchLedger, Value)
ledgerRecord q v led0
    | state == "bad_request" = (led, v)
    | state == "not_found" =
        case protectedFact led cluster of
            Just why -> announce (led, blockedDenial heldForQuery qn why)
            Nothing
                | entity `Set.member` slResolved led ->
                    announce (led, blockedDenial heldForQuery qn resolvedWhy)
                | otherwise -> announce (missWalk (missSummary v) (discoverFresh led))
    | Just sg <- mGoal
    , not (goalSatisfied sg target v) =
        announce
            ( missWalk
                ("goal unsatisfied; " <> foundSummary v)
                (discoverFresh led){slEvidence = evidence'}
            )
    | Just key <- answerKey v =
        announce $ case Map.lookup key (slAnswers led) of
            Just (n, q0)
                | q0 /= qn ->
                    let (repeated, _hard) = discoverRepeat qn (assertFound led)
                     in (repeated, answerDup v qn n q0)
            _ ->
                let led' = assertFound led
                 in ( (discoverFresh led')
                        { slAnswers =
                            Map.insert key (slCalls led', qn) (slAnswers led')
                        }
                    , vG
                    )
    | otherwise = announce (discoverFresh (assertFound led), vG)
  where
    led = noteConsulted v (harvest v (bumpCall (tryShapes q v led0)))
    state = topText "state" v
    qn = T.strip q
    cluster = clusterOf v qn
    entity = clusterName v qn
    target = resolvedTarget v qn
    evidence' = recordEvidence cluster v (slEvidence led)
    heldForQuery = heldHitsFor (slEvidence led) cluster
    standing = standingGoal (slFacts led)
    mGoal = case standing of
        Just sg -> Just sg
        Nothing -> (\t -> StandingGoal t "" "") <$> goalTypeOf target
    -- Scope belongs in the miss key: two package scopes are two questions.
    -- Spelling deliberately does not: those converge on one cluster.
    scopeKey =
        T.concat
            [ seg
            | seg <- T.words qn
            , "[" `T.isPrefixOf` seg
            , not ("[limit=" `T.isPrefixOf` seg)
            ]
    missCluster =
        maybe cluster (\sg -> goalClusterKey (sgType sg) <> scopeKey) standing
    vG = maybe v (\sg -> withGoal sg target v) mGoal
    announce (l, out) = case slWorldNote l of
        Just note ->
            (l{slWorldNote = Nothing}, setField "worldChange" note out)
        Nothing -> (l, out)
    markSatisfied l = case standing of
        Just sg
            | goalSatisfied sg target v ->
                l
                    { slGoalSat =
                        Map.insertWith
                            (\_ old -> old)
                            (goalClusterKey (sgType sg))
                            0
                            (slGoalSat l)
                    }
        _ -> l
    assertFound l0
        | strongEvidence v =
            (remember (foundSummary v) l)
                { slAsserted =
                    Map.insert cluster (slCalls l, foundSummary v) (slAsserted l)
                }
        | otherwise = remember (foundSummary v) l
      where
        l =
            markSatisfied
                (clearGoal l0{slEvidence = recordEvidence cluster v (slEvidence l0)})
    clearGoal l = case standing of
        Just sg
            | goalProduced (sgType sg) v ->
                l{slMisses = Map.delete (goalClusterKey (sgType sg)) (slMisses l)}
        _ -> l
    remember summary l =
        l
            { slSeen = Map.insert qn (slCalls l, summary) (slSeen l)
            , slMisses = Map.delete cluster (slMisses l)
            }
    missWalk summary ledIn =
        let n0 = 1 + Map.findWithDefault 0 missCluster (slMisses ledIn)
            n = max n0 (if slClosed ledIn then 3 else slRungFloor ledIn)
            led' =
                (remember summary ledIn)
                    { slMisses = Map.insert missCluster n (slMisses ledIn)
                    }
            held = heldHitsFor (slEvidence led') cluster
            bestHeld = bestHeldFor (slEvidence led') cluster
            consulted = Set.toAscList (slConsulted led')
         in arm led' $
                missAdvice
                    held
                    (slTried led')
                    (slFacts led')
                    bestHeld
                    consulted
                    unspentGoal
                    n
                    qn
                    vG
    {- The goal is worth a type query only while it stands unsatisfied and no
    query has been spent on its cluster; the spend is recorded when decided. -}
    unspentGoal = case standing of
        Just sg
            | not (goalSatisfied sg target v)
            , goalClusterKey (sgType sg) `Set.notMember` slGoalSpent led ->
                Just sg
        _ -> Nothing
    arm l (Advise out) = (l, out)
    arm l (EscalateType sg out) =
        ( l
            { slGoalSpent =
                Set.insert (goalClusterKey (sgType sg)) (slGoalSpent l)
            , slEscalate = Just sg
            }
        , out
        )

noteConsulted :: Value -> SearchLedger -> SearchLedger
noteConsulted v led =
    led{slConsulted = Set.union (Set.fromList (consultedOf v)) (slConsulted led)}

protectedFact :: SearchLedger -> Text -> Maybe Text
protectedFact led = protectedBy (slSeeded led) (slAsserted led)

tryShapes :: Text -> Value -> SearchLedger -> SearchLedger
tryShapes q v led =
    led
        { slTried =
            Set.union
                (Set.fromList [T.toLower (T.strip q), clusterOf v (T.strip q)])
                (slTried led)
        }

harvest :: Value -> SearchLedger -> SearchLedger
harvest v led = led{slFacts = harvestInto v (slFacts led)}

bumpCall :: SearchLedger -> SearchLedger
bumpCall led = led{slCalls = slCalls led + 1}
