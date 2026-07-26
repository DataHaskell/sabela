{- | The pure client-side search history ledger (search-api.md sections 8,
10 and 11): cross-turn dedup by query AND by answer hash (R3.8), the
miss-escalation ladder (R5.5-R5.6), the post-nudge gate (R5.7), the
assertion ledger that blocks any denial contradicting an asserted fact
(R1.4, R1.5), and the section 8.3 goal judgement — a found answer whose hits
fail the standing goal advances the SAME ladder a not_found does.
'Siza.Agent.Discover.HistoryGuard' wraps it around dispatch.
-}
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
import Siza.Agent.Discover.MissLadder (missAdvice)
import Siza.Agent.Discover.Resolved (resolvedWhy)
import Siza.Agent.Discover.Steer (goalTypeOf)
import Siza.Agent.Discover.Types (StandingGoal (..))

{- | Answer from the ledger alone, skipping the backend — only ever for a
SEEN scope key (R3.8, section 8.2): the post-close reference replays that
key's own evidence and hands over the sweep's best held hit; an unseen key
falls through to the backends even after close, so closure can never
manufacture a duplicate for a question never asked.
-}
ledgerShortcut :: SearchLedger -> Text -> Maybe Value
ledgerShortcut led q
    | slClosed led = case closedSeen of
        Just (_, summary) ->
            Just . duplicateEnvelope qn "already answered" $
                closedSummary
                    (bestHeldFor (slEvidence led) (entityOf qn))
                    (factsClause (slFacts led))
                    summary
        Nothing -> Nothing
    | Just (n, summary) <- Map.lookup qn (slSeen led) =
        Just
            ( duplicateEnvelope
                qn
                ("call " <> tShow n)
                (stripTried (slTried led) summary)
            )
    | otherwise = Nothing
  where
    qn = T.strip q
    closedSeen =
        case Map.lookup qn (slSeen led) of
            Just hit -> Just hit
            Nothing ->
                snd
                    <$> Map.lookupMin
                        (Map.filterWithKey (\k _ -> sameCluster k qn) (slSeen led))

-- | Equivalent query spellings share a head entity and identical scope.
sameCluster :: Text -> Text -> Bool
sameCluster a b = queryHead a == queryHead b && clusterScope a == clusterScope b
  where
    queryHead = T.toLower . T.takeWhile (\c -> c /= ' ' && c /= '[') . T.strip

{- | Record a discover answer under the truthfulness discipline: a denial
contradicting a seeded, asserted or compiler-proven fact is blocked (R1.4,
3.3); a byte-identical ranked answer dedups to a one-line reference (section
10); an answer judged against the standing or spelled goal that satisfies
nothing walks the SAME miss ladder as a not_found — rungs, dedup, closure
and budget included (section 8.3); misses walk the ladder (R5.5-R5.6).
-}
ledgerRecord :: Text -> Value -> SearchLedger -> (SearchLedger, Value)
ledgerRecord q v led0
    | state == "bad_request" = (led, v)
    | state == "not_found" =
        case protectedFact led cluster of
            Just why -> announce (led, blockedDenial qn why)
            Nothing
                | entity `Set.member` slResolved led ->
                    announce (led, blockedDenial qn resolvedWhy)
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
                     in (repeated, answerDup (strongEvidence v) qn n q0)
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
    evidence' = recordEvidence entity v (slEvidence led)
    -- The standing evidence-derived goal is the authority; the query's own
    -- producer-prefix spelling stays as the cheap trigger (section 8.3).
    standing = standingGoal (slFacts led)
    mGoal = case standing of
        Just sg -> Just sg
        Nothing -> (\t -> StandingGoal t "" "") <$> goalTypeOf target
    -- A standing goal's hunt is ONE cluster, whatever the spelling.
    missCluster = maybe cluster (goalClusterKey . sgType) standing
    vG = maybe v (\sg -> withGoal sg target v) mGoal
    -- The pending world-change note rides the first recorded answer (R1.4).
    announce (l, out) = case slWorldNote l of
        Just note ->
            (l{slWorldNote = Nothing}, setField "worldChange" note out)
        Nothing -> (l, out)
    -- Satisfaction on a DERIVED goal arms the gate; keyed on ledger state.
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
    -- Only strong (exact/card) evidence creates a protected assertion (11.1);
    -- weaker tiers still record close-sweep evidence (section 8.2).
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
                (clearGoal l0{slEvidence = recordEvidence entity v (slEvidence l0)})
    -- An answer that actually produces the goal ends the hunt cluster.
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
    -- The ONE record both not_found and goal-miss walk (section 8.3): the
    -- budget floor binds (R5.6) and the rung count drives dedup only.
    missWalk summary ledIn =
        let n0 = 1 + Map.findWithDefault 0 missCluster (slMisses ledIn)
            n = max n0 (if slClosed ledIn then 3 else slRungFloor ledIn)
            led' =
                (remember summary ledIn)
                    { slMisses = Map.insert missCluster n (slMisses ledIn)
                    }
            bestHeld = bestHeldFor (slEvidence led') entity
            consulted = Set.toAscList (slConsulted led')
         in ( led'
            , missAdvice
                (slTried led')
                (slFacts led')
                bestHeld
                consulted
                n
                qn
                vG
            )

{- | What the satisfied-goal gate answers with: the verdict and the evidence
behind it. It used to end \"write the deliverable now\" — an instruction resting
on the harness's own heuristic that the goal was met.
-}

-- | Fold an envelope's consulted source names into the session record.
noteConsulted :: Value -> SearchLedger -> SearchLedger
noteConsulted v led =
    led{slConsulted = Set.union (Set.fromList (consultedOf v)) (slConsulted led)}

-- | Why a denial of this cluster is illegal, if it is ('protectedBy').
protectedFact :: SearchLedger -> Text -> Maybe Text
protectedFact led = protectedBy (slSeeded led) (slAsserted led)

-- | Record the shapes a query occupies: its raw form and its resolved name.
tryShapes :: Text -> Value -> SearchLedger -> SearchLedger
tryShapes q v led =
    led
        { slTried =
            Set.union
                (Set.fromList [T.toLower (T.strip q), clusterOf v (T.strip q)])
                (slTried led)
        }

-- | Fold newly harvested facts into the bounded held-facts list (R5.6).
harvest :: Value -> SearchLedger -> SearchLedger
harvest v led = led{slFacts = harvestInto v (slFacts led)}

bumpCall :: SearchLedger -> SearchLedger
bumpCall led = led{slCalls = slCalls led + 1}
