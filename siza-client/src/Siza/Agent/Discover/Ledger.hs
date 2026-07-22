{- | The search-ledger STATE (search-api.md sections 8, 10, 11): the
per-session record and its small transitions — seeding, closure, pressure,
the world-change wipe, and the observable ladder projection. The answer
judgement lives in 'Siza.Agent.Discover.History'; the split keeps both under
the module-size cap.
-}
module Siza.Agent.Discover.Ledger (
    SearchLedger (..),
    callReadyFacts,
    emptyLedger,
    heldEvidence,
    heldFacts,
    installFactKey,
    ladderState,
    ledgerClose,
    ledgerDeclare,
    ledgerPressure,
    ledgerResolve,
    ledgerSeed,
    ledgerWorldChanged,
    missClusters,
) where

import Data.Aeson (Value)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Siza.Agent.Discover.Closure (worldNote)
import Siza.Agent.Discover.Types (InstallState, installText)

-- | Per-session search memory; see module haddock for placement rationale.
data SearchLedger = SearchLedger
    { slSeen :: Map Text (Int, Text)
    -- ^ Normalised query -> (call number, one-line summary), current generation.
    , slAnswers :: Map Text (Int, Text)
    -- ^ Answer hash -> (call number, query), current generation (section 10).
    , slAsserted :: Map Text (Int, Text)
    -- ^ Asserted cluster -> (call, summary); denials of these are blocked (R1.4).
    , slSeeded :: Set Text
    -- ^ Turn-0 environment facts (imports, builtins) — deniable by nothing.
    , slResolved :: Set Text
    {- ^ Compiler-proven names (clean check_type, landed compile) — cancel
    lexical not_found in every mode/filter until the world changes (3.3).
    -}
    , slMisses :: Map Text Int
    -- ^ Miss cluster (lowercased resolved target) -> consecutive miss count.
    , slTried :: Set Text
    -- ^ Query shapes tried (raw and resolved), for R5.5 advice filtering.
    , slFacts :: [Text]
    -- ^ Held facts (install state, cabal lines, aliases), first-seen order.
    , slEvidence :: Map Text Value
    -- ^ Cluster entity -> best held hit; the close's union sweep (section 8.2).
    , slConsulted :: Set Text
    -- ^ Source names that have answered this session, for legal cannot-help.
    , slWorldNote :: Maybe Text
    -- ^ Pending world-change announcement, attached to the next answer (R1.4).
    , slRungFloor :: Int
    -- ^ Budget-pressure floor on the miss-escalation rung (R5.6).
    , slGoalSat :: Map Text Int
    -- ^ Goal cluster -> answered calls since satisfaction held (k=2 gate).
    , slDeclaredPkgs :: Set Text
    -- ^ Packages the notebook's cabal lines already declare (R1.4 legality).
    , slClosed :: Bool
    , slCalls :: Int
    }

emptyLedger :: SearchLedger
emptyLedger =
    SearchLedger
        Map.empty
        Map.empty
        Map.empty
        Set.empty
        Set.empty
        Map.empty
        Set.empty
        []
        Map.empty
        Set.empty
        Nothing
        1
        Map.empty
        Set.empty
        False
        0

-- | The recorded best-held-hit evidence, for the close's union sweep.
heldEvidence :: SearchLedger -> Map Text Value
heldEvidence = slEvidence

heldFacts :: SearchLedger -> [Text]
heldFacts = slFacts

-- | The active miss-cluster names, scope keys stripped (R5.6 relevance).
missClusters :: SearchLedger -> [Text]
missClusters led = map (T.takeWhile (/= '@')) (Map.keys (slMisses led))

-- | Call-ready held facts (name + signature): what the nudge fires on (R5.6).
callReadyFacts :: SearchLedger -> [Text]
callReadyFacts led = [f | f <- slFacts led, " :: " `T.isInfixOf` f]

{- | The observable miss-ladder state (R8-T2 satisfaction legality): the
per-cluster rungs, closed flag, rung floor and call count — a found answer
whose hits all fail the goal advances this EXACTLY as a not_found does.
-}
ladderState :: SearchLedger -> ([(Text, Int)], Bool, Int, Int)
ladderState led =
    (Map.toAscList (slMisses led), slClosed led, slRungFloor led, slCalls led)

-- | Close the channel: the nudge said act, so discover stops advising search.
ledgerClose :: SearchLedger -> SearchLedger
ledgerClose led = led{slClosed = True}

{- | Record compiler-proven names: they outrank and cancel lexical not_found
under EVERY mode and filter key until 'ledgerWorldChanged' (3.3, R7-T1).
-}
ledgerResolve :: [Text] -> SearchLedger -> SearchLedger
ledgerResolve ns led =
    led{slResolved = Set.union (Set.fromList (map T.toLower ns)) (slResolved led)}

-- | Miss-ladder floor (R5.6); 'Siza.Agent.Loop.WrapUp.missRungFloor' sets it.
ledgerPressure :: Int -> SearchLedger -> SearchLedger
ledgerPressure n led = led{slRungFloor = max 1 n}

{- | Seed turn-0 facts (imports, documented builtins): asserted before any
call and kept across world changes — a restart keeps the notebook's cells,
so its imports and the documented surface stay true (section 11).
-}
ledgerSeed :: [Text] -> SearchLedger -> SearchLedger
ledgerSeed facts led =
    led{slSeeded = Set.union (Set.fromList (map T.toLower facts)) (slSeeded led)}

{- | Install or restart: dedup, assertion, miss and INSTALL-STATE fact reset —
denial is legal again (R1.4), and the next answer carries the announcement so
the change is never silent. Non-install facts and the turn-0 seed survive;
held-hit evidence resets with the install states it carries.
-}

{- | The package of an install-state fact (@"pkg (state): …"@ as
'Siza.Agent.Discover.Advice.harvestFacts' shapes them); 'Nothing' for any
other held fact. The world-change wipe keys its fact reset on it.
-}
installFactKey :: Text -> Maybe Text
installFactKey f = case T.words f of
    (p : st : _)
        | "(" `T.isPrefixOf` st
        , T.dropAround (`elem` ("():" :: String)) st `elem` states ->
            Just p
    _ -> Nothing
  where
    states = map installText [minBound .. maxBound :: InstallState]

ledgerWorldChanged :: SearchLedger -> SearchLedger
ledgerWorldChanged led =
    led
        { slSeen = Map.empty
        , slAnswers = Map.empty
        , slAsserted = Map.empty
        , slResolved = Set.empty
        , slMisses = Map.empty
        , slFacts = [f | f <- slFacts led, isNothing (installFactKey f)]
        , slEvidence = Map.empty
        , slWorldNote = worldNoteWhenPriorAnswer
        , slGoalSat = Map.empty
        }
  where
    -- A change before any recorded discover answer has nothing prior to stale
    -- (the revenueTotal first-of-session false banner, R1.4); only a change
    -- AFTER a real prior query announces.
    worldNoteWhenPriorAnswer
        | slCalls led > 0 = Just worldNote
        | otherwise = Nothing

{- | Record packages a landed write (or the seeded notebook) declares: a
re-declaration of one of these installs nothing, so it is never a world
change (the revenueTotal spurious-banner class, R1.4).
-}
ledgerDeclare :: [Text] -> SearchLedger -> SearchLedger
ledgerDeclare pkgs led =
    led{slDeclaredPkgs = Set.union (Set.fromList pkgs) (slDeclaredPkgs led)}
