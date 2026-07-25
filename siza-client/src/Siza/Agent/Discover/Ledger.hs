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
    ledgerInvalidateOrientation,
    orientationRecord,
    orientationShortcut,
    discoverFresh,
    discoverRepeat,
    ledgerPressure,
    ledgerProbe,
    ledgerRefute,
    normaliseSource,
    ledgerResolve,
    ledgerSeed,
    ledgerWorldChanged,
    missClusters,
) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Siza.Agent.Discover.Closure (worldNote)
import Siza.Agent.Discover.Facts (foldFacts, installFactKey)

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
    , slRefuted :: Set Text
    {- ^ Candidate sources the gate or compiler already rejected (G5.4): a
    nudge may never recommend one of these again.
    -}
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
    , slOrientation :: Map Text Text
    -- ^ Read-only request identity -> bounded result digest.
    , slRepeatRun :: Int
    -- ^ Consecutive exact-query or answer-hash duplicate discoveries.
    , slHardClosed :: Bool
    -- ^ Repeat threshold fired: every later discover is an act-only answer.
    }

emptyLedger :: SearchLedger
emptyLedger =
    SearchLedger
        { slSeen = Map.empty
        , slRefuted = Set.empty
        , slAnswers = Map.empty
        , slAsserted = Map.empty
        , slSeeded = Set.empty
        , slResolved = Set.empty
        , slMisses = Map.empty
        , slTried = Set.empty
        , slFacts = []
        , slEvidence = Map.empty
        , slConsulted = Set.empty
        , slWorldNote = Nothing
        , slRungFloor = 1
        , slGoalSat = Map.empty
        , slDeclaredPkgs = Set.empty
        , slClosed = False
        , slCalls = 0
        , slOrientation = Map.empty
        , slRepeatRun = 0
        , slHardClosed = False
        }

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

-- | Retire a candidate by evidence (G5.4): the compiler outranks the ledger.
ledgerRefute :: Text -> SearchLedger -> SearchLedger
ledgerRefute src led =
    led{slRefuted = Set.insert (normaliseSource src) (slRefuted led)}

-- | Whitespace-insensitive identity, so a re-indented repeat still matches.
normaliseSource :: Text -> Text
normaliseSource = T.unwords . T.words

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

{- | Fold harness-probed facts (G3: hole-probe conclusions) into the held
list, through the same bounded fold every harvested fact takes.
-}
ledgerProbe :: [Text] -> SearchLedger -> SearchLedger
ledgerProbe fs led = led{slFacts = foldFacts fs (slFacts led)}

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
        , slOrientation = Map.empty
        , slRepeatRun = 0
        , slHardClosed = False
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

{- | A notebook mutation invalidates cached orientation without disturbing
discovery evidence.
-}
ledgerInvalidateOrientation :: SearchLedger -> SearchLedger
ledgerInvalidateOrientation led = led{slOrientation = Map.empty}

{- | A repeat is answered entirely from the ledger; a first occurrence falls
through. The stored text is deliberately one physical line.
-}
orientationShortcut :: Text -> SearchLedger -> Maybe Value
orientationShortcut key led = String <$> Map.lookup key (slOrientation led)

orientationRecord :: Text -> Text -> Value -> SearchLedger -> SearchLedger
orientationRecord key tool payload led =
    led
        { slOrientation =
            Map.insert key (orientationSummary tool payload) (slOrientation led)
        }

orientationSummary :: Text -> Value -> Text
orientationSummary "list_cells" (Object o) =
    clip
        220
        ("same as your last list_cells; " <> tshow (length cs) <> " cells" <> previews)
  where
    cs = case KM.lookup "cells" o of
        Just (Array a) -> toList a
        _ -> []
    previews = T.concat (map preview (take 8 cs))
    preview (Object c) =
        ", cell "
            <> valueText "id" c
            <> ": "
            <> clip 48 (oneLine (valueText "source" c))
    preview _ = ""
orientationSummary "kernel_status" v =
    "same as your last kernel_status; " <> clip 120 (oneLine (statusText v))
orientationSummary tool _ = "same as your last " <> tool

statusText :: Value -> Text
statusText (Object o) =
    let s = valueText "state" o
     in if T.null s then valueText "status" o else s
statusText _ = "unchanged"

valueText :: Text -> KM.KeyMap Value -> Text
valueText k o = case KM.lookup (K.fromText k) o of
    Just (String s) -> s
    Just (Number n) -> T.pack (show (round n :: Int))
    _ -> "?"

oneLine :: Text -> Text
oneLine = T.unwords . T.words

clip :: Int -> Text -> Text
clip n t | T.length t <= n = t
clip n t = T.take (n - 1) t <> "…"

tshow :: Int -> Text
tshow = T.pack . show

discoverFresh :: SearchLedger -> SearchLedger
discoverFresh led = led{slRepeatRun = 0}

{- | Give repeated discovery teeth. The second consecutive duplicate closes
the channel hard; subsequent query variation cannot bypass it.
-}
discoverRepeat :: Text -> SearchLedger -> (SearchLedger, Maybe Value)
discoverRepeat q led = (led', if hard then Just (actOnly q) else Nothing)
  where
    n = slRepeatRun led + 1
    hard = slHardClosed led || n >= 2
    led' = led{slRepeatRun = n, slHardClosed = hard}

actOnly :: Text -> Value
actOnly q =
    object
        [ "query" .= q
        , "state" .= ("duplicate" :: Text)
        , "ref" .= ("discovery closed: repeat limit" :: Text)
        , "summary"
            .= ("Discovery is closed: act on what is held, or state the blocker." :: Text)
        ]
