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
    discoverFresh,
    discoverRepeat,
    ledgerPressure,
    ledgerProbe,
    ledgerRefute,
    ledgerRelease,
    normaliseSource,
    refutedBefore,
    ledgerResolve,
    ledgerSeed,
    ledgerWorldChanged,
    missClusters,
    takeEscalation,
) where

import Data.Aeson (Value (..), object, (.=))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Siza.Agent.Discover.Advice (clusterScope)
import Siza.Agent.Discover.Closure (
    entityOf,
    heldHitsFor,
    heldPayload,
    worldNote,
 )
import Siza.Agent.Discover.Facts (foldFacts, installFactKey)
import Siza.Agent.Discover.Types (StandingGoal)

data SearchLedger = SearchLedger
    { slSeen :: Map Text (Int, Text)
    , slAnswers :: Map Text (Int, Text)
    , slAsserted :: Map Text (Int, Text)
    , slSeeded :: Set Text
    , slRefuted :: Map Text Text
    , slResolved :: Set Text
    , slMisses :: Map Text Int
    , slTried :: Set Text
    , slFacts :: [Text]
    , slEvidence :: Map Text [Value]
    , slConsulted :: Set Text
    , slWorldNote :: Maybe Text
    , slRungFloor :: Int
    , slGoalSat :: Map Text Int
    , slGoalSpent :: Set Text
    , slEscalate :: Maybe StandingGoal
    , slDeclaredPkgs :: Set Text
    , slClosed :: Bool
    , slCalls :: Int
    , slRepeatRun :: Int
    , slHardClosed :: Bool
    }

emptyLedger :: SearchLedger
emptyLedger =
    SearchLedger
        { slSeen = Map.empty
        , slRefuted = Map.empty
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
        , slGoalSpent = Set.empty
        , slEscalate = Nothing
        , slDeclaredPkgs = Set.empty
        , slClosed = False
        , slCalls = 0
        , slRepeatRun = 0
        , slHardClosed = False
        }

heldEvidence :: SearchLedger -> Map Text [Value]
heldEvidence = slEvidence

heldFacts :: SearchLedger -> [Text]
heldFacts = slFacts

missClusters :: SearchLedger -> [Text]
missClusters led = map (T.takeWhile (/= '@')) (Map.keys (slMisses led))

callReadyFacts :: SearchLedger -> [Text]
callReadyFacts led = [f | f <- slFacts led, " :: " `T.isInfixOf` f]

ladderState :: SearchLedger -> ([(Text, Int)], Bool, Int, Int)
ladderState led =
    (Map.toAscList (slMisses led), slClosed led, slRungFloor led, slCalls led)

ledgerClose :: SearchLedger -> SearchLedger
ledgerClose led = led{slClosed = True}

ledgerResolve :: [Text] -> SearchLedger -> SearchLedger
ledgerResolve ns led =
    led{slResolved = Set.union (Set.fromList (map T.toLower ns)) (slResolved led)}

{- | Record that this source, up to whitespace, was refused, and with which
diagnostic: the write path reads it back so a resubmission says so.
-}
ledgerRefute :: Text -> Text -> SearchLedger -> SearchLedger
ledgerRefute src diag led =
    led{slRefuted = Map.insert (normaliseSource src) diag (slRefuted led)}

-- | The diagnostic an equivalent source was refused with, if it was.
refutedBefore :: SearchLedger -> Text -> Maybe Text
refutedBefore led src = Map.lookup (normaliseSource src) (slRefuted led)

normaliseSource :: Text -> Text
normaliseSource = T.unwords . T.words

ledgerPressure :: Int -> SearchLedger -> SearchLedger
ledgerPressure n led = led{slRungFloor = max 1 n}

ledgerSeed :: [Text] -> SearchLedger -> SearchLedger
ledgerSeed facts led =
    led{slSeeded = Set.union (Set.fromList (map T.toLower facts)) (slSeeded led)}

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
        , slGoalSpent = Set.empty
        , slEscalate = Nothing
        , slRepeatRun = 0
        , slHardClosed = False
        }
  where
    worldNoteWhenPriorAnswer
        | slCalls led > 0 = Just worldNote
        | otherwise = Nothing

ledgerDeclare :: [Text] -> SearchLedger -> SearchLedger
ledgerDeclare pkgs led =
    led{slDeclaredPkgs = Set.union (Set.fromList pkgs) (slDeclaredPkgs led)}

discoverFresh :: SearchLedger -> SearchLedger
discoverFresh led = led{slRepeatRun = 0}

{- | Hand the pending type escalation to whoever can spend it, once. The
ledger records the spend at the moment it is decided, so a caller that never
runs the query cannot re-arm it by asking again.
-}
takeEscalation :: SearchLedger -> (SearchLedger, Maybe StandingGoal)
takeEscalation led = (led{slEscalate = Nothing}, slEscalate led)

{- | Give a goal cluster its query back. A query that never ran bought no
answer, so holding the spend against the cluster would close it on nothing.
-}
ledgerRelease :: Text -> SearchLedger -> SearchLedger
ledgerRelease key led = led{slGoalSpent = Set.delete key (slGoalSpent led)}

discoverRepeat :: Text -> SearchLedger -> (SearchLedger, Maybe Value)
discoverRepeat q led = (led', if hard then Just (actOnly held q n) else Nothing)
  where
    n = slRepeatRun led + 1
    hard = slHardClosed led || n >= 2
    led' = led{slRepeatRun = n, slHardClosed = hard}
    held = heldHitsFor (slEvidence led) (entityOf q <> clusterScope q)

{- | The repeat limit reports what it did — no backend was consulted — and
returns the hits it is standing in for.
-}
actOnly :: [Value] -> Text -> Int -> Value
actOnly held q n =
    object $
        [ "query" .= q
        , "state" .= ("duplicate" :: Text)
        , "ref" .= ("discovery closed: repeat limit" :: Text)
        , "summary"
            .= ( "repeat "
                    <> T.pack (show n)
                    <> " of this question; no backend was consulted for this call"
               )
        ]
            <> heldPayload held
