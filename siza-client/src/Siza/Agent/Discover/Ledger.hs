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
    normaliseSource,
    ledgerResolve,
    ledgerSeed,
    ledgerWorldChanged,
    missClusters,
) where

import Data.Aeson (Value (..), object, (.=))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Siza.Agent.Discover.Closure (worldNote)
import Siza.Agent.Discover.Facts (foldFacts, installFactKey)

data SearchLedger = SearchLedger
    { slSeen :: Map Text (Int, Text)
    , slAnswers :: Map Text (Int, Text)
    , slAsserted :: Map Text (Int, Text)
    , slSeeded :: Set Text
    , slRefuted :: Set Text
    , slResolved :: Set Text
    , slMisses :: Map Text Int
    , slTried :: Set Text
    , slFacts :: [Text]
    , slEvidence :: Map Text Value
    , slConsulted :: Set Text
    , slWorldNote :: Maybe Text
    , slRungFloor :: Int
    , slGoalSat :: Map Text Int
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
        , slRepeatRun = 0
        , slHardClosed = False
        }

heldEvidence :: SearchLedger -> Map Text Value
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

ledgerRefute :: Text -> SearchLedger -> SearchLedger
ledgerRefute src led =
    led{slRefuted = Set.insert (normaliseSource src) (slRefuted led)}

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
