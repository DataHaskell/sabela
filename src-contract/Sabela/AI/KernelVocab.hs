{-# LANGUAGE OverloadedStrings #-}

{- | The closed kernel-state vocabulary and the post-settled consistency
window (R1.7/R3.6/R6.4). Every state string any kernel\/await\/ack surface
emits is a member of 'stateVocabulary'; producers import the constants, the
one-shape validator ('outOfVocabStates') rejects anything else, and tool
descriptions advertise exactly 'vocabularyLine' — so advertised and actual
behaviour cannot drift.
-}
module Sabela.AI.KernelVocab (
    -- * Closed vocabulary
    stateVocabulary,
    inVocabulary,
    outOfVocabStates,
    vocabularyLine,
    tagCold,
    tagIdle,
    tagBuilding,
    tagExecuting,
    tagSettled,
    tagKernelDead,
    tagTimedOut,

    -- * Post-settled consistency window (R6.4)
    BusyEvidence (..),
    BusyVerdict (..),
    busyVerdict,
    resolveOccupied,
    busyRetryRounds,
    busyDenyJson,
) where

import Data.Aeson (Value (..), (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.Api (errorJsonWith)

-- Individual members, imported by producers so the enum is the single source.
tagCold, tagIdle, tagBuilding, tagExecuting :: Text
tagCold = "cold"
tagIdle = "idle"
tagBuilding = "building"
tagExecuting = "executing"

tagSettled, tagKernelDead, tagTimedOut :: Text
tagSettled = "settled"
tagKernelDead = "kernelDead"
tagTimedOut = "timedOut"

{- | The whole closed enum: kernel states, await tags, write-ack statuses.
@queued@\/@completed@ belong to "Sabela.AI.WriteAck"; they are listed here so
ONE membership check covers every state-bearing field.
-}
stateVocabulary :: [Text]
stateVocabulary =
    [ tagCold
    , tagIdle
    , tagBuilding
    , tagExecuting
    , tagSettled
    , tagKernelDead
    , tagTimedOut
    , "queued"
    , "completed"
    ]

inVocabulary :: Text -> Bool
inVocabulary = (`elem` stateVocabulary)

{- | The one-shape validator: every string under a @state@\/@waited@\/@status@
key, at any depth, must be a vocabulary member. Returns the violations, so a
clean payload answers @[]@.
-}
outOfVocabStates :: Value -> [Text]
outOfVocabStates = go
  where
    stateKeys = ["state", "waited", "status"]
    go (Object o) =
        [ s
        | k <- stateKeys
        , Just (String s) <- [KM.lookup k o]
        , not (inVocabulary s)
        ]
            <> concatMap go (KM.elems o)
    go (Array a) = concatMap go (toList a)
    go _ = []

-- | The advertised enum, embedded in tool descriptions (R1.7).
vocabularyLine :: Text
vocabularyLine =
    "Kernel states: cold|idle|building|executing. await_idle waited: \
    \idle|settled|timedOut|kernelDead. Write status: queued|executing|completed."

{- | One lock-free busy observation plus the settle fence it is judged
against: the generation last observed settled, the generation now, and the
locking holder (cell id, elapsed ms) when a registry knows it.
-}
data BusyEvidence = BusyEvidence
    { beOccupied :: !Bool
    , beSettledGen :: !(Maybe Int)
    , beCurrentGen :: !Int
    , beHolder :: !(Maybe (Int, Int))
    }
    deriving (Eq, Show)

data BusyVerdict = AdmitNow | RetrySoon | DenyBusy (Maybe (Int, Int))
    deriving (Eq, Show)

{- | The window law: occupancy while the generation has not advanced past the
settled one is the settling run's release tail — retry, never deny. A busy
denial is representable only when no settle was observed or a new run
advanced the generation.
-}
busyVerdict :: BusyEvidence -> BusyVerdict
busyVerdict e
    | not (beOccupied e) = AdmitNow
    | beSettledGen e == Just (beCurrentGen e) = RetrySoon
    | otherwise = DenyBusy (beHolder e)

-- | Grace samples for the release tail (with a ~100ms delay each: ~2s).
busyRetryRounds :: Int
busyRetryRounds = 20

{- | Drive 'busyVerdict' to a terminal verdict: re-sample through the release
tail, admitting the moment occupancy drops; deny only on a genuine busy or a
tail that outlives the whole grace window. Never returns 'RetrySoon'.
-}
resolveOccupied :: Int -> IO () -> IO BusyEvidence -> IO BusyVerdict
resolveOccupied rounds delay sample = go rounds
  where
    go n = do
        e <- sample
        case busyVerdict e of
            RetrySoon
                | n > 0 -> delay >> go (n - 1)
                | otherwise -> pure (DenyBusy (beHolder e))
            v -> pure v

{- | The genuine-busy denial (R6.4): names the locking cell and elapsed time
when known, labels the cause so it can never be mistaken for the own-write
bounce, and steers to await_idle, never to a retry.
-}
busyDenyJson :: Maybe (Int, Int) -> Value
busyDenyJson holder =
    errorJsonWith msg (["busy" .= True, "cause" .= ("other-run" :: Text)] <> ids)
  where
    ids = case holder of
        Just (cid, ms) -> ["cellId" .= cid, "elapsedMs" .= ms]
        Nothing -> []
    msg = case holder of
        Just (cid, ms) ->
            "The kernel is busy: cell "
                <> tshow cid
                <> " has been executing for "
                <> tshow ms
                <> "ms (not your in-flight write). Call await_idle; \
                   \interrupt only if it never settles."
        Nothing ->
            "The kernel is busy with a run or compile you did not start. \
            \Call await_idle to block until it finishes, then continue."

tshow :: (Show a) => a -> Text
tshow = T.pack . show
