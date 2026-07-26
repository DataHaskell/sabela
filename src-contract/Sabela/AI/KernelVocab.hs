{-# LANGUAGE OverloadedStrings #-}

module Sabela.AI.KernelVocab (
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
    BusyEvidence (..),
    BusyVerdict (..),
    busyVerdict,
    resolveOccupied,
    busyRetryRounds,
    busyDenyJson,
    LockOwner (..),
    Holding (..),
    ownerLabel,
) where

import Data.Aeson (Value (..), (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T

import Sabela.Api (errorJsonWith)

tagCold, tagIdle, tagBuilding, tagExecuting :: Text
tagCold = "cold"
tagIdle = "idle"
tagBuilding = "building"
tagExecuting = "executing"

tagSettled, tagKernelDead, tagTimedOut :: Text
tagSettled = "settled"
tagKernelDead = "kernelDead"
tagTimedOut = "timedOut"

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

vocabularyLine :: Text
vocabularyLine =
    "Kernel states: cold|idle|building|executing. await_idle waited: \
    \idle|settled|timedOut|kernelDead. Write status: queued|executing|completed."

data LockOwner = OwnedByCell !Int | OwnedByOp !Text
    deriving (Eq, Show)

data Holding = Holding
    { hdOwner :: !LockOwner
    , hdElapsedMs :: !Int
    }
    deriving (Eq, Show)

ownerLabel :: LockOwner -> Text
ownerLabel (OwnedByCell cid) = "cell " <> tshow cid
ownerLabel (OwnedByOp op) = op

data BusyEvidence = BusyEvidence
    { beOccupied :: !Bool
    , beSettledGen :: !(Maybe Int)
    , beCurrentGen :: !Int
    , beHolder :: !(Maybe Holding)
    }
    deriving (Eq, Show)

data BusyVerdict = AdmitNow | RetrySoon | DenyBusy (Maybe Holding)
    deriving (Eq, Show)

busyVerdict :: BusyEvidence -> BusyVerdict
busyVerdict e
    | not (beOccupied e) = AdmitNow
    | beSettledGen e == Just (beCurrentGen e) = RetrySoon
    | otherwise = DenyBusy (beHolder e)

busyRetryRounds :: Int
busyRetryRounds = 20

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

busyDenyJson :: Maybe Holding -> Value
busyDenyJson holder =
    errorJsonWith msg (["busy" .= True, "cause" .= ("other-run" :: Text)] <> ids)
  where
    ids = case holder of
        Just (Holding o ms) -> ownerField o <> ["elapsedMs" .= ms]
        Nothing -> []
    ownerField (OwnedByCell cid) = ["cellId" .= cid]
    ownerField (OwnedByOp op) = ["operation" .= op]
    msg = case holder of
        Just (Holding o ms) ->
            "The kernel is busy: "
                <> ownerLabel o
                <> " has been running for "
                <> tshow ms
                <> "ms (not your in-flight write). Call await_idle; \
                   \interrupt only if it never settles."
        Nothing ->
            "The kernel is busy with a run or compile you did not start. \
            \Call await_idle to block until it finishes, then continue."

tshow :: (Show a) => a -> Text
tshow = T.pack . show
