{-# LANGUAGE OverloadedStrings #-}

{- | Suspension behind a failed upstream cell. A consumer of a cell that failed
must not run: the kernel still holds the producer's previous binding, so running
would quietly compute from stale data. It waits until the producer is fixed.
-}
module Sabela.Reactivity.Upstream (
    failedUpstream,
    settledFailures,
    upstreamFailureMsg,
    cycleBlockedMsg,
) where

import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Sabela.Model (Cell (..))

-- | The lowest-numbered failed cell this cell directly depends on.
failedUpstream :: M.Map Int (S.Set Int) -> S.Set Int -> Int -> Maybe Int
failedUpstream deps failed cid =
    S.lookupMin (M.findWithDefault S.empty cid deps `S.intersection` failed)

{- | Cells whose last run failed and that this plan will not re-run. They stay
failed for the whole run, so their consumers in the plan are suspended too.
-}
settledFailures :: [Cell] -> [Cell] -> S.Set Int
settledFailures allCode planned =
    S.fromList
        [ cellId c
        | c <- allCode
        , isJust (cellError c)
        , cellId c `S.notMember` plannedIds
        ]
  where
    plannedIds = S.fromList (map cellId planned)

upstreamFailureMsg :: M.Map Int Int -> Int -> Text
upstreamFailureMsg posMap upstream =
    "did not run: cell "
        <> T.pack (show (M.findWithDefault upstream upstream posMap))
        <> " failed, and this cell depends on it (fix that cell and this one re-runs)"

cycleBlockedMsg :: M.Map Int Int -> S.Set Int -> Text
cycleBlockedMsg posMap cycleIds =
    "did not run: this cell depends on a circular dependency between cells "
        <> T.intercalate
            ", "
            [T.pack (show (M.findWithDefault c c posMap)) | c <- S.toList cycleIds]
        <> " (break the cycle and this cell re-runs)"
