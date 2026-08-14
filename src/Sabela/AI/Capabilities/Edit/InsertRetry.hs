{- | Whether a notebook that turned red between the route's read and the
atomic append earns another pass. Bounded: the two can keep flipping, and a
retry costs a full disposable build.
-}
module Sabela.AI.Capabilities.Edit.InsertRetry (
    InsertAttempt (..),
    insertRetryFuel,
    nextInsertAttempt,
) where

import Sabela.Handlers (NotebookViolation (..))

data InsertAttempt = RetryInsert Int | AbandonInsert NotebookViolation

nextInsertAttempt :: Int -> NotebookViolation -> InsertAttempt
nextInsertAttempt fuel v = case v of
    VPendingError _ _ | fuel > 0 -> RetryInsert (fuel - 1)
    _ -> AbandonInsert v

insertRetryFuel :: Int
insertRetryFuel = 2
