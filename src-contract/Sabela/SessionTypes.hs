{-# LANGUAGE DeriveGeneric #-}

module Sabela.SessionTypes where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Unique (Unique)
import GHC.Generics (Generic)

data SessionBackend = SessionBackend
    { sbSessionId :: Unique
    -- ^ Identity of this backend instance, for swap-iff-same crash handling.
    , sbJsonDiagnostics :: Bool
    {- ^ Does this backend's compiler emit @-fdiagnostics-as-json@ (GHC ≥ 9.8)?
    Selects the structured JSON diagnostic path over the textual fallback.
    -}
    , sbRunBlock :: Text -> IO (Text, Text)
    -- ^ Execute a block of code, returning (stdout, stderr).
    , sbRunBlockStreaming :: Text -> (Text -> IO ()) -> IO (Text, Text)
    -- ^ Like sbRunBlock, but calls the callback with each output line as it arrives.
    , sbClose :: IO ()
    -- ^ Shut down the backend process and reclaim its whole tree.
    , sbReset :: IO SessionBackend
    -- ^ Kill and restart the backend, returning a fresh handle.
    , sbInterrupt :: IO ()
    -- ^ Abort the running cell (group SIGINT); no-op when idle.
    , sbBusy :: IO Bool
    {- ^ Lock-free: is a cell or query currently running? Answers even
    while the run-lock is held, so a driver can tell busy from wedged.
    -}
    , sbSessionGen :: IO Int
    -- ^ Generation tag of the live backend; changes on restart/reset.
    , sbRequestStale :: UTCTime -> IO Bool
    {- ^ Was a request stamped at this instant stale — issued before the
    kernel's last interrupt? Pure 'False' for backends without the filter.
    -}
    , sbQueryComplete :: Text -> IO [Text]
    -- ^ Completion query (GHCi :complete).
    , sbQueryType :: Text -> IO Text
    -- ^ Type query (GHCi :type).
    , sbQueryInfo :: Text -> IO Text
    -- ^ Info query (GHCi :info).
    , sbQueryKind :: Text -> IO Text
    -- ^ Kind query (GHCi :kind).
    , sbQueryBrowse :: Text -> IO Text
    -- ^ List a module's exports (GHCi :browse).
    , sbQueryDoc :: Text -> IO Text
    -- ^ Doc query (GHCi :doc).
    , sbQueryHoleFits :: Text -> IO Text
    {- ^ Valid-hole-fits query: set the hole-fit flags, then ask GHC which
    in-scope names fit a concrete goal type (e.g. @_ :: [Int] -> Int@), so the
    model picks a real name instead of inventing one. Runs on the introspection
    lock like the other queries.
    -}
    , sbQueryBindings :: IO Text
    -- ^ The session's interactive bindings and their types (GHCi @:show bindings@).
    , sbEvalPureLive :: PureEvalRequest -> IO PureEvalResult
    {- ^ Atomically admit and evaluate one compiler-proven non-@IO@ expression
    against this backend, pinned to one locked session generation;
    non-Haskell backends report 'PureEvalUnavailable'.
    -}
    }

-- | One generation-pinned request for the internal pure-live fast path.
data PureEvalRequest = PureEvalRequest
    { pureEvalExpectedGeneration :: Int
    , pureEvalTimeoutUs :: Int
    , pureEvalExpression :: Text
    }
    deriving (Eq, Generic, Show)

instance ToJSON PureEvalRequest
instance FromJSON PureEvalRequest

-- | Compiler/runtime verdict for a pure-live request.
data PureEvalVerdict
    = PureEvalSucceeded
    | PureEvalRejected
    | PureEvalRuntimeError
    | PureEvalTimedOut
    | PureEvalStale
    | PureEvalInvariantFailed
    | PureEvalUnavailable
    deriving (Eq, Generic, Show)

instance ToJSON PureEvalVerdict
instance FromJSON PureEvalVerdict

-- | Recovery work performed while bounding an evaluation.
data PureEvalRecovery
    = PureEvalNoRecovery
    | PureEvalInterrupted
    | PureEvalKernelDestroyed
    deriving (Eq, Generic, Show)

instance ToJSON PureEvalRecovery
instance FromJSON PureEvalRecovery

{- | Complete evidence returned by the atomic operation. 'pureEvalOutput' is
already capped inside GHCi, before it reaches the session output accumulator.
-}
data PureEvalResult = PureEvalResult
    { pureEvalVerdict :: PureEvalVerdict
    , pureEvalGeneration :: Int
    , pureEvalInferredType :: Text
    , pureEvalOutput :: Text
    , pureEvalError :: Text
    , pureEvalBindingsUnchanged :: Bool
    , pureEvalItUnchanged :: Bool
    , pureEvalRecovery :: PureEvalRecovery
    }
    deriving (Eq, Generic, Show)

instance ToJSON PureEvalResult
instance FromJSON PureEvalResult

pureEvalUnavailableResult :: PureEvalRequest -> Text -> PureEvalResult
pureEvalUnavailableResult req msg =
    PureEvalResult
        { pureEvalVerdict = PureEvalUnavailable
        , pureEvalGeneration = pureEvalExpectedGeneration req
        , pureEvalInferredType = T.empty
        , pureEvalOutput = T.empty
        , pureEvalError = msg
        , pureEvalBindingsUnchanged = True
        , pureEvalItUnchanged = True
        , pureEvalRecovery = PureEvalNoRecovery
        }

data CellLang = Haskell | Python
    deriving (Eq, Generic, Ord, Show)

instance ToJSON CellLang
instance FromJSON CellLang
