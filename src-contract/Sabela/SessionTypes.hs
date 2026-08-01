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
    , sbJsonDiagnostics :: Bool
    , sbRunBlock :: Text -> IO (Text, Text)
    , sbRunBlockStreaming :: Text -> (Text -> IO ()) -> IO (Text, Text)
    , sbClose :: IO ()
    , sbReset :: IO SessionBackend
    , sbInterrupt :: IO ()
    , sbBusy :: IO Bool
    , sbSessionGen :: IO Int
    , sbRequestStale :: UTCTime -> IO Bool
    , sbQueryComplete :: Text -> IO [Text]
    , sbQueryType :: Text -> IO Text
    , sbQueryInfo :: Text -> IO Text
    , sbQueryKind :: Text -> IO Text
    , sbQueryBrowse :: Text -> IO Text
    , sbQueryDoc :: Text -> IO Text
    , sbQueryHoleFits :: Text -> IO Text
    , sbQueryBindings :: IO Text
    , sbEvalPureLive :: PureEvalRequest -> IO PureEvalResult
    }

data PureEvalRequest = PureEvalRequest
    { pureEvalExpectedGeneration :: Int
    , pureEvalTimeoutUs :: Int
    , pureEvalExpression :: Text
    }
    deriving (Eq, Generic, Show)

instance ToJSON PureEvalRequest
instance FromJSON PureEvalRequest

data PureEvalVerdict
    = PureEvalSucceeded
    | PureEvalUnshowable
    | PureEvalRejected
    | PureEvalRuntimeError
    | PureEvalTimedOut
    | PureEvalStale
    | PureEvalInvariantFailed
    | PureEvalUnavailable
    deriving (Eq, Generic, Show)

instance ToJSON PureEvalVerdict
instance FromJSON PureEvalVerdict

data PureEvalRecovery
    = PureEvalNoRecovery
    | PureEvalInterrupted
    | PureEvalKernelDestroyed
    deriving (Eq, Generic, Show)

instance ToJSON PureEvalRecovery
instance FromJSON PureEvalRecovery

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
