{-# LANGUAGE DeriveGeneric #-}

module Sabela.SessionTypes where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Unique (Unique)
import GHC.Generics (Generic)

data SessionBackend = SessionBackend
    { sbSessionId :: Unique
    -- ^ Identity of this backend instance, for swap-iff-same crash handling.
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
    }

data CellLang = Haskell | Python
    deriving (Show, Eq, Ord, Generic)

instance ToJSON CellLang
instance FromJSON CellLang
