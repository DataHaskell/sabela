{-# LANGUAGE DeriveGeneric #-}

module Sabela.SessionTypes where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data SessionBackend = SessionBackend
    { sbRunBlock :: Text -> IO (Text, Text)
    -- ^ Execute a block of code, returning (stdout, stderr).
    , sbRunBlockStreaming :: Text -> (Text -> IO ()) -> IO (Text, Text)
    -- ^ Like sbRunBlock, but calls the callback with each output line as it arrives.
    , sbClose :: IO ()
    -- ^ Shut down the backend process.
    , sbReset :: IO SessionBackend
    -- ^ Kill and restart the backend, returning a fresh handle.
    , sbQueryComplete :: Text -> IO [Text]
    -- ^ Completion query (GHCi :complete).
    , sbQueryType :: Text -> IO Text
    -- ^ Type query (GHCi :type).
    , sbQueryInfo :: Text -> IO Text
    -- ^ Info query (GHCi :info).
    , sbQueryDoc :: Text -> IO Text
    -- ^ Doc query (GHCi :doc).
    }

data CellLang = Haskell | Python
    deriving (Show, Eq, Ord, Generic)

instance ToJSON CellLang
instance FromJSON CellLang
