{-# LANGUAGE DeriveGeneric #-}

module Sabela.Api where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Sabela.Model (CellType)

-- Request bodies.

newtype UpdateCell = UpdateCell
    {ucSource :: Text}
    deriving (Show, Eq, Generic)

instance ToJSON UpdateCell
instance FromJSON UpdateCell

data InsertCell = InsertCell
    { icAfter :: Int
    , icType :: CellType
    , icSource :: Text
    }
    deriving (Show, Eq, Generic)

instance ToJSON InsertCell
instance FromJSON InsertCell

newtype LoadRequest = LoadRequest
    { lrPath :: FilePath
    }
    deriving (Show, Generic, Eq)

instance ToJSON LoadRequest
instance FromJSON LoadRequest

newtype SaveRequest = SaveRequest
    { srPath :: Maybe FilePath
    -- ^ Nothing = save to nbTitle
    }
    deriving (Show, Generic, Eq)

instance ToJSON SaveRequest
instance FromJSON SaveRequest

data CreateFileRequest = CreateFileRequest
    { cfPath :: Text
    -- ^ relative path
    , cfContent :: Text
    , cfIsDir :: Bool
    }
    deriving (Show, Generic, Eq)

instance ToJSON CreateFileRequest
instance FromJSON CreateFileRequest

data WriteFileRequest = WriteFileRequest
    { wfPath :: Text
    , wfContent :: Text
    }
    deriving (Show, Generic, Eq)

instance ToJSON WriteFileRequest
instance FromJSON WriteFileRequest

newtype CompleteRequest = CompleteRequest
    { crPrefix :: Text
    }
    deriving (Show, Generic, Eq)

instance ToJSON CompleteRequest
instance FromJSON CompleteRequest

newtype InfoRequest = InfoRequest
    { irName :: Text
    }
    deriving (Show, Generic, Eq)

instance ToJSON InfoRequest
instance FromJSON InfoRequest

-- Response bodies.

data RunResult = RunResult
    { rrCellId :: Int
    , rrOutput :: Maybe Text
    , rrError :: Maybe Text
    , rrMime :: Text
    }
    deriving (Show, Generic)

instance ToJSON RunResult
instance FromJSON RunResult

newtype RunAllResult = RunAllResult
    { rarResults :: [RunResult]
    }
    deriving (Show, Generic)

instance ToJSON RunAllResult
instance FromJSON RunAllResult

newtype CompleteResult = CompleteResult
    { crCompletions :: [Text]
    }
    deriving (Show, Generic)

instance ToJSON CompleteResult
instance FromJSON CompleteResult

newtype InfoResult = InfoResult
    { irText :: Text
    }
    deriving (Show, Generic)

instance ToJSON InfoResult
instance FromJSON InfoResult

data FileEntry = FileEntry
    { feName :: Text
    , fePath :: Text
    , feIsDir :: Bool
    }
    deriving (Show, Generic)

instance ToJSON FileEntry
instance FromJSON FileEntry

data Example = Example
    { exTitle :: Text
    , exDesc :: Text
    , exCategory :: Text
    , exCode :: Text
    }
    deriving (Show, Generic)

instance ToJSON Example
instance FromJSON Example
