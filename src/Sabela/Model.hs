{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Sabela.Model where

import Control.Concurrent (MVar)
import Control.Concurrent.STM (TChan)
import Data.Aeson (FromJSON, ToJSON (..), object, (.=))
import Data.IORef (IORef)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Sabela.Session (Session)

data OutputItem = OutputItem
    { oiMime :: Text
    , oiOutput :: Text
    }
    deriving (Show, Eq, Generic)

instance ToJSON OutputItem
instance FromJSON OutputItem

data AppState = AppState
    { stNotebook :: MVar Notebook
    , stSession :: MVar (Maybe Session)
    , stTmpDir :: FilePath
    , stWorkDir :: FilePath
    , stNextId :: IORef Int
    , stInstalledDeps :: IORef (Set Text)
    , stInstalledExts :: IORef (Set Text)
    , stBroadcast :: TChan NotebookEvent
    , stGeneration :: IORef Int
    , stDebounceRef :: MVar (Maybe (Int, Set Int))
    , stGlobalDeps :: Set Text
    , stWidgetValues :: MVar (Map Int (Map Text Text))
    -- ^ cellId → name → value; set by POST /api/widget
    }

data Notebook = Notebook
    { nbTitle :: Text
    , nbCells :: [Cell]
    }
    deriving (Show, Eq, Generic)

instance ToJSON Notebook
instance FromJSON Notebook

data Cell = Cell
    { cellId :: Int
    , cellType :: CellType
    , cellSource :: Text
    , cellOutputs :: [OutputItem]
    , cellError :: Maybe Text
    , cellDirty :: Bool
    }
    deriving (Show, Eq, Generic)

data CellType = CodeCell | ProseCell
    deriving (Show, Eq, Generic)

instance ToJSON Cell
instance FromJSON Cell
instance ToJSON CellType
instance FromJSON CellType

data NotebookEvent
    = EvCellUpdating Int
    | EvCellResult Int [OutputItem] (Maybe Text) [CellError]
    | EvExecutionDone
    | EvSessionStatus SessionStatus
    | EvInstallLog Text
    deriving (Show, Eq)

data SessionStatus
    = SReset
    | SUpdateDeps [Text]
    | SDepsUpToDate
    | SStarting
    | SReady
    deriving (Eq)

instance Show SessionStatus where
    show :: SessionStatus -> String
    show SReady = "ready"
    show SReset = "reset"
    show (SUpdateDeps deps) = T.unpack ("installing: " <> T.intercalate ", " deps)
    show SStarting = "starting session"
    show SDepsUpToDate = "dependencies up to date"

instance ToJSON NotebookEvent where
    toJSON (EvCellUpdating cid) =
        object ["type" .= ("cellUpdating" :: Text), "cellId" .= cid]
    toJSON (EvCellResult cid outputs err errs) =
        object
            [ "type" .= ("cellResult" :: Text)
            , "cellId" .= cid
            , "outputs" .= outputs
            , "error" .= err
            , "errors" .= errs
            ]
    toJSON EvExecutionDone =
        object ["type" .= ("executionDone" :: Text)]
    toJSON (EvSessionStatus msg) =
        object ["type" .= ("sessionStatus" :: Text), "message" .= show msg]
    toJSON (EvInstallLog line) =
        object ["type" .= ("installLog" :: Text), "line" .= line]

data CellError = CellError
    { ceLine :: Maybe Int
    -- ^ 1-based line within cell
    , ceCol :: Maybe Int
    , ceMessage :: Text
    }
    deriving (Show, Generic, Eq)

instance ToJSON CellError
instance FromJSON CellError
